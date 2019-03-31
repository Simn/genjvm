open Globals
open Ast
open Common
open Type
open Path
open JvmGlobals
open MethodAccessFlags
open JvmData
open JvmAttribute
open JvmSignature
open JvmMethod
open JvmBuilder

(* Haxe *)

exception HarderFailure of string

type generation_context = {
	com : Common.context;
	jar : Zip.out_file;
	t_exception : Type.t;
	t_throwable : Type.t;
	anon_lut : ((string * jsignature) list,jpath) Hashtbl.t;
	mutable anon_num : int;
}

type ret =
	| RValue of jsignature option
	| RVoid
	| RReturn

type method_type =
	| MStatic
	| MInstance
	| MConstructor
	| MConstructorTop

type access_kind =
	| AKPost
	| AKPre
	| AKNone

module NativeArray = struct
	let read code ja je = match je with
		| TBool -> code#baload TBool ja
		| TByte -> code#baload TByte ja
		| TChar -> code#caload ja
		| TDouble -> code#daload ja
		| TFloat -> code#faload ja
		| TInt -> code#iaload ja
		| TLong -> code#laload ja
		| TShort -> code#saload ja
		| _ -> code#aaload ja je

	let write code ja je = match je with
		| TBool -> code#bastore TBool ja
		| TByte -> code#bastore TByte ja
		| TChar -> code#castore ja
		| TDouble -> code#dastore ja
		| TFloat -> code#fastore ja
		| TInt -> code#iastore ja
		| TLong -> code#lastore ja
		| TShort -> code#sastore ja
		| _ -> code#aastore ja je

	let create code pool je =
		let ja = (TArray(je,None)) in
		let primitive i =
			code#newarray ja i
		in
		let reference path =
			let offset = pool#add_path path in
			code#anewarray ja offset;
		in
		begin match je with
		| TBool -> primitive 4
		| TChar -> primitive 5
		| TFloat -> primitive 6
		| TDouble -> primitive 7
		| TByte -> primitive 8
		| TShort -> primitive 9
		| TInt -> primitive 10
		| TLong -> primitive 11
		| TObject(path,_) -> reference path
		| TMethod _ -> reference NativeSignatures.method_handle_path
		| TTypeParameter _ -> reference NativeSignatures.object_path
		| TArray _ -> assert false (* TODO: hmm... *)
		| TObjectInner _ | TUninitialized _ -> assert false
		end;
		ja
end

open NativeSignatures

let rec jsignature_of_type t = match t with
	| TAbstract(a,tl) ->
		begin match a.a_path with
			| [],"Bool" -> TBool
			| ["java"],"Int8" -> TByte
			| ["java"],"Int16" -> TShort
			| [],"Int" -> TInt
			| ["haxe"],"Int32" -> TInt
			| ["haxe"],"Int64" -> TLong
			| ["java"],"Int64" -> TLong
			| ["java"],"Char16" -> TChar
			| [],"Single" -> TFloat
			| [],"Float" -> TDouble
			| [],"Null" ->
				begin match tl with
				| [t] -> get_boxed_type (jsignature_of_type t)
				| _ -> assert false
				end
			| ["haxe";"ds"],"Vector" ->
				begin match tl with
				| [t] -> TArray(jsignature_of_type t,None)
				| _ -> assert false
				end
			| [],"Dynamic" ->
				object_sig
			| _ ->
				if Meta.has Meta.CoreType a.a_meta then
					TObject(a.a_path,List.map jtype_argument_of_type tl)
				else
					jsignature_of_type (Abstract.get_underlying_type a tl)
		end
	| TDynamic t' when t' == t_dynamic -> object_sig
	| TDynamic _ -> object_sig (* TODO: hmm... *)
	| TMono r ->
		begin match !r with
		| Some t -> jsignature_of_type t
		| None -> object_sig
		end
	| TInst({cl_path = ([],"String")},[]) -> string_sig
	| TInst({cl_path = ([],"Array")},[t]) ->
		let t = get_boxed_type (jsignature_of_type t) in
		TObject(([],"Array"),[TType(WNone,t)])
	| TInst({cl_path = (["java"],"NativeArray")},[t]) ->
		TArray(jsignature_of_type t,None)
	| TInst({cl_kind = KTypeParameter _; cl_path = (_,name)},_) -> TTypeParameter name
	| TInst(c,tl) -> TObject(c.cl_path,List.map jtype_argument_of_type tl)
	| TEnum(en,tl) -> TObject(en.e_path,List.map jtype_argument_of_type tl)
	| TFun(tl,tr) -> method_sig (List.map (fun (_,_,t) -> jsignature_of_type t) tl) (if ExtType.is_void (follow tr) then None else Some (jsignature_of_type tr))
	| TAnon an -> object_sig
	| TType(td,tl) -> jsignature_of_type (apply_params td.t_params tl td.t_type)
	| TLazy f -> jsignature_of_type (lazy_type f)

and jtype_argument_of_type t =
	TType(WNone,jsignature_of_type t)

module TAnonIdentifiaction = struct
	let identify gctx fields =
		let l = PMap.fold (fun cf acc -> cf :: acc) fields [] in
		let l = List.sort (fun cf1 cf2 -> compare cf1.cf_name cf2.cf_name) l in
		let l = List.map (fun cf -> cf.cf_name,jsignature_of_type cf.cf_type) l in
		try
			Hashtbl.find gctx.anon_lut l,l
		with Not_found ->
			let id = gctx.anon_num in
			gctx.anon_num <- gctx.anon_num + 1;
			let path = (["haxe";"generated"],Printf.sprintf "Anon%i" id) in
			Hashtbl.add gctx.anon_lut l path;
			path,l
end

let enum_ctor_sig =
	let ta = TArray(object_sig,None) in
	method_sig [TInt;ta] None

let convert_cmp_op = function
	| OpEq -> CmpEq
	| OpNotEq -> CmpNe
	| OpLt -> CmpLt
	| OpLte -> CmpLe
	| OpGt -> CmpGt
	| OpGte -> CmpGe
	| _ -> assert false

let flip_cmp_op = function
	| CmpEq -> CmpNe
	| CmpNe -> CmpEq
	| CmpLt -> CmpGe
	| CmpLe -> CmpGt
	| CmpGt -> CmpLe
	| CmpGe -> CmpLt

let resolve_class com path =
	let rec loop types = match types with
		| (TClassDecl c) :: types when c.cl_path = path ->
			c
		| _ :: types ->
			loop types
		| [] ->
			jerror ("No such type: " ^ s_type_path path)
	in
	loop com.types

let resolve_field com static path name =
	let c = resolve_class com path in
	try
		c,PMap.find name (if static then c.cl_statics else c.cl_fields)
	with Not_found ->
		jerror (Printf.sprintf "No such field: %s.%s" (s_type_path path) name)

let add_field pool c cf =
	let field_kind = match cf.cf_kind with
		| Method (MethNormal | MethInline) ->
			if c.cl_interface then FKInterfaceMethod else FKMethod
		| _ ->
			if c.cl_interface then jerror (Printf.sprintf "Interface field access is not supported on JVM (for %s.%s)" (s_type_path c.cl_path) cf.cf_name);
			FKField
	in
	let t = jsignature_of_type cf.cf_type in
	pool#add_field (path_map c.cl_path) (if cf.cf_name = "new" then "<init>" else cf.cf_name) t field_kind

let write_class jar path jc =
	let dir = match path with
		| ([],s) -> s
		| (sl,s) -> String.concat "/" sl ^ "/" ^ s
	in
	let path = dir ^ ".class" in
	let t = Timer.timer ["jvm";"write"] in
	let ch = IO.output_bytes() in
	JvmWriter.write_jvm_class ch jc;
	Zip.add_entry (Bytes.unsafe_to_string (IO.close_out ch)) jar path;
	t()

let is_const_int_pattern (el,_) =
	List.for_all (fun e -> match e.eexpr with
		| TConst (TInt _) -> true
		| _ -> false
	) el

let is_interface_var_access c cf =
	c.cl_interface && match cf.cf_kind with
		| Var _ | Method MethDynamic -> true
		| _ -> false

let type_unifies a b =
	try Type.unify a b; true with _ -> false

class haxe_exception gctx (t : Type.t) = object(self)
	val native_exception =
		if follow t == t_dynamic then
			throwable_sig,false
		else if type_unifies t gctx.t_exception then
			jsignature_of_type t,true
		else
			haxe_exception_sig,false

	val mutable native_exception_path = None

	method is_assignable_to (exc2 : haxe_exception) =
		match self#is_native_exception,exc2#is_native_exception with
		| true, true ->
			(* Native exceptions are assignable if they unify *)
			type_unifies t exc2#get_type
		| false,false ->
			(* Haxe exceptions are always assignable to each other *)
			true
		| false,true ->
			(* Haxe exception is assignable to native only if caught type is java.lang.Exception/Throwable *)
			let exc2_native_exception_type = exc2#get_native_exception_type in
			exc2_native_exception_type = throwable_sig || exc2_native_exception_type = exception_sig
		| _ ->
			(* Native to Haxe is never assignable *)
			false

	method is_native_exception = snd native_exception
	method get_native_exception_type = fst native_exception

	method get_native_exception_path =
		match native_exception_path with
		| None ->
			let path = (match (fst native_exception) with TObject(path,_) -> path | _ -> assert false) in
			native_exception_path <- Some path;
			path
		| Some path ->
			path

	method get_type = t
end

class closure_context (jsig : jsignature) = object(self)
	val lut = Hashtbl.create 0
	val sigs = DynArray.create()

	method add (var_id : int) (var_name : string) (var_sig : jsignature) =
		DynArray.add sigs var_sig;
		Hashtbl.add lut var_id (var_sig,var_name)

	method get (code : JvmCode.builder) (var_id : int) =
		let var_sig,var_name = Hashtbl.find lut var_id in
		if DynArray.length sigs > 1 then begin
			(-1),
			(fun () ->
				code#aload jsig 0;
				let offset = code#get_pool#add_field self#get_path var_name var_sig FKField in
				code#getfield offset jsig var_sig
			),
			(fun () ->
				code#aload jsig 0;
				let offset = code#get_pool#add_field self#get_path var_name var_sig FKField in
				code#putfield offset jsig var_sig
			)
		end else begin
			(-1),
			(fun () ->
				code#aload jsig 0;
			),
			(fun () ->
				code#aload jsig 0;
			)
		end

	method get_constructor_sig =
		method_sig (DynArray.to_list sigs) None

	method get_jsig = jsig
	method get_path = match jsig with TObject(path,_) -> path | _ -> assert false
end

let create_context_class gctx jc jm name vl = match vl with
	| [v] ->
		let jsig = get_boxed_type (jsignature_of_type v.v_type) in
		let ctx_class = new closure_context jsig in
		ctx_class#add v.v_id v.v_name jsig;
		ctx_class
	| _ ->
		let jc = jc#spawn_inner_class (Some jm) object_path None in
		let path = jc#get_this_path in
		let ctx_class = new closure_context (object_path_sig path) in
		let jsigs = List.map (fun v -> jsignature_of_type v.v_type) vl in
		let jm_ctor = jc#spawn_method "<init>" (method_sig jsigs None) [MPublic] in
		jm_ctor#load_this;
		jm_ctor#call_super_ctor (method_sig [] None);
		List.iter2 (fun v jsig ->
			jm_ctor#add_argument_and_field v.v_name jsig;
			ctx_class#add v.v_id v.v_name jsig;
		) vl jsigs;
		jm_ctor#get_code#return_void;
		write_class gctx.jar path jc#export_class;
		ctx_class

let rvalue_any = RValue None

class texpr_to_jvm gctx (jc : JvmClass.builder) (jm : JvmMethod.builder) (return_type : Type.t) = object(self)
	val com = gctx.com
	val code = jm#get_code
	val pool : JvmConstantPool.constant_pool = jc#get_pool

	val mutable local_lookup = Hashtbl.create 0;
	val mutable last_line = 0

	val mutable breaks = []
	val mutable continue = 0
	val mutable caught_exceptions = []
	val mutable env = None

	method vtype t =
		jsignature_of_type t

	method mknull t = com.basic.tnull (follow t)

	(* locals *)

	method add_named_local (name : string) (jsig : jsignature) =
		jm#add_local name jsig VarArgument

	method add_local v init_state : (int * (unit -> unit) * (unit -> unit)) =
		let t = self#vtype v.v_type in
		let slot,load,store = jm#add_local v.v_name t init_state in
		Hashtbl.add local_lookup v.v_id (slot,load,store);
		slot,load,store

	method get_local v =
		try
			Hashtbl.find local_lookup v.v_id
		with Not_found -> try
			begin match env with
			| Some env ->
				env#get code v.v_id
			| None ->
				raise Not_found
			end
		with Not_found ->
			failwith ("Unbound local: " ^ v.v_name)


	method set_context (ctx : closure_context) =
		env <- Some ctx

	(* casting *)

	method expect_reference_type = jm#expect_reference_type

	method cast t =
		if follow t != t_dynamic then begin
			let vt = self#vtype t in
			jm#cast vt
		end else
			self#expect_reference_type

	method tfunction e tf =
		let name = jc#get_next_closure_name in
		let outside = match Texpr.collect_captured_vars e with
			| [] ->
				None
			| vl ->
				let ctx_class = create_context_class gctx jc jm name vl in
				Some (ctx_class,vl)
		in
		let jsig =
			let args = List.map (fun (v,_) -> self#vtype v.v_type) tf.tf_args in
			let args = match outside with
				| None -> args
				| Some(ctx_class,_) -> ctx_class#get_jsig :: args
			in
			method_sig args (if ExtType.is_void (follow tf.tf_type) then None else Some (self#vtype tf.tf_type))
		in
		let jm = jc#spawn_method name jsig [MPublic;MStatic] in
		let handler = new texpr_to_jvm gctx jc jm tf.tf_type in
		begin match outside with
		| None -> ()
		| Some(ctx_class,vl) ->
			handler#set_context ctx_class;
			let name = match vl with [v] -> v.v_name | _ -> "_hx_ctx" in
			ignore(handler#add_named_local name ctx_class#get_jsig)
		end;
		List.iter (fun (v,_) ->
			ignore(handler#add_local v VarArgument);
		) tf.tf_args;
		jm#finalize_arguments;
		handler#texpr RReturn tf.tf_expr;
		self#read_closure true jc#get_this_path name jsig;
		outside

	(* access *)

	method read_native_array vta vte =
		NativeArray.read code vta vte

	method write_native_array vta vte =
		NativeArray.write code vta vte

	method read_closure is_static path name jsig_method =
		let offset = pool#add_field path name jsig_method FKMethod in
		let offset = pool#add (ConstMethodHandle((if is_static then 6 else 5), offset)) in
		code#ldc offset jsig_method

	method read t e1 fa =
		match fa with
		| FStatic(c,({cf_kind = Method (MethNormal | MethInline)} as cf)) ->
			self#read_closure true c.cl_path cf.cf_name (jsignature_of_type cf.cf_type);
			self#cast cf.cf_type;
		| FStatic(c,cf) ->
			let offset = add_field pool c cf in
			let t = self#vtype cf.cf_type in
			code#getstatic offset t
		| FInstance({cl_path = ([],"String")},_,{cf_name = "length"}) ->
			self#texpr rvalue_any e1;
			let vtobj = self#vtype e1.etype in
			jm#invokevirtual string_path "length" vtobj (method_sig [] (Some TInt))
		| FInstance({cl_path = (["java"],"NativeArray")},_,{cf_name = "length"}) ->
			self#texpr rvalue_any e1;
			let vtobj = self#vtype e1.etype in
			code#arraylength vtobj
		| FInstance(c,tl,cf) when not (is_interface_var_access c cf) ->
			let vt = self#vtype cf.cf_type in
			let offset = add_field pool c cf in
			self#texpr rvalue_any e1;
			let vtobj = self#vtype e1.etype in
			code#getfield offset vtobj vt
		| FEnum(en,ef) ->
			let jsig = self#vtype ef.ef_type in
			let offset = pool#add_field en.e_path ef.ef_name jsig FKField in
			code#getstatic offset jsig
		| FAnon {cf_name = s} ->
			self#texpr rvalue_any e1;
			let default () =
				self#string s;
				jm#invokestatic haxe_jvm_path "readField" (method_sig [object_sig;string_sig] (Some object_sig));
				self#cast t;
			in
			begin match follow e1.etype with
			| TAnon an ->
				let path,_ = TAnonIdentifiaction.identify gctx an.a_fields in
				code#dup;
				code#instanceof path;
				jm#if_then_else
					(fun () -> code#if_ref CmpEq)
					(fun () ->
						jm#cast (object_path_sig path);
						jm#getfield path s (self#vtype t);
					)
					(fun () -> default());
			| _ ->
				default();
			end
		| FDynamic s | FInstance(_,_,{cf_name = s}) ->
			self#texpr rvalue_any e1;
			self#string s;
			jm#invokestatic haxe_jvm_path "readField" (method_sig [object_sig;string_sig] (Some object_sig));
			self#cast t;
		| FClosure((Some(c,_)),cf) ->
			let jsig = self#vtype cf.cf_type in
			self#read_closure false c.cl_path cf.cf_name jsig;
			self#texpr rvalue_any e1;
			jm#invokevirtual method_handle_path "bindTo" method_handle_sig (method_sig [object_sig] (Some method_handle_sig));
		| _ ->
			assert false

	method read_write ret ak e (f : unit -> unit) (t : Type.t) =
		let apply dup =
			if ret <> RVoid && ak = AKPost then dup();
			f();
			if ret <> RVoid && ak <> AKPost then dup();
		in
		match (Texpr.skip e).eexpr with
		| TLocal v ->
			let _,load,store = self#get_local v in
			if ak <> AKNone then load();
			apply (fun () -> code#dup);
			store();
		| TField(_,FStatic(c,cf)) ->
			let vt = self#vtype t in
			let offset = add_field pool c cf in
			if ak <> AKNone then code#getstatic offset vt;
			apply (fun () -> code#dup);
			code#putstatic offset vt
		| TField(e1,FInstance(c,tl,cf)) when not (is_interface_var_access c cf) ->
			let vt = self#vtype t in
			let offset = add_field pool c cf in
			let vtobj = self#vtype e1.etype in
			self#texpr rvalue_any e1;
			if ak <> AKNone then begin
				code#dup;
				code#getfield offset vtobj vt;
			end;
			apply (fun () -> code#dup_x1);
			code#putfield offset vtobj vt
		| TField(e1,(FDynamic s | FAnon {cf_name = s} | FInstance(_,_,{cf_name = s}))) ->
			self#texpr rvalue_any e1;
			if ak <> AKNone then code#dup;
			self#string s;
			if ak <> AKNone then begin
				code#dup_x1;
				jm#invokestatic haxe_jvm_path "readField" (method_sig [object_sig;string_sig] (Some object_sig));
				self#cast e.etype;
			end;
			apply (fun () -> code#dup_x2);
			self#cast (self#mknull e.etype);
			jm#invokestatic haxe_jvm_path "writeField" (method_sig [object_sig;string_sig;object_sig] None)
		| TArray(e1,e2) ->
			begin match follow e1.etype with
				| TInst({cl_path = ([],"Array")} as c,[t]) ->
					let cf_get = PMap.find "__get" c.cl_fields in
					let cf_set = PMap.find "__set" c.cl_fields in
					let offset_get = add_field pool c cf_get in
					let offset_set = add_field pool c cf_set in
					let vta = self#vtype e1.etype in
					let t = self#mknull t in
					let vte = self#vtype t in
					self#texpr rvalue_any e1;
					if ak <> AKNone then code#dup;
					self#texpr rvalue_any e2;
					if ak <> AKNone then begin
						code#dup_x1;
						code#invokevirtual offset_get vta [TInt] [vte];
						self#cast e.etype;
					end;
					apply (fun () -> code#dup_x2;);
					self#cast t;
					code#invokevirtual offset_set vta [TInt;vte] []
				| TInst({cl_path = (["java"],"NativeArray")},_) ->
					let vte = self#vtype t in
					let vta = self#vtype e1.etype in
					self#texpr rvalue_any e1;
					if ak <> AKNone then code#dup;
					self#texpr rvalue_any e2;
					if ak <> AKNone then begin
						code#dup_x1;
						self#read_native_array vta vte
					end;
					apply (fun () -> code#dup_x2);
					self#cast t;
					self#write_native_array vta vte
				| t ->
					Error.error (s_type (print_context()) t) e.epos;
				end
		| _ ->
			print_endline (s_expr_ast false "" (s_type (print_context())) e);
			assert false

	(* branching *)

	method if_null t =
		(fun () -> code#if_null_ref t)

	method if_not_null t =
		(fun () -> code#if_nonnull_ref t)

	method condition e = match (Texpr.skip e).eexpr with
		| TBinop((OpEq | OpNotEq | OpLt | OpGt | OpLte | OpGte) as op,e1,e2) ->
			let op = convert_cmp_op op in
			let op = flip_cmp_op op in
			self#binop_compare op e1 e2
		| _ ->
			self#texpr rvalue_any e;
			jm#cast TBool;
			(fun () -> code#if_ref CmpEq)

	method maybe_make_jump =
		let r = ref code#get_fp in
		if not jm#is_terminated then code#goto r;
		r

	method close_jumps rl =
		let fp' = code#get_fp in
		let term = List.fold_left (fun term (term',r) ->
			r := fp' - !r;
			term && term'
		) true rl in
		jm#set_terminated term;
		if not term then jm#add_stack_frame;

	method int_switch ret e1 cases def =
		let is_exhaustive = OptimizerTexpr.is_exhaustive e1 in
		let def,cases = match def,cases with
			| None,(_,ec) :: cases when is_exhaustive ->
				Some ec,cases
			| _ ->
				def,cases
		in
		self#texpr rvalue_any e1;
		jm#cast TInt;
		let flat_cases = DynArray.create () in
		let case_lut = ref IntMap.empty in
		let fp = code#get_fp in
		let imin = ref max_int in
		let imax = ref min_int in
		let cases = List.map (fun (el,e) ->
			let rl = List.map (fun e ->
				match e.eexpr with
				| TConst (TInt i32) ->
					let r = ref fp in
					let i = Int32.to_int i32 in
					if i < !imin then imin := i;
					if i > !imax then imax := i;
					DynArray.add flat_cases (i,r);
					case_lut := IntMap.add i r !case_lut;
					r
				| _ ->
					assert false
			) el in
			(rl,e)
		) cases in
		let offset_def = ref fp in
		(* No idea what's a good heuristic here... *)
		let use_tableswitch = (!imax - !imin) < (DynArray.length flat_cases + 10) in
		if use_tableswitch then begin
			let offsets = Array.init (!imax - !imin + 1) (fun i ->
				try IntMap.find (i + !imin) !case_lut
				with Not_found -> offset_def
			) in
			code#tableswitch offset_def !imin !imax offsets
		end else begin
			let a = DynArray.to_array flat_cases in
			Array.sort (fun (i1,_) (i2,_) -> compare i1 i2) a;
			code#lookupswitch offset_def a;
		end;
		let restore = jm#start_branch in
		offset_def := code#get_fp - !offset_def;
		let r_def = match def with
			| None ->
				if ret = RVoid then offset_def else (ref 0)
			| Some e ->
				jm#add_stack_frame;
				self#texpr ret e;
				self#maybe_make_jump
		in

		let rec loop acc cases = match cases with
		| (rl,e) :: cases ->
			restore();
			jm#add_stack_frame;
			List.iter (fun r -> r := code#get_fp - !r) rl;
			self#texpr ret e;
			loop ((jm#is_terminated,self#maybe_make_jump) :: acc) cases
		| [] ->
			List.rev acc
		in
		let rl = loop [] cases in
		self#close_jumps ((jm#is_terminated,r_def) :: rl)

	method switch ret e1 cases def =
		if cases = [] then
			self#texpr ret e1
		else if List.for_all is_const_int_pattern cases then
			self#int_switch ret e1 cases def
		else begin
			(* TODO: rewriting this is stupid *)
			let el = List.rev_map (fun (el,e) ->
				let f e' = mk (TBinop(OpEq,e1,e')) com.basic.tbool e'.epos in
				let e_cond = match el with
					| [] -> assert false
					| [e] -> f e
					| e :: el ->
						List.fold_left (fun eacc e ->
							mk (TBinop(OpBoolOr,eacc,f e)) com.basic.tbool e.epos
						) (f e) el
				in
				(e_cond,e)
			) cases in
			let e = List.fold_left (fun e_else (e_cond,e_then) -> Some (mk (TIf(e_cond,e_then,e_else)) e_then.etype e_then.epos)) def el in
			self#texpr ret (Option.get e)
		end

	(* binops *)

	method binop_exprs cast_type f1 f2 =
		f1();
		Option.may (jm#cast ~allow_to_string:true) cast_type;
		f2();
		Option.may (jm#cast ~allow_to_string:true) cast_type;

	method get_binop_type t1 t2 = match jsignature_of_type (follow t1),jsignature_of_type (follow t2) with
		| TObject((["java";"lang"],"String"),_),_
		| _,TObject((["java";"lang"],"String"),_) ->
			Some string_sig
		| TLong,_ | _,TLong -> Some TLong
		| TDouble,_ | _,TDouble -> Some TDouble
		| TFloat,_ | _,TFloat -> Some TFloat
		| TInt,_ | _,TInt -> Some TInt
		| TShort,_ | _,TShort -> Some TShort
		| TChar,_ | _,TChar -> Some TChar
		| TByte,_ | _,TByte -> Some TByte
		| TBool,_ | _,TBool -> Some TBool
		| _ -> None

	method do_compare op =
		match code#get_stack#get_stack_items 2 with
		| [TInt | TByte | TChar | TBool;TInt | TByte | TChar | TBool] ->
			(fun () -> code#if_icmp_ref op)
		| [t2;TObject((["java";"lang"],"String"),[]) as t1] ->
			(* TODO: We need a slow compare if java.lang.Object is involved because it could refer to String *)
			(fun () ->
				jm#invokevirtual string_path "equals" t1 (method_sig [object_sig] (Some TBool));
				code#if_ref (flip_cmp_op op)
			)
		| [TObject((["java";"lang"],"String"),[]) as t1;t2] ->
			(fun () ->
				code#swap;
				jm#invokevirtual string_path "equals" t1 (method_sig [object_sig] (Some TBool));
				code#if_ref (flip_cmp_op op)
			)
		| [TObject((["java";"lang"],"Object"),[]) | TTypeParameter _;_]
		| [_;TObject((["java";"lang"],"Object"),[]) | TTypeParameter _] ->
			(fun () ->
				jm#invokestatic haxe_jvm_path "equals" (method_sig [object_sig;object_sig] (Some TBool));
				code#if_ref (flip_cmp_op op)
			)
		| [TObject _ as t1;TObject _ as t2] ->
			(fun () -> (if op = CmpNe then code#if_acmp_ne_ref else code#if_acmp_eq_ref) t1 t2)
		| [TDouble;TDouble] ->
			code#dcmpl;
			(fun () -> code#if_ref op)
		| [TLong;TLong] ->
			code#lcmpl;
			(fun () -> code#if_ref op)
		| [t1;t2] ->
			jerror (Printf.sprintf "Can't compare %s and %s" (generate_signature false t1) (generate_signature false t2))
		| tl ->
			jerror (Printf.sprintf "Bad stack: %s" (String.concat ", " (List.map (generate_signature false) tl)));

	method binop_compare op e1 e2 =
		match (Texpr.skip e1),(Texpr.skip e2) with
		| {eexpr = TConst TNull},e1
		| e1,{eexpr = TConst TNull} ->
			self#texpr rvalue_any e1;
			(if op = CmpNe then self#if_not_null else self#if_null) (self#vtype e1.etype);
		| {eexpr = TConst (TInt i32);etype = t2},e1 when Int32.to_int i32 = 0 ->
			self#texpr rvalue_any e1;
			self#cast t2;
			(fun () -> code#if_ref op)
		| e1,{eexpr = TConst (TInt i32); etype = t2;} when Int32.to_int i32 = 0 ->
			self#texpr rvalue_any e1;
			self#cast t2;
			(fun () -> code#if_ref op)
		| _ ->
			let f e () = self#texpr rvalue_any e in
			self#binop_exprs (self#get_binop_type e1.etype e2.etype) (f e1) (f e2);
			self#do_compare op

	method binop_numeric (opi,opd,opl) cast_type f1 f2 =
		self#binop_exprs cast_type f1 f2;
		begin match code#get_stack#get_stack_items 2 with
		| [TInt | TByte | TShort;TInt | TByte | TShort] -> opi ()
		| [TDouble;TDouble] -> opd ()
		| [TLong;TLong] -> opl ()
		| [TObject((["java";"lang"],"String"),[]);TObject((["java";"lang"],"String"),[]);] ->
			jm#invokestatic haxe_jvm_path "stringConcat" (method_sig [object_sig;object_sig] (Some string_sig))
		| [t1;t2] -> jerror (Printf.sprintf "Can't numop %s and %s" (generate_signature false t1) (generate_signature false t2))
		| tl -> jerror (Printf.sprintf "Bad stack: %s" (String.concat ", " (List.map (generate_signature false) tl)));
		end

	method binop_basic ret op cast_type f1 f2 =
		let long_op_int () =
			let is_long = match cast_type with
				| Some TLong -> true
				| _ -> false
			in
			f1();
			jm#cast (if is_long then TLong else TInt);
			f2();
			jm#cast TInt; (* ! *)
			is_long
		in
		match op with
		| OpAdd -> self#binop_numeric ((fun () -> code#iadd),(fun () -> code#dadd),(fun () -> code#ladd)) cast_type f1 f2
		| OpSub -> self#binop_numeric ((fun () -> code#isub),(fun () -> code#dsub),(fun () -> code#lsub)) cast_type f1 f2
		| OpMult -> self#binop_numeric ((fun () -> code#imul),(fun () -> code#dmul),(fun () -> code#lmul)) cast_type f1 f2
		| OpDiv ->
			let operand f =
				f ();
				begin match cast_type with
				| Some TLong -> jm#cast TLong
				| _ -> jm#cast TDouble
				end
			in
			operand f1;
			operand f2;
			begin match cast_type with
			| Some TLong -> code#ldiv
			| _ -> code#ddiv
			end
		| OpAnd -> self#binop_numeric ((fun () -> code#iand),(fun () -> assert false),(fun () -> code#land_)) cast_type f1 f2
		| OpOr -> self#binop_numeric ((fun () -> code#ior),(fun () -> assert false),(fun () -> code#lor_)) cast_type f1 f2
		| OpXor -> self#binop_numeric ((fun () -> code#ixor),(fun () -> assert false),(fun () -> code#lxor_)) cast_type f1 f2
		| OpShl ->
			let is_long = long_op_int () in
			if is_long then code#lshl else code#ishl
		| OpShr ->
			let is_long = long_op_int () in
			if is_long then code#lshr else code#ishr
		| OpUShr ->
			let is_long = long_op_int () in
			if is_long then code#lushr else code#iushr
		| OpMod -> self#binop_numeric ((fun () -> code#irem),(fun () -> code#drem),(fun () -> code#lrem)) cast_type f1 f2
		| OpBoolAnd ->
			let operand f =
				f();
				jm#cast TBool;
			in
			operand f1;
			jm#if_then_else
				(fun () -> code#if_ref CmpEq)
				(fun () -> operand f2)
				(fun () -> code#bconst false)
		| OpBoolOr ->
			let operand f =
				f();
				jm#cast TBool;
			in
			operand f1;
			jm#if_then_else
				(fun () -> code#if_ref CmpEq)
				(fun () -> code#bconst true)
				(fun () -> operand f2)
		| _ ->
			assert false

	method boolop f =
		jm#if_then_else
			f
			(fun () -> code#bconst true)
			(fun () -> code#bconst false)

	method var_slot_is_in_int8_range v =
		let slot,_,_ = self#get_local v in
		in_range true Int8Range slot

	method binop ret op e1 e2 t = match op with
		| OpEq | OpNotEq | OpLt | OpGt | OpLte | OpGte ->
			let op = convert_cmp_op op in
			let op = flip_cmp_op op in
			self#boolop (self#binop_compare op e1 e2)
		| OpAssign ->
			let f () =
				self#texpr rvalue_any e2;
				self#cast e1.etype;
			in
			self#read_write ret AKNone e1 f t
		| OpAssignOp op ->
			begin match op,(Texpr.skip e1).eexpr,(Texpr.skip e2).eexpr with
			| OpAdd,TLocal v,TConst (TInt i32) when not (is_null v.v_type) && in_range false Int8Range (Int32.to_int i32) && self#var_slot_is_in_int8_range v->
				let slot,load,_ = self#get_local v in
				let i = Int32.to_int i32 in
				code#iinc slot i;
				if ret <> RVoid then load();
			| OpSub,TLocal v,TConst (TInt i32) when not (is_null v.v_type) && in_range false Int8Range (-Int32.to_int i32) && self#var_slot_is_in_int8_range v ->
				let slot,load,_ = self#get_local v in
				let i = -Int32.to_int i32 in
				code#iinc slot i;
				if ret <> RVoid then load();
			| _ ->
				let f () =
					self#binop_basic ret op (self#get_binop_type e1.etype e2.etype) (fun () -> ()) (fun () -> self#texpr rvalue_any e2);
					if is_null e1.etype then self#expect_reference_type;
				in
				self#read_write ret AKPre e1 f t
			end
		| _ ->
			let f e () = self#texpr rvalue_any e in
			self#binop_basic ret op (self#get_binop_type e1.etype e2.etype) (f e1) (f e2)

	method unop ret op flag e =
		match op,(Texpr.skip e).eexpr with
		| (Increment | Decrement),TLocal v when ExtType.is_int v.v_type && self#var_slot_is_in_int8_range v ->
			let slot,load,_ = self#get_local v in
			if flag = Postfix && ret <> RVoid then load();
			code#iinc slot (if op = Increment then 1 else -1);
			if flag = Prefix && ret <> RVoid then load();
		| (Increment | Decrement),_ ->
			let is_null = is_null e.etype in
			let f () =
				let t = jsignature_of_type (follow e.etype) in
				begin match t with
				| TLong ->
					jm#cast TLong;
					code#lconst Int64.zero;
					if op = Increment then code#ladd else code#lsub
				| TDouble ->
					jm#cast TDouble;
					code#dconst 1.;
					if op = Increment then code#dadd else code#dsub
				| TInt ->
					jm#cast TInt;
					code#iconst Int32.one;
					if op = Increment then code#iadd else code#isub
				| _ ->
					assert false
				end
			in
			self#read_write ret (if flag = Prefix then AKPre else AKPost) e f e.etype;
			if is_null then self#expect_reference_type;
		| Neg,_ ->
			self#texpr rvalue_any e;
			begin match jsignature_of_type (follow e.etype) with
			| TLong -> code#lneg;
			| TDouble -> code#dneg;
			| TInt -> code#ineg;
			| _ -> assert false
			end;
		| Not,_ ->
			jm#if_then_else
				(self#condition e)
				(fun () -> code#bconst false)
				(fun () -> code#bconst true)
		| NegBits,_ ->
			self#texpr rvalue_any e;
			begin match jsignature_of_type (follow e.etype) with
			| TInt ->
				code#iconst Int32.minus_one;
				code#ixor;
			| TLong ->
				code#lconst Int64.minus_one;
				code#lxor_;
			| _ ->
				assert false
			end

	(* calls *)

	(* TODO: this bloody mess tries to find the right overload because TNew loses it... *)
	method check_hack hack tl el = match hack with
		| None ->
			tl
		| Some rcf ->
			let tl' = List.map (fun e -> e.etype) el in
			let rec loop cfl = match cfl with
				| cf :: cfl ->
					begin match follow cf.cf_type with
						| TFun(tl'',_) ->
							let rec loop2 tl' tl = match tl',tl with
								| t' :: tl',(_,_,t) :: tl ->
									(try Type.unify t' t; loop2 tl' tl with _ -> loop cfl)
								| [],[] ->
									rcf := cf;
									tl''
								| _ ->
									loop cfl
							in
							loop2 tl' tl''
						| _ ->
							assert false
					end;

				| [] ->
					tl
			in
			loop (!rcf :: !rcf.cf_overloads)

	method call_arguments ?(hack=None) t el =
		let tl,tr = match follow t with
			| TFun(tl,tr) -> tl,tr
			| _ -> (List.map (fun _ -> "",false,t_dynamic) el),t_dynamic
		in
		let tl = self#check_hack hack tl el in
		let rec loop acc tl el = match tl,el with
			| (_,_,t) :: tl,e :: el ->
				self#texpr rvalue_any e;
				self#cast t;
				loop (self#vtype t :: acc) tl el
			| _,[] -> List.rev acc
			| [],e :: el ->
				(* TODO: this sucks *)
				self#texpr rvalue_any e;
				loop (self#vtype e.etype :: acc) [] el
		in
		let tl = loop [] tl el in
		let tr = if ExtType.is_void (follow tr) then None else Some (self#vtype tr) in
		tl,tr

	method call ret tr e1 el =
		let retype tr = match tr with None -> [] | Some t -> [t] in
		let tro = match (Texpr.skip e1).eexpr with
		| TField(_,FStatic({cl_path = ["haxe";"jvm"],"Jvm"},({cf_name = "referenceEquals"} as cf))) ->
			let tl,tr = self#call_arguments cf.cf_type el in
			begin match tl with
				| [t1;t2] -> self#boolop (fun () -> code#if_acmp_ne_ref t1 t2);
				| _ -> assert false
			end;
			tr
		| TField(_,FStatic({cl_path = ["haxe";"jvm"],"Jvm"},({cf_name = "instanceof"}))) ->
			begin match el with
				| [e1;{eexpr = TTypeExpr mt}] ->
					self#texpr rvalue_any e1;
					self#expect_reference_type;
					code#instanceof (path_map (t_infos mt).mt_path);
					Some TBool
				| _ -> Error.error "Type expression expected" e1.epos
			end;
		| TField(_,FStatic({cl_path = ["haxe";"jvm"],"Jvm"},({cf_name = "invokedynamic"}))) ->
			begin match el with
				| e_bsm :: {eexpr = TConst (TString name)} :: {eexpr = TArrayDecl el_static_args} :: el ->
					let t = tfun (List.map (fun e -> e.etype) el) tr in
					let tl,tr = self#call_arguments t el in
					let path,mname = match e_bsm.eexpr with
						| TField(_,FStatic(c,cf)) -> c.cl_path,cf.cf_name
						| _ -> Error.error "Reference to bootstrap method expected" e_bsm.epos
					in
					let rec loop consts jsigs static_args = match static_args with
						| e :: static_args ->
							let const,jsig =  match e.eexpr with
							| TConst (TString s) -> pool#add_const_string s,string_sig
							| TConst (TInt i) -> pool#add (ConstInt i),TInt
							| TConst (TFloat f) -> pool#add (ConstDouble (float_of_string f)),TDouble
							| TField(_,FStatic(c,cf)) ->
								let offset = pool#add_field c.cl_path cf.cf_name (self#vtype cf.cf_type) FKMethod in
								pool#add (ConstMethodHandle(6, offset)),method_handle_sig
							| _ -> Error.error "Invalid static argument" e.epos
							in
							loop (const :: consts) (jsig :: jsigs) static_args
						| [] ->
							List.rev consts,List.rev jsigs
					in
					let consts,jsigs = loop [] [] el_static_args in
					let mtl = method_lookup_sig :: string_sig :: method_type_sig :: jsigs in
					let index = jc#get_bootstrap_method path mname (method_sig mtl (Some call_site_sig)) consts in
					let jsig_method = method_sig tl tr in
					let offset_info = pool#add_name_and_type name jsig_method FKMethod in
					let offset = pool#add (ConstInvokeDynamic(index,offset_info)) in
					code#invokedynamic offset tl (retype tr);
					tr
				| _ ->
					Error.error "Bad invokedynamic call" e1.epos
			end
		| TField(_,FStatic(c,({cf_kind = Method (MethNormal | MethInline)} as cf))) ->
			let tl,tr = self#call_arguments cf.cf_type el in
			jm#invokestatic c.cl_path cf.cf_name (method_sig tl tr);
			tr
		| TField(e1,FInstance(c,tl,({cf_kind = Method (MethNormal | MethInline)} as cf))) ->
			let is_super = match e1.eexpr with
			| TConst TSuper ->
				code#aload jc#get_jsig 0;
				true
			| _ ->
				self#texpr rvalue_any e1;
				false
			in
			let hack_cf = ref cf in
			let tl,tr = self#call_arguments ~hack:(Some hack_cf) cf.cf_type el in
			let t1 = self#vtype e1.etype in
			let offset = add_field pool c !hack_cf in
			(if is_super then code#invokespecial else if c.cl_interface then code#invokeinterface else code#invokevirtual) offset t1 tl (retype tr);
			tr
		| TField(_,FEnum(en,ef)) ->
			let tl,_ = self#call_arguments ef.ef_type el in
			let tr = self#vtype tr in
			jm#invokestatic en.e_path ef.ef_name (method_sig tl (Some tr));
			Some tr
		| TConst TSuper ->
			let jsig = (TUninitialized None) in
			code#aload jsig 0;
			begin match follow e1.etype with
			| TInst(c,_) ->
				begin match get_constructor (fun cf -> cf.cf_type) c with
				| _,cf ->
					let hack_cf = ref cf in
					let tl,_ = self#call_arguments ~hack:(Some hack_cf) cf.cf_type el in
					let offset = add_field pool c !hack_cf in
					code#invokespecial offset jsig tl [];
					jm#set_this_initialized;
					None
				end;
			| _ ->
				assert false
			end
		| TIdent "__array__" ->
			begin match follow tr with
			| TInst({cl_path = (["java"],"NativeArray")},[t]) ->
				code#iconst (Int32.of_int (List.length el));
				let jasig,_ = self#new_native_array (self#vtype t) el in
				Some jasig
			| _ ->
				assert false
			end
		| _ ->
			self#texpr rvalue_any e1;
			jm#cast method_handle_sig;
			let tl,tr = self#call_arguments e1.etype el in
			jm#invokevirtual method_handle_path "invoke" (self#vtype e1.etype) (method_sig tl tr);
			tr
		in
		match ret = RVoid,tro with
		| true,Some _ -> code#pop
		| true,None -> ()
		| false,Some _ -> self#cast tr;
		| false,None -> assert false

	(* exceptions *)

	method throw vt =
		jm#expect_reference_type;
		jm#invokestatic (["haxe";"jvm"],"Exception") "wrap" (method_sig [object_sig] (Some exception_sig));
		code#athrow;
		jm#set_terminated true

	method try_catch ret e1 catches =
		let restore = jm#start_branch in
		let fp_from = code#get_fp in
		let old_exceptions = caught_exceptions in
		let excl = List.map (fun (v,e) ->
			let exc = new haxe_exception gctx v.v_type in
			caught_exceptions <- exc :: caught_exceptions;
			exc,v,e
		) catches in
		self#texpr ret e1;
		caught_exceptions <- old_exceptions;
		let term_try = jm#is_terminated in
		let r_try = self#maybe_make_jump in
		let fp_to = code#get_fp in
		let unwrap () =
			code#dup;
			code#instanceof haxe_exception_path;
			jm#if_then_else
				(fun () -> code#if_ref CmpEq)
				(fun () ->
					jm#cast haxe_exception_sig;
					jm#getfield (["haxe";"jvm"],"Exception") "value" object_sig;
				)
				(fun () -> jm#cast object_sig);
		in
		let start_exception_block path jsig =
			restore();
			let fp_target = code#get_fp in
			let offset = pool#add_path path in
			jm#add_exception {
				exc_start_pc = fp_from;
				exc_end_pc = fp_to;
				exc_handler_pc = fp_target;
				exc_catch_type = Some offset;
			};
			code#get_stack#push jsig;
			jm#add_stack_frame
		in
		let run_catch_expr v e =
			let pop_scope = jm#push_scope in
			let _,_,store = self#add_local v VarWillInit in
			store();
			self#texpr ret e;
			pop_scope();
			jm#is_terminated
		in
		let add_catch (exc,v,e) =
			start_exception_block exc#get_native_exception_path exc#get_native_exception_type;
			if not exc#is_native_exception then begin
				unwrap();
				self#cast v.v_type
			end;
			let term = run_catch_expr v e in
			let r = self#maybe_make_jump in
			term,r
		in
		let commit_instanceof_checks excl =
			start_exception_block throwable_path throwable_sig;
			let pop_scope = jm#push_scope in
			let _,load,save = jm#add_local "exc" throwable_sig VarWillInit in
			code#dup;
			save();
			unwrap();
			let restore = jm#start_branch in
			let rl = ref [] in
			let rec loop excl = match excl with
				| [] ->
					code#pop;
					load();
					code#athrow;
					jm#set_terminated true
				| (_,v,e) :: excl ->
					code#dup;
					let path = match self#vtype (self#mknull v.v_type) with TObject(path,_) -> path | _ -> assert false in
					code#instanceof path;
					jm#if_then_else
						(fun () -> code#if_ref CmpEq)
						(fun () ->
							restore();
							self#cast v.v_type;
							let term = run_catch_expr v e in
							rl := (term,ref 0) :: !rl;
						)
						(fun () -> loop excl)
			in
			loop excl;
			pop_scope();
			!rl
		in
		let rec loop acc excl = match excl with
			| (exc,v,e) :: excl ->
				if List.exists (fun (exc',_,_) -> exc'#is_assignable_to exc) excl || excl = [] && not exc#is_native_exception then begin
					let res = commit_instanceof_checks ((exc,v,e) :: excl) in
					acc @ res
				end else begin
					let res = add_catch (exc,v,e) in
					loop (res :: acc) excl
				end
			| [] ->
				acc
		in
		let rl = loop [] excl in
		self#close_jumps ((term_try,r_try) :: rl)

	(* texpr *)

	method string s =
		let offset = pool#add_const_string s in
		code#sconst (string_sig) offset

	method const t ct = match ct with
		| Type.TInt i32 -> code#iconst i32
		| TFloat f -> code#dconst (float_of_string f)
		| TBool true -> code#bconst true
		| TBool false -> code#bconst false
		| TNull -> code#aconst_null (self#vtype t)
		| TThis -> code#aload jc#get_jsig 0
		| TString s -> self#string s
		| TSuper -> failwith "Invalid super access"

	method new_native_array_f jsig (fl : (unit -> unit) list) =
		let jasig = NativeArray.create code pool jsig in
		List.iteri (fun i f ->
			code#dup;
			code#iconst (Int32.of_int i);
			f();
			jm#cast jsig;
			self#write_native_array jasig jsig
		) fl;
		jasig,jsig

	method new_native_array jsig el =
		self#new_native_array_f jsig (List.map (fun e -> fun () -> self#texpr rvalue_any e) el)

	method construct ret path t f =
		let offset_class = pool#add_path (path_map path) in
		code#new_ offset_class;
		if ret <> RVoid then code#dup;
		let tl,offset = f() in
		code#invokespecial offset t tl [];
		if ret <> RVoid then jm#set_top_initialized (object_path_sig path)

	method basic_type_path name =
		let offset = pool#add_field (["java";"lang"],name) "TYPE" java_class_sig FKField in
		code#getstatic offset java_class_sig

	method type_expr = function
		| TByte -> self#basic_type_path "Byte"
		| TChar -> self#basic_type_path "Character"
		| TDouble -> self#basic_type_path "Double"
		| TFloat -> self#basic_type_path "Float"
		| TInt -> self#basic_type_path "Integer"
		| TLong -> self#basic_type_path "Long"
		| TShort -> self#basic_type_path "Short"
		| TBool -> self#basic_type_path "Boolean"
		| TObject(path,_) ->
			let path = path_map path in
			let offset = pool#add_path path in
			let t = object_path_sig path in
			code#ldc offset (TObject(java_class_path,[TType(WNone,t)]))
		| TMethod _ ->
			let offset = pool#add_path method_handle_path in
			code#ldc offset (TObject(java_class_path,[TType(WNone,method_handle_sig)]))
		| TTypeParameter _ ->
			let offset = pool#add_path object_path in
			code#ldc offset (TObject(java_class_path,[TType(WNone,object_sig)]))
		| TArray _ as t ->
			(* TODO: this seems hacky *)
			let offset = pool#add_path ([],generate_signature false t) in
			code#ldc offset (TObject(java_class_path,[TType(WNone,object_sig)]))
		| jsig ->
			print_endline (generate_signature false jsig);
			assert false

	method texpr ret e =
		try
			if not jm#is_terminated then self#texpr' ret e
		with Failure s ->
			raise (HarderFailure (Printf.sprintf "Expr %s\n%s" (s_expr_pretty false "" false (s_type (print_context())) e) s))

	method texpr' ret e =
		code#set_line (Lexer.get_error_line e.epos);
		match e.eexpr with
		| TVar(v,Some e1) ->
			self#texpr rvalue_any e1;
			self#cast v.v_type;
			let _,_,store = self#add_local v VarWillInit in
			store()
		| TVar(v,None) ->
			ignore(self#add_local v VarNeedDefault);
		| TLocal _ | TConst _  | TTypeExpr _ when ret = RVoid ->
			()
		| TLocal v ->
			let _,load,_ = self#get_local v in
			load()
		| TTypeExpr mt ->
			self#type_expr (jsignature_of_type (type_of_module_type mt))
		| TUnop(op,flag,e1) ->
			begin match op with
			| Not | Neg | NegBits when ret = RVoid -> self#texpr ret e1
			| _ -> self#unop ret op flag e1
			end
		| TBinop(op,e1,e2) ->
			begin match op with
			| OpAssign | OpAssignOp _ -> self#binop ret op e1 e2 e.etype
			| _ when ret = RVoid ->
				self#texpr ret e1;
				self#texpr ret e2;
			| _ ->
				self#binop ret op e1 e2 e.etype
			end
		| TConst ct ->
			self#const e.etype ct
		| TIf(e1,e2,None) ->
			jm#if_then
				(self#condition e1)
				(fun () -> self#texpr RVoid (mk_block e2))
		| TIf(e1,e2,Some e3) ->
			jm#if_then_else
				(self#condition e1)
				(fun () ->
					self#texpr ret (mk_block e2);
					if ret <> RVoid then self#cast e.etype
				)
				(fun () ->
					self#texpr ret (mk_block e3);
					if ret <> RVoid then self#cast e.etype;
				)
		| TSwitch(e1,cases,def) ->
			self#switch ret e1 cases def
		| TWhile(e1,e2,flag) -> (* TODO: do-while *)
			(* TODO: could optimize a bit *)
			jm#add_stack_frame;
			let fp = code#get_fp in
			let old_continue = continue in
			continue <- fp;
			let old_breaks = breaks in
			breaks <- [];
			let restore = jm#start_branch in
			let jump_then = (self#condition e1 ()) in
			let pop_scope = jm#push_scope in
			self#texpr RVoid e2;
			if not jm#is_terminated then code#goto (ref (fp - code#get_fp));
			pop_scope();
			restore();
			jump_then := code#get_fp - !jump_then;
			jm#add_stack_frame;
			let fp' = code#get_fp in
			List.iter (fun r -> r := fp' - !r) breaks;
			continue <- old_continue;
			breaks <- old_breaks;
		| TBreak ->
			let r = ref (code#get_fp) in
			code#goto r;
			breaks <- r :: breaks;
			jm#set_terminated true;
		| TContinue ->
			code#goto (ref (continue - code#get_fp));
			jm#set_terminated true;
		| TTry(e1,catches) ->
			self#try_catch ret e1 catches
		| TField(e1,fa) ->
			if ret = RVoid then self#texpr ret e1
			else self#read e.etype e1 fa;
		| TCall(e1,el) ->
			self#call ret e.etype e1 el
		| TNew({cl_path = (["java"],"NativeArray")},[t],[e1]) ->
			self#texpr rvalue_any e1;
			(* Technically this could throw... but whatever *)
			if ret <> RVoid then ignore(self#new_native_array (jsignature_of_type t) [])
		| TNew(c,tl,el) ->
			begin match get_constructor (fun cf -> cf.cf_type) c with
			| _,cf ->
				let f () =
					let hack_cf = ref cf in
					let tl,_ = self#call_arguments ~hack:(Some hack_cf) cf.cf_type el in
					let offset = add_field pool c !hack_cf in
					tl,offset
				in
				self#construct ret c.cl_path (self#vtype (TInst(c,tl))) f;
			end
		| TReturn None ->
			code#return_void;
			jm#set_terminated true;
		| TReturn (Some e1) ->
			self#texpr rvalue_any e1;
			self#cast return_type;
			let vt = self#vtype return_type in
			code#return_value vt;
			jm#set_terminated true;
		| TFunction tf ->
			begin match self#tfunction e tf with
			| None ->
				()
			| Some(_,[v]) ->
				let _,load,_ = self#get_local v in
				load();
				self#expect_reference_type;
				jm#invokevirtual method_handle_path "bindTo" method_handle_sig (method_sig [object_sig] (Some method_handle_sig));
			| Some(ctx_class,vl) ->
				let f () =
					let tl = List.map (fun v ->
						let _,load,_ = self#get_local v in
						load();
						self#vtype v.v_type
					) vl in
					let offset = pool#add_field ctx_class#get_path "<init>" (ctx_class#get_constructor_sig) FKMethod in
					tl,offset
				in
				self#construct rvalue_any ctx_class#get_path ctx_class#get_jsig f;
				jm#invokevirtual method_handle_path "bindTo" method_handle_sig (method_sig [object_sig] (Some method_handle_sig));
			end
		| TArrayDecl el when ret = RVoid ->
			List.iter (self#texpr ret) el
		| TArrayDecl el ->
			let length = List.length el in
			begin match follow e.etype with
			| TInst({cl_path = ([],"Array")},[t]) ->
				code#iconst (Int32.of_int length);
				ignore(self#new_native_array (jsignature_of_type (self#mknull t)) el);
				jm#invokestatic ([],"Array") "ofNative" (method_sig [array_sig object_sig] (Some (object_path_sig ([],"Array"))));
				self#cast e.etype
			| _ ->
				assert false
			end
		| TArray(e1,e2) when ret = RVoid ->
			(* Array access never throws so this should be fine... *)
			self#texpr ret e1;
			self#texpr ret e2;
		| TArray(e1,e2) ->
			begin match follow e1.etype with
			| TInst({cl_path = ([],"Array")} as c,[t]) ->
				let cf_get = PMap.find "__get" c.cl_fields in
				let t = self#mknull t in
				let ef = mk (TField(e1,FInstance(c,[t],cf_get))) (apply_params c.cl_params [t] cf_get.cf_type) e.epos in
				self#call rvalue_any t ef [e2];
				self#cast e.etype;
			| TInst({cl_path = (["java"],"NativeArray")},_) ->
				self#texpr rvalue_any e1;
				let vt = self#vtype e1.etype in
				let vte = self#vtype e.etype in
				self#texpr rvalue_any e2;
				self#read_native_array vt vte
			| t ->
				self#texpr rvalue_any e1;
				self#texpr rvalue_any e2;
				jm#invokestatic (["haxe";"jvm"],"Jvm") "arrayRead" (method_sig [object_sig;TInt] (Some object_sig));
				self#cast e.etype;
			end
		| TBlock [] ->
			if ret = RReturn && not jm#is_terminated then code#return_void;
		| TBlock el ->
			let rec loop el = match el with
				| [] -> assert false
				| [e1] ->
					self#texpr (if ret = RReturn then RVoid else ret) e1;
					if ret = RReturn && not jm#is_terminated then code#return_void;
				| e1 :: el ->
					self#texpr RVoid e1;
					loop el
			in
			let pop_scope = jm#push_scope in
			loop el;
			pop_scope();
		| TCast(e1,None) ->
			self#texpr ret e1;
			self#cast e.etype
		| TCast(e1,Some mt) ->
			self#texpr rvalue_any e1;
			let jsig = jsignature_of_type (type_of_module_type mt) in
			(* TODO: I think this needs some instanceof checking for basic types *)
			if NativeSignatures.is_unboxed jsig then jm#cast jsig
			else code#checkcast (path_map (t_infos mt).mt_path);
			if ret = RVoid then code#pop;
		| TParenthesis e1 | TMeta(_,e1) ->
			self#texpr ret e1
		| TFor(v,e1,e2) ->
			self#texpr ret (Texpr.for_remap com.basic v e1 e2 e.epos)
		| TEnumIndex e1 ->
			self#texpr rvalue_any e1;
			jm#getfield haxe_enum_path "_hx_index" TInt
		| TEnumParameter(e1,ef,i) ->
			self#texpr rvalue_any e1;
			let path,name,jsig_arg = match follow ef.ef_type with
				| TFun(tl,TEnum(en,_)) ->
					let n,_,t = List.nth tl i in
					en.e_path,n,self#vtype t
				| _ -> assert false
			in
			let cpath = ((fst path),Printf.sprintf "%s$%s" (snd path) ef.ef_name) in
			let jsig = (object_path_sig cpath) in
			jm#cast jsig;
			jm#getfield cpath name jsig_arg;
			self#cast e.etype;
		| TThrow e1 ->
			self#texpr rvalue_any e1;
			let exc = new haxe_exception gctx e1.etype in
			if not (List.exists (fun exc' -> exc#is_assignable_to exc') caught_exceptions) then jm#add_thrown_exception exc#get_native_exception_path;
			if not exc#is_native_exception then begin
				let vt = self#vtype (self#mknull e1.etype) in
				self#throw vt
			end else begin
				code#athrow;
				jm#set_terminated true
			end
		| TObjectDecl fl ->
			begin match follow e.etype with
			| TAnon an ->
				let path,fl' = TAnonIdentifiaction.identify gctx an.a_fields in
				jm#construct path (fun () ->
					(* We have to respect declaration order, so let's temp var where necessary *)
					let rec loop fl fl' ok acc = match fl,fl' with
						| ((name,_,_),e) :: fl,(name',jsig) :: fl' ->
							if ok && name = name' then begin
								self#texpr rvalue_any e;
								jm#cast jsig;
								loop fl fl' ok acc
							end else begin
								let load = match (Texpr.skip e).eexpr with
								| TConst _ | TTypeExpr _ | TFunction _ ->
									(fun () -> self#texpr rvalue_any e)
								| _ ->
									let _,load,save = jm#add_local (Printf.sprintf "_hx_tmp_%s" name) (self#vtype e.etype) VarWillInit in
									self#texpr rvalue_any e;
									save();
									load
								in
								loop fl fl' false ((name,load) :: acc)
							end
						| [],[] ->
							acc
						| (_,e) :: fl,[] ->
							self#texpr RVoid e;
							loop fl fl' ok acc
						| [],(_,jsig) :: fl' ->
							jm#load_default_value jsig;
							loop [] fl' ok acc
					in
					let vars = loop fl fl' true [] in
					let vars = List.sort (fun (name1,_) (name2,_) -> compare name1 name2) vars in
					List.iter (fun (_,load) ->
						load();
					) vars;
					List.map snd fl';
				)
			| _ ->
				let f () =
					let offset = pool#add_field haxe_dynamic_object_path "<init>" (method_sig [] None) FKMethod in
					[],offset
				in
				self#construct ret haxe_dynamic_object_path object_sig f;
				List.iter (fun ((name,_,_),e) ->
					code#dup;
					self#string name;
					self#texpr rvalue_any e;
					self#expect_reference_type;
					jm#invokevirtual haxe_dynamic_object_path "_hx_setField" haxe_dynamic_object_sig (method_sig [string_sig;object_sig] None);
				) fl;
			end
		| TIdent _ ->
			Error.error (s_expr_ast false "" (s_type (print_context())) e) e.epos;

	(* api *)

	method object_constructor =
		let path = jc#get_super_path in
		let offset = pool#add_field path "<init>" (method_sig [] None) FKMethod in
		code#aload jc#get_jsig 0;
		code#invokespecial offset jc#get_jsig [] [];
		jm#set_this_initialized
end

let generate_expr gctx jc jm e is_main is_method mtype =
	let e,args,tr = match e.eexpr with
		| TFunction tf when is_method ->
			tf.tf_expr,tf.tf_args,tf.tf_type
		| _ ->
			e,[],t_dynamic
	in
	let handler = new texpr_to_jvm gctx jc jm tr in
	if is_main then ignore(jm#add_local "args" (TArray(string_sig,None)) VarArgument);
	List.iter (fun (v,_) ->
		ignore(handler#add_local v VarArgument);
	) args;
	jm#finalize_arguments;
	begin match mtype with
	| MConstructorTop ->
		handler#object_constructor
	| _ ->
		()
	end;
	handler#texpr RReturn e

let generate_method gctx jc c mtype cf =
	try
		let jsig = if cf.cf_name = "main" then
			method_sig [array_sig string_sig] None
		else
			jsignature_of_type cf.cf_type
		in
		let flags = [MPublic] in
		let flags = if c.cl_interface then MAbstract :: flags else flags in
		let flags = if mtype = MStatic then MethodAccessFlags.MStatic :: flags else flags in
		let flags = if has_class_field_flag cf CfFinal then MFinal :: flags else flags in
		let jm = jc#spawn_method cf.cf_name jsig flags in
		begin match cf.cf_expr with
		| None -> ()
		| Some e ->
			generate_expr gctx jc jm e (cf.cf_name = "main") true mtype;
		end;
		begin match cf.cf_params with
			| [] when c.cl_params = [] ->
				()
			| _ ->
				let stl = String.concat "" (List.map (fun (n,_) ->
					Printf.sprintf "%s:Ljava/lang/Object;" n
				) cf.cf_params) in
				let ssig = generate_method_signature true (jsignature_of_type cf.cf_type) in
				let s = if cf.cf_params = [] then ssig else Printf.sprintf "<%s>%s" stl ssig in
				let offset = jc#get_pool#add_string s in
				jm#add_attribute (AttributeSignature offset);
		end;
	with Failure s | HarderFailure s ->
		failwith (Printf.sprintf "%s\nMethod %s.%s" s (s_type_path c.cl_path) cf.cf_name)

let generate_field gctx jc c mtype cf =
	let jsig = jsignature_of_type cf.cf_type in
	let flags = [MPublic] in
	let flags = if c.cl_interface then MAbstract :: flags else flags in
	let flags = if mtype = MStatic then MethodAccessFlags.MStatic :: flags else flags in
	let jm = jc#spawn_field cf.cf_name jsig flags in
	begin match cf.cf_expr with
		| None ->
			()
		| Some e ->
			let default () =
				let p = null_pos in
				let efield = Texpr.Builder.make_static_field c cf p in
				let eop = mk (TBinop(OpAssign,efield,e)) cf.cf_type p in
				begin match c.cl_init with
				| None -> c.cl_init <- Some eop
				| Some e -> c.cl_init <- Some (concat e eop)
				end
			in
			match e.eexpr with
			| TConst ct ->
				begin match ct with
				| TInt i32 when not (is_nullable cf.cf_type) ->
					let offset = jc#get_pool#add (ConstInt i32) in
					jm#add_attribute (AttributeConstantValue offset);
				| TString s ->
					let offset = jc#get_pool#add_const_string s in
					jm#add_attribute (AttributeConstantValue offset);
				| _ ->
					default();
				end
			| _ ->
				default();
	end;
	let ssig = generate_signature true (jsignature_of_type cf.cf_type) in
	let offset = jc#get_pool#add_string ssig in
	jm#add_attribute (AttributeSignature offset)

type super_situation =
	| SuperNo
	| SuperNoConstructor
	| SuperGood of jsignature

let generate_class gctx c =
	let is_annotation = Meta.has Meta.Annotation c.cl_meta in
	let path_super,sig_super_ctor = match c.cl_super with
		| None -> object_path,SuperNo
		| Some(c,_) -> path_map c.cl_path,match c.cl_constructor with
			| Some cf ->
				SuperGood (jsignature_of_type cf.cf_type)
			| None ->
				SuperNoConstructor
	in
	let jc = new JvmClass.builder (path_map c.cl_path) path_super in
	let pool = jc#get_pool in
	let field mtype cf = match cf.cf_kind with
		| Method (MethNormal | MethInline) ->
			List.iter (fun cf ->
				generate_method gctx jc c mtype cf
			) (cf :: cf.cf_overloads)
		| _ ->
			if not c.cl_interface then generate_field gctx jc c mtype cf
	in
	List.iter (field MStatic) c.cl_ordered_statics;
	List.iter (field MInstance) c.cl_ordered_fields;
	begin match c.cl_constructor,sig_super_ctor with
		| Some cf,SuperGood _ -> field MConstructor cf
		| Some cf,_ -> field MConstructorTop cf
		| None,_ ->
			let jm = jc#spawn_method "<init>" (method_sig [] None) [MPublic] in
			let handler = new texpr_to_jvm gctx jc jm t_dynamic in
			handler#object_constructor;
			jm#get_code#return_void;
	end;
	begin match c.cl_init with
		| None ->
			()
		| Some e ->
			let cf = mk_field "<clinit>" (tfun [] gctx.com.basic.tvoid) null_pos null_pos in
			cf.cf_kind <- Method MethNormal;
			let tf = {
				tf_args = [];
				tf_type = gctx.com.basic.tvoid;
				tf_expr = mk_block e;
			} in
			let e = mk (TFunction tf) cf.cf_type null_pos in
			cf.cf_expr <- Some e;
			field MStatic cf
	end;
	List.iter (fun (c,_) ->
		if is_annotation && c.cl_path = (["java";"lang";"annotation"],"Annotation") then
			()
		else
			jc#add_interface c.cl_path
	) c.cl_implements;
	jc#add_attribute (AttributeSourceFile (pool#add_string c.cl_pos.pfile));
	begin match c.cl_params with
		| [] ->
			()
		| _ ->
			let stl = String.concat "" (List.map (fun (n,_) ->
				Printf.sprintf "%s:Ljava/lang/Object;" n
			) c.cl_params) in
			let ssuper = match c.cl_super with
				| Some(c,tl) -> generate_method_signature true (jsignature_of_type (TInst(c,tl)))
				| None -> generate_method_signature true object_sig
			in
			let s = Printf.sprintf "<%s>%s" stl ssuper in
			let offset = pool#add_string s in
			jc#add_attribute (AttributeSignature offset)
	end;
	jc#add_access_flag 1; (* public *)
	if c.cl_final then jc#add_access_flag 0x10;
	if c.cl_interface then begin
		jc#add_access_flag 0x200;
		jc#add_access_flag 0x400;
	end;
	if is_annotation then begin
		jc#add_access_flag 0x2000;
		jc#add_interface (["java";"lang";"annotation"],"Annotation");
		(* TODO: this should be done via Haxe metadata instead of hardcoding it here *)
		jc#add_annotation retention_path ["value",(AEnum(retention_policy_sig,"RUNTIME"))];
	end;
	jc#add_annotation (["haxe";"jvm";"annotation"],"ClassReflectionInformation") (["hasSuperClass",(ABool (c.cl_super <> None))]);
	let jc = jc#export_class in
	write_class gctx.jar (path_map c.cl_path) jc

let generate_enum gctx en =
	let jc_enum = new JvmClass.builder en.e_path haxe_enum_path in
	jc_enum#add_access_flag 0x1; (* public *)
	jc_enum#add_access_flag 0x400; (* abstract *)
	let jsig_enum_ctor = method_sig [TInt] None in
	(* Create base constructor *)
	begin
		let jm_ctor = jc_enum#spawn_method "<init>" jsig_enum_ctor [MProtected] in
		jm_ctor#load_this;
		let _,load,_ = jm_ctor#add_local "index" TInt VarArgument in
		load();
		jm_ctor#call_super_ctor jsig_enum_ctor;
		jm_ctor#get_code#return_void;
	end;
	let inits = DynArray.create () in
	let names = List.map (fun name ->
		let ef = PMap.find name en.e_constrs in
		let args = match follow ef.ef_type with
			| TFun(tl,_) -> List.map (fun (n,_,t) -> n,jsignature_of_type t) tl
			| _ -> []
		in
		let jsigs = List.map snd args in
		(* Create class for constructor *)
		let jc_ctor = begin
			let jc_ctor = jc_enum#spawn_inner_class None jc_enum#get_this_path (Some ef.ef_name) in
			jc_ctor#add_access_flag 0x10; (* final *)
			let jsig_method = method_sig jsigs None in
			let jm_ctor = jc_ctor#spawn_method "<init>" jsig_method [MPublic] in
			jm_ctor#load_this;
			jm_ctor#get_code#iconst (Int32.of_int ef.ef_index);
			jm_ctor#call_super_ctor jsig_enum_ctor;
			List.iter (fun (n,jsig) ->
				jm_ctor#add_argument_and_field n jsig
			) args;
			jm_ctor#get_code#return_void;
			jc_ctor#add_annotation (["haxe";"jvm";"annotation"],"EnumValueReflectionInformation") (["argumentNames",AArray (List.map (fun (name,_) -> AString name) args)]);
			jc_ctor
		end in
		write_class gctx.jar jc_ctor#get_this_path jc_ctor#export_class;
		begin match args with
			| [] ->
				(* Create static field for ctor without args *)
				let jm_static = jc_enum#spawn_field ef.ef_name jc_enum#get_jsig [MPublic;MStatic;MFinal] in
				DynArray.add inits (jm_static,jc_ctor);
			| _ ->
				(* Create static function for ctor with args *)
				let jsig_static = method_sig jsigs (Some jc_enum#get_jsig) in
				let jm_static = jc_enum#spawn_method ef.ef_name jsig_static [MPublic;MStatic] in
				jm_static#construct jc_ctor#get_this_path (fun () ->
					List.iter (fun (n,jsig) ->
						let _,load,_ = jm_static#add_local n jsig VarArgument in
						load();
					) args;
					jsigs;
				);
				jm_static#get_code#return_value jc_enum#get_jsig;
		end;
		AString name
	) en.e_names in
	(* Assign static fields for ctors without args *)
	if DynArray.length inits > 0 then begin
		let jm_clinit = jc_enum#spawn_method "<clinit>" (method_sig [] None) [MStatic] in
		DynArray.iter (fun (jm_static,jc_ctor) ->
			jm_clinit#construct jc_ctor#get_this_path (fun () -> []);
			jm_clinit#putstatic jc_enum#get_this_path jm_static#get_name jm_static#get_jsig;
		) inits;
		jm_clinit#get_code#return_void;
	end;
	jc_enum#add_annotation (["haxe";"jvm";"annotation"],"EnumReflectionInformation") (["constructorNames",AArray names]);
	write_class gctx.jar en.e_path jc_enum#export_class

let generate_abstract gctx a =
	let super_path = object_path in
	let jc = new JvmClass.builder a.a_path super_path in
	jc#add_access_flag 1; (* public *)
	let jc = jc#export_class in
	write_class gctx.jar a.a_path jc

let debug_path path = match path with
	(* | ([],"Main") | (["haxe";"jvm"],_) -> true *)
	| (["haxe";"lang"],_) -> false (* Old Haxe/Java stuff that's weird *)
	| _ -> true

let is_extern_abstract a = match a.a_impl with
	| Some {cl_extern = true} -> true
	| _ -> false

let generate_module_type ctx mt = match mt with
	| TClassDecl c when not c.cl_extern && debug_path c.cl_path -> generate_class ctx c
	| TEnumDecl en when not en.e_extern -> generate_enum ctx en
	| TAbstractDecl a when not (is_extern_abstract a) && Meta.has Meta.CoreType a.a_meta -> generate_abstract ctx a
	| _ -> ()

let generate com =
	mkdir_from_path com.file;
	let jar_name,manifest_suffix = match com.main_class with
		| Some path -> snd path,"\nMain-Class: " ^ (s_type_path path)
		| None -> "jar",""
	in
	let jar_name = if com.debug then jar_name ^ "-debug" else jar_name in
	let jar_path = Printf.sprintf "%s%s.jar" (add_trailing_slash com.file) jar_name in
	let gctx = {
		com = com;
		jar = Zip.open_out jar_path;
		t_exception = TInst(resolve_class com (["java";"lang"],"Exception"),[]);
		t_throwable = TInst(resolve_class com (["java";"lang"],"Throwable"),[]);
		anon_lut = Hashtbl.create 0;
		anon_num = 0;
	} in
	let manifest_content =
		"Manifest-Version: 1.0\n" ^
		"Created-By: Haxe (Haxe Foundation)" ^
		manifest_suffix ^
		"\n\n"
	in
	Zip.add_entry manifest_content gctx.jar "META-INF/MANIFEST.MF";
	List.iter (generate_module_type gctx) com.types;
	Hashtbl.iter (fun fields path ->
		let jc = new JvmClass.builder path haxe_dynamic_object_path in
		jc#add_access_flag 0x1;
		begin
			let jm_ctor = jc#spawn_method "<init>" (method_sig (List.map snd fields) None) [MPublic] in
			jm_ctor#load_this;
			List.iter (fun (name,jsig) ->
				jm_ctor#add_argument_and_field name jsig;
			) fields;
			jm_ctor#call_super_ctor (method_sig [] None);
			jm_ctor#get_code#return_void;
		end;
		begin
			let string_map_path = (["haxe";"ds"],"StringMap") in
			let string_map_sig = object_path_sig string_map_path in
			let jm_fields = jc#spawn_method "_hx_getKnownFields" (method_sig [] (Some string_map_sig)) [MProtected] in
			let _,load,save = jm_fields#add_local "tmp" string_map_sig VarWillInit in
			jm_fields#construct string_map_path (fun () -> []);
			save();
			List.iter (fun (name,jsig) ->
				load();
				let offset = jc#get_pool#add_const_string name in
				jm_fields#get_code#sconst (string_sig) offset;
				jm_fields#load_this;
				jm_fields#getfield jc#get_this_path name jsig;
				jm_fields#expect_reference_type;
				jm_fields#invokevirtual string_map_path "set" string_map_sig (method_sig [string_sig;object_sig] None);
			) fields;
			load();
			jm_fields#get_code#return_value string_map_sig
		end;
		write_class gctx.jar path jc#export_class
	) gctx.anon_lut;
	Zip.close_out gctx.jar