open Globals
open Ast
open Common
open Type
open Path
open JvmGlobals
open JvmData
open JvmAttribute
open JvmSignature
open JvmMethod

(* Haxe *)

type generation_context = {
	com : Common.context;
	jar : Zip.out_file;
	t_exception : Type.t;
	t_throwable : Type.t;
}

type ret =
	| RValue
	| RVoid
	| RReturn

type method_type =
	| MStatic
	| MInstance
	| MConstructor
	| MConstructorTop
	| MMain

type access_kind =
	| AKPost
	| AKPre
	| AKNone

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
			| _ -> TObject(a.a_path,List.map jtype_argument_of_type tl)
		end
	| TDynamic t' when t' == t_dynamic -> object_sig
	| TDynamic _ -> assert false (* TODO: hmm... *)
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
		let t = get_boxed_type (jsignature_of_type t) in (* TODO: ? *)
		TArray(t,None)
	| TInst({cl_kind = KTypeParameter _; cl_path = (_,name)},_) -> TTypeParameter name
	| TInst(c,tl) -> TObject(c.cl_path,List.map jtype_argument_of_type tl)
	| TEnum(en,tl) -> TObject(en.e_path,List.map jtype_argument_of_type tl)
	| TFun(tl,tr) -> TMethod(List.map (fun (_,_,t) -> jsignature_of_type t) tl,if ExtType.is_void (follow tr) then None else Some (jsignature_of_type tr))
	| TAnon an -> object_sig
	| TType(td,tl) -> jsignature_of_type (apply_params td.t_params tl td.t_type)
	| TLazy f -> jsignature_of_type (lazy_type f)

and jtype_argument_of_type t =
	TType(WNone,jsignature_of_type t)

let enum_ctor_sig =
	let ta = TArray(object_sig,None) in
	(TMethod([TInt;ta],None))

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

let resolve_method com static path name =
	let c = resolve_class com path in
	try
		c,PMap.find name (if static then c.cl_statics else c.cl_fields)
	with Not_found ->
		jerror (Printf.sprintf "No such field: %s.%s" (s_type_path path) name)

let add_field pool c cf =
	let field_kind =
		if c.cl_interface then
			FKInterfaceMethod
		else match cf.cf_kind with
		| Method (MethNormal | MethInline) ->
			FKMethod
		| _ ->
			FKField
	in
	let t =
		(if field_kind = FKField then generate_signature else generate_method_signature) false (jsignature_of_type cf.cf_type)
	in
	pool#add_field (path_map c.cl_path) (if cf.cf_name = "new" then "<init>" else cf.cf_name) t field_kind

let make_resolve_api com jc =
	let api = {
		resolve_method = (fun static path name ->
			let c,cf = resolve_method com static path name in
			add_field jc#get_pool c cf
		)
	} in
	api

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

class texpr_to_jvm gctx (jc : JvmClass.builder) (jm : JvmMethod.builder) (return_type : Type.t) = object(self)
	val com = gctx.com
	val code = jm#get_code
	val pool = jc#get_pool

	val mutable local_lookup = Hashtbl.create 0;
	val mutable last_line = 0

	val mutable breaks = []
	val mutable continue = 0


	method vtype t =
		jsignature_of_type t

	method mknull t = com.basic.tnull (follow t)

	method add_haxe_field is_static path name =
		let c,cf = resolve_method com is_static path name in
		add_field pool c cf

	(* locals *)

	method add_local v init_state : (int * (unit -> unit) * (unit -> unit)) =
		let t = self#vtype v.v_type in
		let slot,load,store = jm#add_local v.v_name t init_state in
		Hashtbl.add local_lookup v.v_id (slot,load,store);
		slot,load,store

	method get_local v =
		try Hashtbl.find local_lookup v.v_id
		with Not_found -> failwith ("Unbound local " ^ v.v_name)

	(* casting *)

	method expect_reference_type = jm#expect_reference_type

	method cast t =
		if follow t != t_dynamic then begin
			let vt = self#vtype t in
			jm#cast vt
		end else
			self#expect_reference_type

	(* access *)

	method read_static_closure name =
		let c',cf' = resolve_method com true haxe_jvm_path "readField" in
		let offset = add_field pool c' cf' in
		let t = code#get_stack#top in
		self#string name;
		code#invokestatic offset [t;self#vtype com.basic.tstring] [self#vtype t_dynamic];

	method read t e1 fa =
		match fa with
		| FStatic(c,({cf_kind = Method (MethNormal | MethInline)} as cf)) ->
			self#texpr RValue e1;
			self#read_static_closure cf.cf_name;
			self#cast cf.cf_type;
		| FStatic(c,cf) ->
			let offset = add_field pool c cf in
			let t = self#vtype cf.cf_type in
			code#getstatic offset t
		| FInstance({cl_path = ([],"String")},_,{cf_name = "length"}) ->
			let offset = pool#add_field string_path "length" "()I" FKMethod in
			self#texpr RValue e1;
			let vtobj = self#vtype e1.etype in
			code#invokevirtual offset vtobj [] [TInt]
		| FInstance({cl_path = (["java"],"NativeArray")},_,{cf_name = "length"}) ->
			self#texpr RValue e1;
			let vtobj = self#vtype e1.etype in
			code#arraylength vtobj
		| FInstance(c,tl,cf) ->
			let vt = self#vtype cf.cf_type in
			let offset = add_field pool c cf in
			self#texpr RValue e1;
			let vtobj = self#vtype e1.etype in
			code#getfield offset vtobj vt
		| FEnum(en,ef) ->
			let path = en.e_path in
			let offset = pool#add_path path in
			code#new_ (TObject(path,[])) offset;
			code#dup;
			code#iconst (Int32.of_int ef.ef_index);
			code#iconst Int32.zero;
			let jasig,jsig = self#new_native_array object_sig [] in
			let offset_field = pool#add_field path "<init>" (generate_method_signature false enum_ctor_sig) FKMethod in
			code#invokespecial offset_field (TObject(path,[])) [TInt;jasig] []
		| FDynamic s | FAnon {cf_name = s} ->
			let c,cf = resolve_method com true haxe_jvm_path "readField" in
			let offset = add_field pool c cf in
			self#texpr RValue e1;
			self#string s;
			code#invokestatic offset [self#vtype e1.etype;self#vtype com.basic.tstring] [self#vtype t_dynamic];
			self#cast t;
		| FClosure(_,cf) ->
			let offset = self#add_haxe_field true haxe_jvm_path "bindMethod" in
			self#texpr RValue e1;
			self#string cf.cf_name;
			self#string (generate_method_signature false (self#vtype cf.cf_type));
			code#invokestatic offset [self#vtype e1.etype;string_sig;string_sig] [method_handle_sig]

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
		| TField(e1,FInstance(c,tl,cf)) ->
			let vt = self#vtype t in
			let offset = add_field pool c cf in
			let vtobj = self#vtype e1.etype in
			self#texpr RValue e1;
			if ak <> AKNone then begin
				code#dup;
				code#getfield offset vtobj vt;
			end;
			apply (fun () -> code#dup_x1);
			code#putfield offset vtobj vt
		| TField(e1,(FDynamic s | FAnon {cf_name = s})) ->
			let c,cf_write = resolve_method com true haxe_jvm_path "writeField" in
			let offset_write = add_field pool c cf_write in
			let c,cf_read = resolve_method com true haxe_jvm_path "readField" in
			let offset_read = add_field pool c cf_read in
			self#texpr RValue e1;
			if ak <> AKNone then code#dup;
			self#string s;
			if ak <> AKNone then begin
				code#dup_x1;
				code#invokestatic offset_read [self#vtype e1.etype;self#vtype com.basic.tstring] [self#vtype t_dynamic];
				self#cast e.etype;
			end;
			apply (fun () -> code#dup_x2);
			self#cast (self#mknull e.etype);
			code#invokestatic offset_write [self#vtype e1.etype;self#vtype com.basic.tstring;code#get_stack#top] []
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
					self#texpr RValue e1;
					if ak <> AKNone then code#dup;
					self#texpr RValue e2;
					if ak <> AKNone then begin
						code#dup_x1;
						code#invokevirtual offset_get vta [TInt] [vte];
						self#cast e.etype;
					end;
					apply (fun () -> code#dup_x2;);
					self#cast t;
					code#invokevirtual offset_set vta [TInt;vte] []
				| TInst({cl_path = (["java"],"NativeArray")},_) ->
					let t = self#mknull t in
					let vte = self#vtype t in
					let vta = self#vtype e1.etype in
					self#texpr RValue e1;
					if ak <> AKNone then code#dup;
					self#texpr RValue e2;
					if ak <> AKNone then begin
						code#dup_x1;
						code#aaload vta vte;
					end;
					apply (fun () -> code#dup_x2);
					self#cast t;
					code#aastore vta vte
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
			self#texpr RValue e;
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
		self#texpr RValue e1;
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
		end else
			code#lookupswitch offset_def (DynArray.to_array flat_cases);
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
			ignore(restore());
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
		| TLong,_ | _,TLong ->
			Some TLong
		| TDouble,_ | _,TDouble ->
			Some TDouble
		| TInt,_ | _,TInt ->
			Some TInt
		| _ ->
			None

	method do_compare op =
		match code#get_stack#get_stack_items 2 with
		| [TInt | TByte | TChar | TBool;TInt | TByte | TChar | TBool] ->
			(fun () -> code#if_icmp_ref op)
		| [t2;TObject((["java";"lang"],"String"),[]) as t1] ->
			(* TODO: We need a slow compare if java.lang.Object is involved because it could refer to String *)
			(fun () ->
				let offset = pool#add_field string_path "equals" "(Ljava/lang/Object;)Z" FKMethod in
				code#invokevirtual offset t1 [t2] [TBool];
				code#if_ref (flip_cmp_op op)
			)
		| [TObject((["java";"lang"],"String"),[]) as t1;t2] ->
			(fun () ->
				let offset = pool#add_field string_path "equals" "(Ljava/lang/Object;)Z" FKMethod in
				code#swap;
				code#invokevirtual offset t1 [t2] [TBool];
				code#if_ref (flip_cmp_op op)
			)
		| [TObject((["java";"lang"],"Object"),[]) | TTypeParameter _ as t2;t1]
		| [t1;TObject((["java";"lang"],"Object"),[]) | TTypeParameter _ as t2] ->
			(fun () ->
				let c,cf = resolve_method com true haxe_jvm_path "equals" in
				let offset = add_field pool c cf in
				code#invokestatic offset [t1;t2] [TBool];
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
			self#texpr RValue e1;
			(if op = CmpNe then self#if_not_null else self#if_null) (self#vtype e1.etype);
		| {eexpr = TConst (TInt i32);etype = t2},e1 when Int32.to_int i32 = 0 ->
			self#texpr RValue e1;
			self#cast t2;
			(fun () -> code#if_ref op)
		| e1,{eexpr = TConst (TInt i32); etype = t2;} when Int32.to_int i32 = 0 ->
			self#texpr RValue e1;
			self#cast t2;
			(fun () -> code#if_ref op)
		| _ ->
			let f e () = self#texpr RValue e in
			self#binop_exprs (self#get_binop_type e1.etype e2.etype) (f e1) (f e2);
			self#do_compare op

	method binop_numeric (opi,opd,opl) cast_type f1 f2 =
		self#binop_exprs cast_type f1 f2;
		begin match code#get_stack#get_stack_items 2 with
		| [TInt;TInt] -> opi ()
		| [TDouble;TDouble] -> opd ()
		| [TLong;TLong] -> opl ()
		| [TObject((["java";"lang"],"String"),[]);TObject((["java";"lang"],"String"),[]);] ->
			let c,cf = resolve_method com true (["haxe";"jvm"],"Jvm") "stringConcat" in
			let offset = add_field jc#get_pool c cf in
			code#invokestatic offset [string_sig;string_sig] [string_sig]
		| [t1;t2] -> jerror (Printf.sprintf "Can't numop %s and %s" (generate_signature false t1) (generate_signature false t2))
		| tl -> jerror (Printf.sprintf "Bad stack: %s" (String.concat ", " (List.map (generate_signature false) tl)));
		end

	method binop_basic ret op cast_type f1 f2 = match op with
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
		| OpShl -> self#binop_numeric ((fun () -> code#ishl),(fun () -> assert false),(fun () -> code#lshl)) cast_type f1 f2
		| OpShr -> self#binop_numeric ((fun () -> code#ishr),(fun () -> assert false),(fun () -> code#lshr)) cast_type f1 f2
		| OpUShr -> self#binop_numeric ((fun () -> code#iushr),(fun () -> assert false),(fun () -> code#lushr)) cast_type f1 f2
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
				self#texpr RValue e2;
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
					self#binop_basic ret op (self#get_binop_type e1.etype e2.etype) (fun () -> ()) (fun () -> self#texpr RValue e2);
					if is_null e1.etype then self#expect_reference_type;
				in
				self#read_write ret AKPre e1 f t
			end
		| _ ->
			let f e () = self#texpr RValue e in
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
			self#texpr RValue e;
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
			self#texpr RValue e;
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
						| TFun(tl,_) ->
							let rec loop2 tl' tl = match tl',tl with
								| t' :: tl',(_,_,t) :: tl ->
									(try Type.unify t' t; loop2 tl' tl with _ -> loop cfl)
								| [],[] ->
									rcf := cf;
									tl
								| _ ->
									loop cfl
							in
							loop2 tl' tl
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
				self#texpr RValue e;
				self#cast t;
				loop (self#vtype t :: acc) tl el
			| _,[] -> List.rev acc
			| [],e :: el ->
				(* TODO: this sucks *)
				self#texpr RValue e;
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
					self#texpr RValue e1;
					self#expect_reference_type;
					code#instanceof (path_map (t_infos mt).mt_path);
					Some TBool
				| _ -> Error.error "Type expression expected" e1.epos
			end;
		| TField(_,FStatic(c,({cf_kind = Method (MethNormal | MethInline)} as cf))) ->
			let offset = add_field pool c cf in
			let tl,tr = self#call_arguments cf.cf_type el in
			code#invokestatic offset tl (retype tr);
			tr
		| TField(e1,FInstance(c,tl,({cf_kind = Method (MethNormal | MethInline)} as cf))) ->
			let offset = add_field pool c cf in
			let is_super = match e1.eexpr with
			| TConst TSuper ->
				code#aload jc#get_jsig 0;
				true
			| _ ->
				self#texpr RValue e1;
				false
			in
			let tl,tr = self#call_arguments cf.cf_type el in
			let t1 = self#vtype e1.etype in
			(if is_super then code#invokespecial else if c.cl_interface then code#invokeinterface else code#invokevirtual) offset t1 tl (retype tr);
			tr
		| TField(_,FEnum(en,ef)) ->
			let path = en.e_path in
			let offset = pool#add_path path in
			code#new_ (TObject(path,[])) offset;
			code#dup;
			code#iconst (Int32.of_int ef.ef_index);
			code#iconst (Int32.of_int (List.length el));
			let jasig,jsig = self#new_native_array object_sig el in
			let offset_field = pool#add_field path "<init>" (generate_method_signature false enum_ctor_sig) FKMethod in
			code#invokespecial offset_field (TObject(path,[])) [TInt;jasig] [];
			Some (TObject(path,[]))
		| TConst TSuper ->
			code#aload jc#get_jsig 0;
			begin match follow e1.etype with
			| TInst(c,_) ->
				begin match c.cl_constructor with
				| None ->
					jerror (Printf.sprintf "%s does not have a constructor" (s_type_path c.cl_path));
				| Some cf ->
					let hack_cf = ref cf in
					let tl,_ = self#call_arguments ~hack:(Some hack_cf) cf.cf_type el in
					let offset = add_field pool c !hack_cf in
					code#invokespecial offset jc#get_jsig tl [];
					None
				end;
			| _ ->
				assert false
			end
		| TIdent "__array__" ->
			begin match follow tr with
			| TInst({cl_path = (["java"],"NativeArray")},[t]) ->
				code#iconst (Int32.of_int (List.length el));
				let jasig,_ = self#new_native_array (self#vtype (self#mknull t)) el in
				Some jasig
			| _ ->
				assert false
			end
		| _ ->
			self#texpr RValue e1;
			code#checkcast method_handle_path;
			let tl,tr = self#call_arguments e1.etype el in
			let offset = pool#add_field method_handle_path "invoke" (generate_method_signature false (TMethod(tl,tr))) FKMethod in
			code#invokevirtual offset (self#vtype e1.etype) tl (retype tr);
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
		let c,cf = resolve_method com true (["haxe";"jvm"],"Exception") "wrap" in
		let offset = add_field pool c cf in
		code#invokestatic offset [vt] [exception_sig];
		code#athrow;
		jm#set_terminated true

	method try_catch ret e1 catches =
		let restore = jm#start_branch in
		let fp_from = code#get_fp in
		self#texpr ret e1;
		let term_try = jm#is_terminated in
		let r_try = self#maybe_make_jump in
		let fp_to = code#get_fp in
		let unwrap () =
			code#dup;
			code#instanceof haxe_exception_path;
			jm#if_then_else
				(fun () -> code#if_ref CmpEq)
				(fun () ->
					code#checkcast haxe_exception_path;
					let c,cf = resolve_method com false (["haxe";"jvm"],"Exception") "value" in
					let offset = add_field pool c cf in
					code#getfield offset haxe_exception_sig object_sig;
				)
				(fun () -> code#checkcast object_path);
		in
		let start_exception_block path jsig =
			ignore(restore());
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
			unwrap();
			let restore = jm#start_branch in
			let rl = ref [] in
			let rec loop excl = match excl with
				| [] ->
					(* TODO: this re-wraps which we should avoid. Either have to store the original expression in a local
					   or keep it in the stack (but in that case we have to pop it in the individual cases). *)
					self#throw throwable_sig;
				| (_,v,e) :: excl ->
					code#dup;
					let path = match self#vtype (self#mknull v.v_type) with TObject(path,_) -> path | _ -> assert false in
					code#instanceof path;
					jm#if_then_else
						(fun () -> code#if_ref CmpNe)
						(fun () ->
							ignore(restore());
							self#cast v.v_type;
							let term = run_catch_expr v e in
							rl := (term,ref 0) :: !rl;
						)
						(fun () -> loop excl)
			in
			loop excl;
			!rl
		in
		let excl = List.map (fun (v,e) -> new haxe_exception gctx v.v_type,v,e) catches in
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

	method new_native_array jsig el =
		let path = match jsig with
			| TObject(path,_) -> path
			| TTypeParameter name -> object_path
			| TMethod _ -> method_handle_path
			| _ ->
				print_endline (generate_signature true jsig);
				assert false
		in
		let offset = pool#add_path path in
		let jasig = TArray(jsig,None) in
		code#anewarray jasig offset;
		let init f =
			List.iteri (fun i e ->
				code#dup;
				code#iconst (Int32.of_int i);
				self#texpr RValue e;
				jm#cast jsig;
				f();
			) el
		in
		init (fun () -> code#aastore jasig jsig);
		jasig,jsig

	method construct ret path t f =
		let offset_class = pool#add_path (path_map path) in
		code#new_ t offset_class;
		if ret <> RVoid then code#dup;
		let tl,offset = f() in
		code#invokespecial offset t tl []

	method type_expr path =
		let basic_path name =
			let offset = pool#add_field (["java";"lang"],name) "TYPE" "Ljava/lang/Class;" FKField in
			code#getstatic offset java_class_sig
		in
		match path with
		| [],"Void" ->
			basic_path "Void"
		| [],"Int" ->
			basic_path "Integer"
		| [],"Float" ->
			basic_path "Double"
		| [],"Bool" ->
			basic_path "Boolean"
		(* TODO: other types *)
		| _ ->
			let path = path_map path in
			let offset = pool#add_path path in
			let t = TObject(path,[]) in
			code#ldc offset (TObject(java_class_path,[TType(WNone,t)]))

	method texpr ret e =
		try
			if not jm#is_terminated then self#texpr' ret e
		with Failure s ->
			failwith (Printf.sprintf "Expr %s\n%s" (s_expr_pretty false "" false (s_type (print_context())) e) s)

	method texpr' ret e =
		code#set_line (Lexer.get_error_line e.epos);
		match e.eexpr with
		| TVar(v,Some e1) ->
			self#texpr RValue e1;
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
			self#type_expr (t_infos mt).mt_path
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
			ignore(restore());
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
			self#texpr RValue e1;
			(* Technically this could throw... but whatever *)
			if ret <> RVoid then ignore(self#new_native_array (jsignature_of_type (self#mknull t)) [])
		| TNew(c,tl,el) ->
			begin match c.cl_constructor with
			| None ->
				jerror (Printf.sprintf "%s does not have a constructor" (s_type_path c.cl_path));
			| Some cf ->
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
			self#texpr RValue e1;
			self#cast return_type;
			let vt = self#vtype return_type in
			code#return_value vt;
			jm#set_terminated true;
		| TFunction tf ->
			let t = tfun (List.map (fun (v,_) -> v.v_type) tf.tf_args) tf.tf_type in
			let name = jc#get_next_closure_name in
			let jm = new JvmMethod.builder jc (make_resolve_api com jc) name (self#vtype t) in
			jm#add_access_flag 0x1;
			jm#add_access_flag 0x8;
			let handler = new texpr_to_jvm gctx jc jm tf.tf_type in
			List.iter (fun (v,_) ->
				ignore(handler#add_local v VarArgument);
			) tf.tf_args;
			jm#finalize_arguments;
			handler#texpr RReturn tf.tf_expr;
			jc#add_method jm#export_method;
			self#type_expr jc#get_this_path;
			self#read_static_closure name
		| TArrayDecl el when ret = RVoid ->
			List.iter (self#texpr ret) el
		| TArrayDecl el ->
			let length = List.length el in
			begin match follow e.etype with
			| TInst({cl_path = ([],"Array")},[t]) ->
				code#iconst (Int32.of_int length);
				let jasig,jsig = self#new_native_array (jsignature_of_type (self#mknull t)) el in
				let offset_field = pool#add_field ([],"Array") "ofNative" (Printf.sprintf "([Ljava/lang/Object;)LArray;") FKMethod in
				let vta = self#vtype e.etype in
				code#invokestatic offset_field [jasig] [vta]
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
				self#call RValue t ef [e2];
				self#cast e.etype;
			| TInst({cl_path = (["java"],"NativeArray")},_) ->
				self#texpr RValue e1;
				let vt = self#vtype e1.etype in
				let vte = self#vtype (self#mknull e.etype) in
				self#texpr RValue e2;
				code#aaload vt vte
			| t ->
				Error.error (s_type (print_context()) t) e.epos;
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
			self#texpr RValue e1;
			(* self#expect_reference_type; *)
			code#checkcast (path_map (t_infos mt).mt_path);
			if ret = RVoid then code#pop;
		| TParenthesis e1 | TMeta(_,e1) ->
			self#texpr ret e1
		| TFor(v,e1,e2) ->
			self#texpr ret (Texpr.for_remap com.basic v e1 e2 e.epos)
		| TEnumIndex e1 ->
			self#texpr RValue e1;
			let vtobj = self#vtype e1.etype in
			let path = match vtobj with TObject(path,_) -> path | _ -> assert false in
			let offset = pool#add_field path "index" "I" FKField in
			code#getfield offset vtobj TInt
		| TEnumParameter(e1,_,i) ->
			self#texpr RValue e1;
			let vtobj = self#vtype e1.etype in
			let path = match vtobj with TObject(path,_) -> path | _ -> assert false in
			let offset = pool#add_field path "parameters" "[Ljava/lang/Object;" FKField in
			let tobj = object_sig in
			let ta = TArray(tobj,None) in
			code#getfield offset vtobj ta;
			code#iconst (Int32.of_int i);
			code#aaload ta tobj;
			self#cast e.etype;
		| TThrow e1 ->
			self#texpr RValue e1;
			let vt = self#vtype (self#mknull e1.etype) in
			self#throw vt
		| TObjectDecl fl ->
			let f () =
				let offset = pool#add_field haxe_dynamic_object_path "<init>" "()V" FKMethod in
				[],offset
			in
			self#construct ret haxe_dynamic_object_path object_sig f;
			let c,cf = resolve_method com false (["haxe";"jvm"],"DynamicObject") "_hx_setField" in
			let offset = add_field pool c cf in
			List.iter (fun ((name,_,_),e) ->
				code#dup;
				self#string name;
				self#texpr RValue e;
				self#expect_reference_type;
				code#invokevirtual offset haxe_dynamic_object_sig [string_sig;self#vtype (self#mknull e.etype)] [];
			) fl;
		| TIdent _ ->
			Error.error (s_expr_ast false "" (s_type (print_context())) e) e.epos;

	(* api *)

	method object_constructor =
		let path = object_path in
		let offset = pool#add_field path "<init>" "()V" FKMethod in
		code#aload jc#get_jsig 0;
		code#invokespecial offset jc#get_jsig [] []
end

let generate_expr gctx jc jm e is_main is_method mtype =
	let e,args,tr = match e.eexpr with
		| TFunction tf when is_method ->
			tf.tf_expr,tf.tf_args,tf.tf_type
		| _ ->
			e,[],t_dynamic
	in
	let handler = new texpr_to_jvm gctx jc jm tr in
	if (mtype <> MStatic) then ignore(jm#add_local "this" jc#get_jsig VarArgument);
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
		let api = make_resolve_api gctx.com jc in
		let jsig = if cf.cf_name = "main" then
			TMethod([TArray(string_sig,None)],None)
		else
			jsignature_of_type cf.cf_type
		in
		let jm = new JvmMethod.builder jc api cf.cf_name jsig in
		let close_scope = jm#push_scope in
		jm#add_access_flag 1; (* public *)
		if c.cl_interface then jm#add_access_flag 0x0400; (* abstact *)
		if mtype = MStatic then jm#add_access_flag 0x8;
		if (has_class_field_flag cf CfFinal) then jm#add_access_flag 0x10;
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
		close_scope();
		jm#export_method
	with Failure s ->
		failwith (Printf.sprintf "Method %s.%s:\n%s" (s_type_path c.cl_path) cf.cf_name s)

let generate_field gctx jc c mtype cf =
	let api = make_resolve_api gctx.com jc in
	let jsig = jsignature_of_type cf.cf_type in
	let jm = new JvmMethod.builder jc api cf.cf_name jsig in
	jm#add_access_flag 1; (* public *)
	if c.cl_interface then jm#add_access_flag 0x0400; (* abstact *)
	if mtype = MStatic then jm#add_access_flag 0x8;
	if has_class_field_flag cf CfFinal then jm#add_access_flag 0x10;
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
	jm#add_attribute (AttributeSignature offset);
	jm#export_field

let generate_class gctx c =
	let path_super,sig_super_ctor = match c.cl_super with
		| None -> (object_path),"()V"
		| Some(c,_) -> path_map c.cl_path,match c.cl_constructor with
			| Some cf ->
				generate_method_signature true (jsignature_of_type cf.cf_type)
			| None ->
				"()V"
	in
	let jc = new JvmClass.builder (path_map c.cl_path) path_super in
	let pool = jc#get_pool in
	let offset_super_constructor = pool#add_field path_super "<init>" sig_super_ctor FKMethod in
	jc#set_offset_super_ctor offset_super_constructor;
	let field mtype cf = match cf.cf_kind with
		| Method (MethNormal | MethInline) ->
			List.iter (fun cf ->
				jc#add_method (generate_method gctx jc c mtype cf)
			) (cf :: cf.cf_overloads)
		| _ ->
			jc#add_field (generate_field gctx jc c mtype cf)
	in
	List.iter (field MStatic) c.cl_ordered_statics;
	List.iter (field MInstance) c.cl_ordered_fields;
	Option.may (field (if c.cl_super = None then MConstructorTop else MConstructor)) c.cl_constructor;
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
	List.iter (fun (c,_) -> jc#add_interface c.cl_path) c.cl_implements;
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
	if Meta.has Meta.Annotation c.cl_meta then begin
		jc#add_access_flag 0x2000;
		jc#add_interface (["java";"lang";"annotation"],"Annotation");
		(* TODO: this should be done via Haxe metadata instead of hardcoding it here *)
		jc#add_annotation retention_sig ["value",(JvmClass.AEnum(retention_policy_sig,"RUNTIME"))];
	end;
	let jc = jc#export_class in
	write_class gctx.jar (path_map c.cl_path) jc

let generate_enum gctx en =
	let super_path = (["haxe";"jvm"],"Enum") in
	let jc = new JvmClass.builder en.e_path super_path in
	jc#add_access_flag 1; (* public *)
	let c = resolve_class gctx.com super_path in
	let cf = Option.get c.cl_constructor in
	let offset_field = add_field jc#get_pool c cf in
	let api = make_resolve_api gctx.com jc in
	let jm = new JvmMethod.builder jc api "<init>" enum_ctor_sig in
	jm#add_access_flag 1; (* public *)
	let _,load0,_ = jm#add_local "this" jc#get_jsig VarArgument in
	let _,load1,_ = jm#add_local "index" TInt VarArgument in
	let _,load2,_ = jm#add_local "parameters" enum_ctor_sig VarArgument in
	load0();
	load1();
	load2();
	jm#get_code#invokespecial offset_field jc#get_jsig [TInt;enum_ctor_sig] [];
	jm#get_code#return_void;
	jc#add_method jm#export_method;
	let names = List.map (fun name -> JvmClass.AConst (ACString name)) en.e_names in
	jc#add_annotation (TObject((["haxe";"jvm";"annotation"],"EnumReflectionInformation"),[])) (["constructorNames",AArray names]);
	let jc = jc#export_class in
	write_class gctx.jar en.e_path jc

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
	| TEnumDecl en -> generate_enum ctx en
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
	} in
	let manifest_content =
		"Manifest-Version: 1.0\n" ^
		"Created-By: Haxe (Haxe Foundation)" ^
		manifest_suffix ^
		"\n\n"
	in
	Zip.add_entry manifest_content gctx.jar "META-INF/MANIFEST.MF";
	List.iter (generate_module_type gctx) com.types;
	Zip.close_out gctx.jar