open JvmGlobals
open JvmData
open JvmAttribute
open JvmSignature
open JvmBuilder

(* High-level method builder. *)

type builder_api = {
	resolve_method : bool -> Globals.path -> string -> jvm_constant_pool_index;
}

type var_init_state =
	| VarArgument
	| VarWillInit
	| VarNeedDefault

class builder jc api name jsig = object(self)
	inherit base_builder
	val code = new JvmCode.builder jc#get_pool

	val mutable max_num_locals = 0
	val mutable debug_locals = []
	val mutable stack_frames = []
	val mutable exceptions = []
	val mutable argument_locals = []

	(* per-frame *)
	val mutable locals = []
	val mutable local_offset = 0
	val mutable terminated = false

	method push_scope =
		let old_locals = locals in
		let old_offset = local_offset in
		(fun () ->
			let delta = local_offset - old_offset in
			let fp_end = code#get_fp in
			let rec loop i l =
				if i = 0 then
					()
				else begin match l with
					| (fpo,name,t) :: l ->
						let fp = match !fpo with
							| None -> failwith ("Uninitialized local " ^ name);
							| Some fp -> fp
						in
						let ld = {
							ld_start_pc = fp;
							ld_length = fp_end - fp;
							ld_name_index = jc#get_pool#add_string name;
							ld_descriptor_index = jc#get_pool#add_string (generate_signature false t);
							ld_index = old_offset + i - 1;
						} in
						debug_locals <- ld :: debug_locals;
						loop (i - (signature_size t)) l
					| [] ->
						assert false
				end
			in
			loop delta locals;
			locals <- old_locals;
			local_offset <- old_offset;
		)

	method get_locals_for_stack_frame locals =
		List.map (fun (init,_,t) ->
			match !init with
			| None -> JvmVerificationTypeInfo.VTop
			| _ -> JvmVerificationTypeInfo.of_signature jc#get_pool t
		) locals

	method add_stack_frame =
		let locals = self#get_locals_for_stack_frame locals in
		let astack = List.map (JvmVerificationTypeInfo.of_signature jc#get_pool) (code#get_stack#get_stack) in
		let r = code#get_fp in
		let ff = (r,locals,astack) in
		(* If we already have a frame at the same position, overwrite it. This can happen in the case of nested branches. *)
		stack_frames <- (match stack_frames with
			| (r',_,_) :: stack_frames when r' = r -> ff :: stack_frames
			| _ -> ff :: stack_frames)

	method add_exception (exc : jvm_exception) =
		exceptions <- exc :: exceptions

	(* casting *)

	method expect_reference_type =
		let wrap_null jsig name =
			let path = (["java";"lang"],name) in
			let offset = api.resolve_method true path "valueOf" in
			code#invokestatic offset [jsig] [TObject(path,[])]
		in
		match code#get_stack#top with
		| TBool as t -> wrap_null t "Boolean"
		| TByte as t -> wrap_null t "Byte"
		| TShort as t -> wrap_null t "Short"
		| TInt as t -> wrap_null t "Integer"
		| TLong as t -> wrap_null t "Long"
		| TFloat as t -> wrap_null t "Float"
		| TDouble as t -> wrap_null t "Double"
		| _ -> ()

	method private expect_basic_type t =
		let unwrap_null tname name =
			let path = (["java";"lang"],tname) in
			let tp = TObject(path,[]) in
			let offset = api.resolve_method true (["haxe";"jvm"],"Jvm") name in
			code#invokestatic offset [tp] [t]
		in
		match t with
		| TBool -> unwrap_null "Boolean" "toBoolean"
		| TByte -> unwrap_null "Byte" "toByte"
		| TShort -> unwrap_null "Short" "toShort"
		| TInt -> unwrap_null "Integer" "toInt"
		| TLong -> unwrap_null "Long" "toLong"
		| TFloat -> unwrap_null "Float" "toFloat"
		| TDouble -> unwrap_null "Double" "toDouble"
		| _ -> ()

	method cast ?(allow_to_string=false) vt =
		let vt' = code#get_stack#top in
		begin match vt,vt' with
		| TObject((["java";"lang"],"Double"),_),TInt ->
			code#i2d;
			self#expect_reference_type;
		| TDouble,TInt ->
			code#i2d;
		| TInt,TDouble ->
			code#d2i;
		| TLong,TDouble ->
			code#d2l;
		| TDouble,TLong ->
			code#l2d;
		| TFloat,TDouble ->
			code#d2f;
		| TLong,TInt ->
			code#i2l;
		| TInt,TLong ->
			code#l2i;
		| TObject(path1,_),TObject(path2,_) when path1 = path2 ->
			()
		| TObject((["java";"lang"],"String"),_),_ when allow_to_string ->
			self#expect_reference_type;
			let offset = api.resolve_method true (["haxe";"jvm"],"Jvm") "toString" in
			code#invokestatic offset [code#get_stack#top] [vt]
		| TObject(path1,_),TObject(path2,_) ->
			code#checkcast path1;
		| TObject(path,_),TTypeParameter _ ->
			code#checkcast path
		| TMethod _,TMethod _ ->
			()
		| TMethod _,_ ->
			code#checkcast (["java";"lang";"invoke"],"MethodHandle")
		| t1,t2 ->
			match NativeSignatures.is_unboxed t1,NativeSignatures.is_unboxed t2 with
			| true,false -> self#expect_basic_type t1
			| false,true -> self#expect_reference_type
			| _ -> ()
		end

	(* branches *)

	method start_branch =
		let save = code#get_stack#save in
		let old_terminated = terminated in
		(fun () ->
			code#get_stack#restore save;
			let was_terminated = terminated in
			terminated <- old_terminated;
			was_terminated
		)

	method if_then_else f_if (f_then : unit -> unit) (f_else : unit -> unit) =
		let jump_then = f_if () in
		let restore = self#start_branch in
		let pop = self#push_scope in
		f_then();
		pop();
		let r_then = ref code#get_fp in
		if not self#is_terminated then code#goto r_then;
		jump_then := code#get_fp - !jump_then;
		let term1 = restore() in
		self#add_stack_frame;
		let pop = self#push_scope in
		f_else();
		pop();
		self#set_terminated (term1 && self#is_terminated);
		r_then := code#get_fp - !r_then;
		if not self#is_terminated then self#add_stack_frame

	method if_then f_if (f_then : unit -> unit) =
		let jump_then = f_if () in
		let restore = self#start_branch in
		let pop = self#push_scope in
		f_then();
		pop();
		ignore(restore());
		jump_then := code#get_fp - !jump_then;
		self#add_stack_frame

	method add_local (name : string) t (init_state : var_init_state) =
		let slot = local_offset in
		let load,store,d = match t with
			| TInt | TBool ->
				if init_state = VarNeedDefault then begin
					code#iconst Int32.zero;
					code#istore slot
				end;
				(fun () -> code#iload ~vt:t slot),(fun () -> code#istore slot),1
			| TLong ->
				if init_state = VarNeedDefault then begin
					code#lconst Int64.zero;
					code#lstore slot
				end;
				(fun () -> code#lload slot),(fun () -> code#lstore slot),2
			| TFloat ->
				if init_state = VarNeedDefault then begin
					code#fconst 0.;
					code#fstore slot
				end;
				(fun () -> code#fload slot),(fun () -> code#fstore slot),1
			| TDouble ->
				if init_state = VarNeedDefault then begin
					code#dconst 0.;
					code#dstore slot
				end;
				(fun () -> code#dload slot),(fun () -> code#dstore slot),2
			| _ ->
				if init_state = VarNeedDefault then begin
					code#aconst_null t;
					code#astore t slot
				end;
				(fun () -> code#aload t slot),(fun () -> code#astore t slot),1
		in
		let init = ref None in
		locals <- (init,name,t) :: locals;
		local_offset <- local_offset + d;
		if local_offset > max_num_locals then max_num_locals <- local_offset;
		let check_store =
			let did_store = ref false in
			(fun () ->
				if not !did_store then begin
					did_store := true;
					init := Some (code#get_fp)
				end
			)
		in
		begin match init_state with
		| VarArgument | VarNeedDefault -> check_store();
		| _ -> ()
		end;
		slot,
		load,
		(fun () ->
			check_store();
			store()
		)

	method finalize_arguments =
		argument_locals <- locals

	method get_stack_map_table =
		let argument_locals = self#get_locals_for_stack_frame argument_locals in
		let stack_map = List.fold_left (fun ((last_offset,last_locals,last_locals_length),acc) (offset,locals,stack) ->
			let cur = offset - last_offset - 1 in
			let a_locals = Array.of_list (List.rev locals) in
			let locals_length = Array.length a_locals in
			let default () =
				StackFull(cur,a_locals,Array.of_list (List.rev stack))
			in
			let entry = match stack,locals_length - last_locals_length with
			| [],0 ->
				if last_locals = locals then begin
					if cur < 64 then StackSame cur
					else StackSameExtended cur
				end else
					default()
			| [vt],0 ->
				if last_locals = locals then begin
					if cur < 64 then Stack1StackItem(cur,vt)
					else Stack1StackItemExtended(cur,vt)
				end else
					default()
			| [],1 ->
				begin match locals with
				| vt1 :: locals when locals = last_locals -> StackAppend1(cur,vt1)
				| _ -> default()
				end
			| [],2 ->
				begin match locals with
				| vt1 :: vt2 :: locals when locals = last_locals -> StackAppend2(cur,vt2,vt1)
				| _ -> default()
				end
			| [],3 ->
				begin match locals with
				| vt1 :: vt2 :: vt3 :: locals when locals = last_locals -> StackAppend3(cur,vt3,vt2,vt1)
				| _ -> default()
				end
			| [],-1 ->
				begin match last_locals with
				| _ :: last_locals when locals = last_locals -> StackChop1 cur
				| _ -> default()
				end
			| [],-2 ->
				begin match last_locals with
				| _ :: _ :: last_locals when locals = last_locals -> StackChop2 cur
				| _ -> default()
				end
			| [],-3 ->
				begin match last_locals with
				| _ :: _ :: _ :: last_locals when locals = last_locals -> StackChop3 cur
				| _ -> default()
				end
			| _ ->
				default()
			in
			((offset,locals,locals_length),entry :: acc)
		) ((-1,argument_locals,List.length argument_locals),[]) (List.rev stack_frames) in
		Array.of_list (List.rev (snd stack_map))

	method get_code = code
	method is_terminated = terminated
	method set_terminated b = terminated <- b

	method private get_jcode =
		let lines = AttributeLineNumberTable (DynArray.to_array code#get_lines) in
		let exceptions = Array.of_list (List.rev exceptions) in
		let attributes = [lines;AttributeStackMapTable self#get_stack_map_table] in
		let attributes = List.map (JvmAttribute.write_attribute jc#get_pool) attributes in
		{
			code_max_stack = code#get_max_stack_size;
			code_max_locals = max_num_locals;
			code_code = code#export_code;
			code_exceptions = exceptions;
			code_attributes = Array.of_list attributes;
		}

	method export_method =
		self#commit_annotations jc#get_pool;
		if code#get_fp > 0 then begin
			let code = self#get_jcode in
			self#add_attribute (AttributeCode code);
		end;
		begin match debug_locals with
		| [] ->
			()
		| _ ->
			let a = Array.of_list debug_locals in
			self#add_attribute (AttributeLocalVariableTable a);
		end;
		let attributes = self#export_attributes jc#get_pool in
		let offset_name = jc#get_pool#add_string (if name = "new" then "<init>" else name) in
		let jsig = generate_method_signature false jsig in
		let offset_desc = jc#get_pool#add_string jsig in
		{
			field_access_flags = access_flags;
			field_name_index = offset_name;
			field_descriptor_index = offset_desc;
			field_attributes = attributes;
		}

	method export_field =
		assert (code#get_fp = 0);
		if code#get_fp > 0 then begin
			let code = self#get_jcode in
			self#add_attribute (AttributeCode code);
		end;
		let attributes = self#export_attributes jc#get_pool in
		let offset_name = jc#get_pool#add_string name in
		let jsig = generate_signature false jsig in
		let offset_desc = jc#get_pool#add_string jsig in
		{
			field_access_flags = access_flags;
			field_name_index = offset_name;
			field_descriptor_index = offset_desc;
			field_attributes = attributes;
		}
end