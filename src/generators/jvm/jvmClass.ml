open JvmGlobals
open JvmData
open JvmSignature
open JvmAttribute
open JvmBuilder

(* High-level class builder. *)

class builder path_this path_super = object(self)
	inherit base_builder
	val pool = new JvmConstantPool.constant_pool
	val jsig = TObject(path_this,[])
	val mutable offset_this = 0
	val mutable offset_super = 0
	val mutable interface_offsets = []
	val fields = DynArray.create ()
	val methods = DynArray.create ()
	val inner_classes = DynArray.create ()
	val mutable closure_count = 0
	val mutable bootstrap_methods = []
	val mutable num_bootstrap_methods = 0

	method add_interface path =
		interface_offsets <- (pool#add_path path) :: interface_offsets

	method add_field (f : jvm_field) =
		DynArray.add fields f

	method add_method (m : jvm_field) =
		DynArray.add methods m

	method get_bootstrap_method path name jsig (consts : jvm_constant list) =
		try
			fst (List.assoc (path,name) bootstrap_methods)
		with Not_found ->
			let offset = pool#add_field path name jsig FKMethod in
			let offset = pool#add (ConstMethodHandle(6, offset)) in
			let bm = {
				bm_method_ref = offset;
				bm_arguments = Array.of_list (List.map pool#add consts);
			} in
			bootstrap_methods <- ((path,name),(offset,bm)) :: bootstrap_methods;
			num_bootstrap_methods <- num_bootstrap_methods + 1;
			num_bootstrap_methods - 1

	method get_pool = pool

	method get_this_path = path_this
	method get_super_path = path_super
	method get_jsig = jsig
	method get_offset_this = offset_this
	method get_access_flags = access_flags

	method get_next_closure_name =
		let name = Printf.sprintf "hx_closure$%i" closure_count in
		closure_count <- closure_count + 1;
		name

	method spawn_inner_class (jm : JvmMethod.builder) path_super =
		let path = ([],Printf.sprintf "%s$%i" (snd path_this) (DynArray.length inner_classes)) in
		let jc = new builder path path_super in
		jc#add_access_flag 0x01;
		let _ =
			let pool = jc#get_pool in
			let offset_class = pool#add_path path_this in
			let offset_name = pool#add_string jm#get_name in
			let offset_desc = pool#add_string (generate_signature false jm#get_jsig) in
			let offset_info = pool#add (ConstNameAndType(offset_name,offset_desc)) in
			jc#add_attribute (JvmAttribute.AttributeEnclosingMethod(offset_class,offset_info));
		in
		let offset_name = pool#add_string (snd path) in
		let offset_class = pool#add (ConstClass offset_name) in
		DynArray.add inner_classes (jc,offset_name,offset_class);
		jc

	method private commit_inner_classes =
		if DynArray.length inner_classes > 0 then begin
			let open JvmAttribute in
			let a = DynArray.to_array inner_classes in
			let a = Array.map (fun (jc,offset_name,offset_class) -> {
				ic_inner_class_info_index = offset_class;
				ic_outer_class_info_index = self#get_offset_this;
				ic_inner_name_index = offset_name;
				ic_inner_class_access_flags = jc#get_access_flags;
			}) a in
			self#add_attribute (AttributeInnerClasses a)
		end

	method private commit_bootstrap_methods =
		match bootstrap_methods with
		| [] ->
			()
		| _ ->
			let l = List.fold_left (fun acc (_,(_,bm)) -> bm :: acc) [] bootstrap_methods in
			self#add_attribute (AttributeBootstrapMethods (Array.of_list l))

	method export_class =
		self#commit_bootstrap_methods;
		self#commit_inner_classes;
		self#commit_annotations pool;
		let attributes = self#export_attributes pool in
		let pool = pool#close in
		{
			class_minor_version = 0;
			class_major_version = 0x34;
			class_constant_pool = pool;
			class_access_flags = access_flags;
			class_this_class = offset_this;
			class_super_class = offset_super;
			class_interfaces = Array.of_list interface_offsets;
			class_fields = DynArray.to_array fields;
			class_methods = DynArray.to_array methods;
			class_attributes = attributes;
		}

	initializer
		offset_this <- pool#add_path path_this;
		offset_super <- pool#add_path path_super;
end