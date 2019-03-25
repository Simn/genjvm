open JvmGlobals
open JvmData
open JvmSignature
open JvmBuilder

(* High-level class builder. *)

class builder path_this path_super = object(self)
	inherit base_builder
	val pool = new JvmConstantPool.constant_pool
	val jsig = TObject(path_this,[])
	val mutable offset_this = 0
	val mutable offset_super = 0
	val mutable offset_super_ctor = 0
	val mutable interface_offsets = []
	val fields = DynArray.create ()
	val methods = DynArray.create ()
	val inner_classes = DynArray.create ()
	val mutable closure_count = 0

	method add_interface path =
		interface_offsets <- (pool#add_path path) :: interface_offsets

	method add_field (f : jvm_field) =
		DynArray.add fields f

	method add_method (m : jvm_field) =
		DynArray.add methods m

	method get_pool = pool

	method get_this_path = path_this
	method get_super_path = path_super
	method get_jsig = jsig
	method get_offset_super_ctor = offset_super_ctor
	method get_offset_this = offset_this
	method get_access_flags = access_flags

	method set_offset_super_ctor offset = offset_super_ctor <- offset

	method get_next_closure_name =
		let name = Printf.sprintf "hx_closure$%i" closure_count in
		closure_count <- closure_count + 1;
		name

	method spawn_inner_class path_super =
		let path = ([],Printf.sprintf "%s$%i" (snd path_this) (DynArray.length inner_classes)) in
		let jc = new builder path path_super in
		jc#add_access_flag 0x01;
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

	method export_class =
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