open JvmData

(* High-level class builder. *)

class base_builder = object(self)
	val mutable access_flags = 0
	val attributes = DynArray.create ()

	method add_access_flag i =
		access_flags <- i lor access_flags

	method add_attribute (a : JvmAttribute.j_attribute) =
		DynArray.add attributes a

	method export_attributes (pool : JvmConstantPool.constant_pool) =
		DynArray.to_array (DynArray.map (JvmAttribute.write_attribute pool) attributes)
end

class builder path_this path_super = object(self)
	inherit base_builder
	val pool = new JvmConstantPool.constant_pool
	val jsig = JvmSignature.TObject(path_this,[])
	val mutable offset_this = 0
	val mutable offset_super = 0
	val mutable offset_super_ctor = 0
	val mutable interface_offsets = []
	val fields = DynArray.create ()
	val methods = DynArray.create ()

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
	method set_offset_super_ctor offset = offset_super_ctor <- offset

	method export_class =
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