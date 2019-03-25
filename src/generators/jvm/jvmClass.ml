open JvmData

(* High-level class builder. *)

type annotation_const =
	| ACString of string
	(* TODO when I'm bored *)

type annotation_kind =
	| AConst of annotation_const
	| AEnum of JvmSignature.jsignature * string
	| AArray of annotation_kind list

and annotation = (string * annotation_kind) list

class base_builder = object(self)
	val mutable access_flags = 0
	val attributes = DynArray.create ()
	val annotations = DynArray.create ()

	method add_access_flag i =
		access_flags <- i lor access_flags

	method add_attribute (a : JvmAttribute.j_attribute) =
		DynArray.add attributes a

	method add_annotation (jsig : JvmSignature.jsignature) (a : annotation) =
		DynArray.add annotations (jsig,a)

	method private commit_annotations pool =
		if DynArray.length annotations > 0 then begin
			let open JvmAttribute in
			let a = DynArray.to_array annotations in
			let a = Array.map (fun (jsig,l) ->
				let offset = pool#add_string (JvmSignature.generate_signature false jsig) in
				let l = List.map (fun (name,ak) ->
					let offset = pool#add_string name in
					let rec loop ak = match ak with
						| AConst c ->
							begin match c with
							| ACString(s) -> 's',ValConst(pool#add_string s)
							end
						| AEnum(jsig,name) ->
							'e',ValEnum(pool#add_string (JvmSignature.generate_signature false jsig),pool#add_string name)
						| AArray l ->
							let l = List.map (fun ak -> loop ak) l in
							'[',ValArray(Array.of_list l)
					in
					offset,loop ak
				) l in
				{
					ann_type = offset;
					ann_elements = Array.of_list l;
				}
			) a in
			self#add_attribute (AttributeRuntimeVisibleAnnotations a)
		end

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