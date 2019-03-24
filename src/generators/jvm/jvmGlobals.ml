type jvm_constant_pool_index = int

(* Constants *)

type jvm_bootstrap_method_index = int

type jvm_constant =
	| ConstUtf8 of string
	| ConstInt of int32
	| ConstFloat of float
	| ConstLong of int64
	| ConstDouble of float
	| ConstClass of jvm_constant_pool_index
	| ConstString of jvm_constant_pool_index
	| ConstFieldref of jvm_constant_pool_index * jvm_constant_pool_index
	| ConstMethodref of jvm_constant_pool_index * jvm_constant_pool_index
	| ConstInterfaceMethodref of jvm_constant_pool_index * jvm_constant_pool_index
	| ConstNameAndType of jvm_constant_pool_index * jvm_constant_pool_index
	| ConstMethodHandle of int * jvm_constant_pool_index
	| ConstMethodType of jvm_constant_pool_index
	| ConstInvokeDynamic of jvm_bootstrap_method_index * jvm_constant_pool_index

type field_kind =
	| FKField
	| FKMethod
	| FKInterfaceMethod

type numeric_range =
	| Int8Range
	| Int16Range
	| Int32Range

let get_numeric_range i =
	if i >= -128 && i <= 127 then Int8Range
	else if i >= -32768 && i <= 32767 then Int16Range
	else Int32Range

let get_numeric_range_unsigned i =
	if i <= 0xFF then Int8Range
	else if i <= 0xFFFF then Int16Range
	else Int32Range

let in_range unsigned range i = match (if unsigned then get_numeric_range_unsigned else get_numeric_range) i,range with
	| Int8Range,(Int8Range | Int16Range | Int32Range) -> true
	| Int16Range,(Int16Range | Int32Range) -> true
	| Int32Range,Int32Range -> true
	| _ -> false

let jerror s =
	failwith s

let write_byte ch i = IO.write_byte ch i
let write_bytes ch b = IO.nwrite ch b
let write_ui16 ch i = IO.BigEndian.write_ui16 ch i
let write_ui32 ch i = IO.BigEndian.write_real_i32 ch (Int32.of_int i)
let write_string ch s = IO.nwrite_string ch s

let write_array16 ch f a =
	write_ui16 ch (Array.length a);
	Array.iter (f ch) a

let path_map path = match path with
	| [],"String" -> ["java";"lang"],"String"
	(* | [],"Bool" -> ["java";"lang"],"Boolean"
	| [],"Int" -> ["java";"lang"],"Integer"
	| [],"Float" -> ["java";"lang"],"Double" *)
	| ["java"],"NativeArray" -> ["java";"lang"],"Object"
	| _ -> path