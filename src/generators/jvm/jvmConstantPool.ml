open IO
open IO.BigEndian
open JvmGlobals

(* High-level constant pool *)

class constant_pool = object(self)
	val pool = DynArray.create ();
	val lut = Hashtbl.create 0;
	val luti = Hashtbl.create 0;
	val mutable next_index = 1;
	val mutable closed = false

	method add const =
		try
			Hashtbl.find lut const
		with Not_found ->
			assert (not closed);
			let i = next_index in
			next_index <- next_index + 1;
			DynArray.add pool const;
			Hashtbl.add lut const i;
			Hashtbl.add luti i (DynArray.length pool - 1);
			match const with
			| ConstDouble _ | ConstLong _ ->
				next_index <- next_index + 1;
				i
			| _ ->
				i

	method get i =
		DynArray.get pool (Hashtbl.find luti i)

	method private s_type_path (p,s) = match p with [] -> s | _ -> String.concat "/" p ^ "/" ^ s

	method add_path path =
		let path = path_map path in
		match path with
		| [],"Int" ->
			self#add_field (["java";"lang"],"Integer") "TYPE" "Ljava/lang/Class;" true
		| ["java"],"Int64" ->
			self#add_field (["java";"lang"],"Long") "TYPE" "Ljava/lang/Class;" true
		| _ ->
			let s = self#s_type_path path in
			let offset = self#add (ConstUtf8 s) in
			self#add (ConstClass offset);

	method add_string s =
		self#add (ConstUtf8 s)

	method add_const_string s =
		let offset = self#add_string s in
		self#add (ConstString offset)

	method add_field path name jsig is_field =
		let offset_class = self#add_path path in
		let offset_name = self#add_string name in
		let offset_desc = self#add_string jsig in
		let offset_info = self#add (ConstNameAndType(offset_name,offset_desc)) in
		let const = if is_field then
			(ConstFieldref(offset_class,offset_info))
		else
			(ConstMethodref(offset_class,offset_info))
		in
		self#add const

	method private write_utf8 ch s =
		String.iter (fun c ->
			let c = Char.code c in
			if c = 0 then begin
			write_byte ch 0xC0;
			write_byte ch 0x80
			end else
			write_byte ch c
		) s

	method private write_i64 ch i64 =
		write_real_i32 ch (Int64.to_int32 i64);
		write_real_i32 ch (Int64.to_int32 (Int64.shift_right_logical i64 32))

	method private write ch =
		write_ui16 ch next_index;
		DynArray.iter (function
			| ConstUtf8 s ->
				write_byte ch 1;
				write_ui16 ch (String.length s);
				self#write_utf8 ch s;
			| ConstInt i32 ->
				write_byte ch 3;
				write_real_i32 ch i32;
			| ConstFloat f ->
				write_byte ch 4;
				(match classify_float f with
				| FP_normal | FP_subnormal | FP_zero ->
					write_real_i32 ch (Int32.bits_of_float f)
				| FP_infinite when f > 0.0 ->
					write_real_i32 ch 0x7f800000l
				| FP_infinite ->
					write_real_i32 ch 0xff800000l
				| FP_nan ->
					write_real_i32 ch 0x7f800001l)
			| ConstLong i64 ->
				write_byte ch 5;
				write_i64 ch i64;
			| ConstDouble d ->
				write_byte ch 6;
				write_double ch d
			| ConstClass offset ->
				write_byte ch 7;
				write_ui16 ch offset;
			| ConstString offset ->
				write_byte ch 8;
				write_ui16 ch offset;
			| ConstFieldref (offset1,offset2) ->
				write_byte ch 9;
				write_ui16 ch offset1;
				write_ui16 ch offset2;
			| ConstMethodref (offset1,offset2) ->
				write_byte ch 10;
				write_ui16 ch offset1;
				write_ui16 ch offset2;
			| ConstInterfaceMethodref (offset1,offset2) ->
				write_byte ch 11;
				write_ui16 ch offset1;
				write_ui16 ch offset2;
			| ConstNameAndType (offset1,offset2) ->
				write_byte ch 12;
				write_ui16 ch offset1;
				write_ui16 ch offset2;
			| ConstMethodHandle (i,offset) ->
				write_byte ch 15;
				write_byte ch i;
				write_ui16 ch offset;
			| ConstMethodType offset ->
				write_byte ch 16;
				write_ui16 ch offset;
			| ConstInvokeDynamic (offset1,offset2) ->
				write_byte ch 18;
				write_ui16 ch offset1;
				write_ui16 ch offset2;
		) pool

	method close =
		closed <- true;
		let ch = IO.output_bytes () in
		self#write ch;
		IO.close_out ch
end