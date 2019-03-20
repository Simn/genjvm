open JvmGlobals

type jpath = (string list) * string

type jwildcard =
	| WExtends (* + *)
	| WSuper (* -  *)
	| WNone

type jtype_argument =
	| TType of jwildcard * jsignature
	| TAny (* * *)

and jsignature =
	| TByte (* B *)
	| TChar (* C *)
	| TDouble (* D *)
	| TFloat (* F *)
	| TInt (* I *)
	| TLong (* J *)
	| TShort (* S *)
	| TBool (* Z *)
	| TObject of jpath * jtype_argument list (* L Classname *)
	| TObjectInner of (string list) * (string * jtype_argument list) list (* L Classname ClassTypeSignatureSuffix *)
	| TArray of jsignature * int option (* [ *)
	| TMethod of jmethod_signature (* ( *)
	| TTypeParameter of string (* T *)

(* ( jsignature list ) ReturnDescriptor (| V | jsignature) *)
and jmethod_signature = jsignature list * jsignature option

let encode_path (pack,name) =
	String.concat "/" (pack @ [name])

let rec write_param full ch ch param = match param with
	| TAny -> write_byte ch (Char.code '*')
	| TType(w, s) ->
		begin match w with
			| WExtends -> write_byte ch (Char.code '+')
			| WSuper -> write_byte ch (Char.code '-')
			| WNone -> ()
		end;
		write_signature full ch s

and write_signature full ch jsig = match jsig with
	| TByte -> write_byte ch (Char.code 'B')
	| TChar -> write_byte ch (Char.code 'C')
	| TDouble -> write_byte ch (Char.code 'D')
	| TFloat -> write_byte ch (Char.code 'F')
	| TInt -> write_byte ch (Char.code 'I')
	| TLong -> write_byte ch (Char.code 'J')
	| TShort -> write_byte ch (Char.code 'S')
	| TBool -> write_byte ch (Char.code 'Z')
	| TObject(path, params) ->
		write_byte ch (Char.code 'L');
		write_string ch (encode_path path);
		if params <> [] && full then begin
			write_byte ch (Char.code '<');
			List.iter (write_param full ch ch) params;
			write_byte ch (Char.code '>')
		end;
		write_byte ch (Char.code ';')
	| TObjectInner(pack, inners) ->
		write_byte ch (Char.code 'L');
		List.iter (fun p ->
			write_string ch p;
			write_byte ch (Char.code '/')
		) pack;
		let first = ref true in
		List.iter (fun (name,params) ->
			(if !first then first := false else write_byte ch (Char.code '.'));
			write_string ch name;
			if params <> [] then begin
				write_byte ch (Char.code '<');
				List.iter (write_param full ch ch) params;
				write_byte ch (Char.code '>')
			end;
		) inners;
		write_byte ch (Char.code ';')
	| TArray(s,size) ->
		write_byte ch (Char.code '[');
		begin match size with
			| Some size ->
				write_string ch (string_of_int size);
			| None -> ()
		end;
		write_signature full ch s
	| TMethod _ ->
		write_signature full ch (TObject((["java";"lang";"invoke"],"MethodHandle"),[]))
	| TTypeParameter name ->
		if full then begin
			write_byte ch (Char.code 'T');
			write_string ch name;
			write_byte ch (Char.code ';')
		end else
			write_string ch "Ljava/lang/Object;"

let generate_signature full jsig =
	let ch = IO.output_bytes () in
	write_signature full ch jsig;
	Bytes.unsafe_to_string (IO.close_out ch)

let generate_method_signature full jsig =
	let ch = IO.output_bytes () in
	begin match jsig with
	| TMethod(args, ret) ->
		write_byte ch (Char.code '(');
		List.iter (write_signature full ch) args;
		write_byte ch (Char.code ')');
		begin match ret with
			| None -> write_byte ch (Char.code 'V')
			| Some jsig -> write_signature full ch jsig
		end
	| _ ->
		write_signature full ch jsig;
	end;
	Bytes.unsafe_to_string (IO.close_out ch)

let signature_size = function
	| TDouble | TLong -> 2
	| _ -> 1

module NativeSignatures = struct
	let object_path = ["java";"lang"],"Object"
	let object_sig = TObject(object_path,[])

	let string_path = ["java";"lang"],"String"
	let string_sig = TObject(string_path,[])

	let boolean_path = ["java";"lang"],"Boolean"
	let boolean_sig = TObject(boolean_path,[])

	let character_path = ["java";"lang"],"Character"
	let character_sig = TObject(character_path,[])

	let method_handle_path = (["java";"lang";"invoke"],"MethodHandle")
	let method_handle_sig = TObject(method_handle_path,[])

	let java_class_path = ["java";"lang"],"Class"
	let java_class_sig = TObject(java_class_path,[TType(WNone,object_sig)])

	let haxe_jvm_path = ["haxe";"jvm"],"Jvm"

	let haxe_dynamic_object_path = ["haxe";"jvm"],"DynamicObject"
	let haxe_dynamic_object_sig = TObject(haxe_dynamic_object_path,[])

	let throwable_path = (["java";"lang"],"Throwable")
	let throwable_sig = TObject(throwable_path,[])

	let exception_path = (["java";"lang"],"Exception")
	let exception_sig = TObject(exception_path,[])

	(* numeric *)

	let byte_path = ["java";"lang"],"Byte"
	let byte_sig = TObject(byte_path,[])

	let short_path = ["java";"lang"],"Short"
	let short_sig = TObject(short_path,[])

	let integer_path = ["java";"lang"],"Integer"
	let integer_sig = TObject(integer_path,[])

	let long_path = ["java";"lang"],"Long"
	let long_sig = TObject(long_path,[])

	let float_path = ["java";"lang"],"Float"
	let float_sig = TObject(float_path,[])

	let double_path = ["java";"lang"],"Double"
	let double_sig = TObject(double_path,[])

	let get_boxed_type jsig = match jsig with
		| TBool -> boolean_sig
		| TChar -> character_sig
		| TByte -> byte_sig
		| TShort -> short_sig
		| TInt -> integer_sig
		| TLong -> long_sig
		| TFloat -> float_sig
		| TDouble -> double_sig
		| _ -> jsig

	let is_unboxed jsig = match jsig with
		| TBool | TChar
		| TByte | TShort | TInt | TLong
		| TFloat | TDouble ->
			true
		| _ ->
			false
end