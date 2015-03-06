with Ada.Exceptions;
with Ada.Unchecked_Conversion;
with Interfaces.C.Strings;
with System.Address_To_Access_Conversions;
with System.Storage_Elements;
with System.Unwind.Foreign;
with C.unwind;
procedure exception_cpp is
	pragma Linker_Options ("-lstdc++");
	use type Interfaces.C.char_array;
	use type System.Storage_Elements.Storage_Offset;
	type type_info is record
		vtbl : System.Address;
		name : Interfaces.C.Strings.const_chars_ptr;
	end record;
	pragma Convention (Cpp, type_info);
	char_const_ptr : aliased constant type_info;
	pragma Import (Cpp, char_const_ptr, "_ZTIPKc");
	Message : aliased constant Interfaces.C.char_array := "This is a C++ exception!" & Interfaces.C.nul;
	package Conv is
		new System.Address_To_Access_Conversions (
			Interfaces.C.Strings.const_chars_ptr);
begin
	declare
		function cxa_allocate_exception (
			size : Interfaces.C.size_t)
			return System.Address;
		pragma Import (C, cxa_allocate_exception, "__cxa_allocate_exception");
		procedure cxa_throw (
			obj : System.Address;
			tinfo : access constant type_info;
			dest : System.Address);
		pragma Import (C, cxa_throw, "__cxa_throw");
		pragma No_Return (cxa_throw);
		Cpp_Exception : System.Address;
	begin
		Cpp_Exception := cxa_allocate_exception (Interfaces.C.Strings.const_chars_ptr'Max_Size_In_Storage_Elements);
		Conv.To_Pointer (Cpp_Exception).all := Interfaces.C.Strings.To_Const_Chars_ptr (Message'Access);
		cxa_throw (Cpp_Exception, char_const_ptr'Access, System.Null_Address);
	end;
exception
	when E : System.Unwind.Foreign.Foreign_Exception =>
		declare
			function To_Repr is
				new Ada.Unchecked_Conversion (
					Ada.Exceptions.Exception_Occurrence_Access,
					System.Unwind.Exception_Occurrence_Access);
			GCC_Exception : constant System.Address :=
				To_Repr (E'Unrestricted_Access).Machine_Occurrence;
			Cpp_Exception : constant System.Address :=
				GCC_Exception + C.unwind.struct_Unwind_Exception'Max_Size_In_Storage_Elements;
		begin
			Ada.Debug.Put (Interfaces.C.Strings.Value (Conv.To_Pointer (Cpp_Exception).all));
		end;
end exception_cpp;
