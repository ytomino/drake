with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded_Strings;
with System;
procedure exception5 is
	
	procedure Magic is
		S : aliased Ada.Strings.Unbounded.Unbounded_String;
	begin
		Ada.Strings.Unbounded.Append (S, "12345678");
		Ada.Strings.Unbounded.Append (S, "12345678");
		Ada.Strings.Unbounded.Append (S, "1234567812345678");
		Ada.Strings.Unbounded.Append (S, "9");
		Ada.Debug.Put (Ada.Strings.Unbounded_Strings.Reference (S'Access).Element.all);
	end Magic;
	
	procedure Test is
	begin
		begin
			Magic;
			raise Program_Error; -- reuse memory used by Magic.S
		exception
			when E : Program_Error =>
				Ada.Debug.Put (Ada.Exceptions.Exception_Information (E));
		end;
		Ada.Debug.Put ("OK");
	end Test;

begin
	Test;
end exception5;
