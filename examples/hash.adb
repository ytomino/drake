with Ada.Containers;
with Ada.Strings.Hash;
with Ada.Strings.Wide_Hash;
with Ada.Strings.Wide_Wide_Hash;
procedure hash is
	procedure printf (f : String; d : Ada.Containers.Hash_Type);
	pragma Import (C, printf);
begin
	printf ("%.8x" & ASCII.LF & ASCII.NUL, Ada.Strings.Hash ("abcdefg"));
	printf ("%.8x" & ASCII.LF & ASCII.NUL, Ada.Strings.Hash ("ab"));
	printf ("%.8x" & ASCII.LF & ASCII.NUL, Ada.Strings.Hash ("ba"));
	printf ("%.8x" & ASCII.LF & ASCII.NUL, Ada.Strings.Hash ("----------"));
	printf ("%.8x" & ASCII.LF & ASCII.NUL, Ada.Strings.Hash ("-----------"));
	printf ("%.8x" & ASCII.LF & ASCII.NUL, Ada.Strings.Hash ("------------"));
end hash;
