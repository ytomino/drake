with Ada.Unchecked_Conversion;
with C.string;
function System.To_String (First : Address) return String is
   function Cast is new Ada.Unchecked_Conversion (Address, C.char_ptr);
   Result : String (1 .. Natural (C.string.strlen (Cast (First))));
   for Result'Address use First;
begin
   return Result;
end System.To_String;
