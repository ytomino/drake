pragma License (Unrestricted);
--  extended unit
package Ada.References.Wide_String is
   --  There are helpers for returning sliced wide string from a function.
   pragma Pure;

   type Constant_Reference_Type (
      Element : not null access constant Standard.Wide_String) is null record;
   pragma Suppress_Initialization (Constant_Reference_Type);

   type Reference_Type (
      Element : not null access Standard.Wide_String) is null record;
   pragma Suppress_Initialization (Reference_Type);

   package Slicing is new Generic_Slicing (
      Positive,
      Wide_Character,
      Standard.Wide_String);

end Ada.References.Wide_String;
