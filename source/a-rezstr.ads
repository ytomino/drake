pragma License (Unrestricted);
--  extended unit
package Ada.References.Wide_Wide_String is
   --  There are helpers for returning sliced wide wide string from a function.
   pragma Pure;

   type Constant_Reference_Type (
      Element : not null access constant Standard.Wide_Wide_String) is
      null record
      with Implicit_Dereference => Element;
   pragma Suppress_Initialization (Constant_Reference_Type);

   type Reference_Type (
      Element : not null access Standard.Wide_Wide_String) is null record
      with Implicit_Dereference => Element;
   pragma Suppress_Initialization (Reference_Type);

   package Slicing is new Generic_Slicing (
      Positive,
      Wide_Wide_Character,
      Standard.Wide_Wide_String);

end Ada.References.Wide_Wide_String;
