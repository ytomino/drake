pragma License (Unrestricted);
--  extended unit
package Ada.References.Strings is
   --  There are helpers for returning sliced string from a function.
   pragma Pure;

   type Constant_Reference_Type (
      Element : not null access constant Standard.String) is null record
      with Implicit_Dereference => Element;
   pragma Suppress_Initialization (Constant_Reference_Type);

   type Reference_Type (
      Element : not null access Standard.String) is null record
      with Implicit_Dereference => Element;
   pragma Suppress_Initialization (Reference_Type);

   package Slicing is new Generic_Slicing (
      Positive,
      Character,
      Standard.String);

end Ada.References.Strings;
