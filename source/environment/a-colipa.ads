pragma License (Unrestricted);
--  extended unit
with Ada.Command_Line.Generic_Parsing;
package Ada.Command_Line.Parsing is
   new Generic_Parsing (
      Input_Cursor => Natural,
      Has_Element => Has_Element,
      Input_Iterator_Interfaces => Iterator_Interfaces,
      Input_Iterator => Concrete_Iterator'(null record), -- Iterate
      Argument => Argument);
--  Command line parser.
