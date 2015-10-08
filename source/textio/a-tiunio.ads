pragma License (Unrestricted);
--  with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded_Strings;
with Ada.Text_IO.Generic_Unbounded_IO;
package Ada.Text_IO.Unbounded_IO is
   new Generic_Unbounded_IO (
      Strings.Unbounded_Strings,
      Put => Put,
      Put_Line => Put_Line,
      Get_Line => Get_Line);
