pragma License (Unrestricted);
with Ada.Strings.Bounded;
with Ada.Strings.Bounded_Strings;
with Ada.Text_IO.Generic_Bounded_IO;
generic
   with package Bounded is new Strings.Bounded.Generic_Bounded_Length (<>);
package Ada.Text_IO.Bounded_IO is

   --  for renaming
   package Bounded_Strings_IO is
      new Generic_Bounded_IO (
         Strings.Bounded_Strings,
         Bounded.Bounded_Strings,
         Put => Put,
         Put_Line => Put_Line,
         Get_Line => Get_Line);

   procedure Put (
      File : File_Type; -- Output_File_Type
      Item : Bounded.Bounded_String)
      renames Bounded_Strings_IO.Put;

   procedure Put (
      Item : Bounded.Bounded_String)
      renames Bounded_Strings_IO.Put;

   procedure Put_Line (
      File : File_Type; -- Output_File_Type
      Item : Bounded.Bounded_String)
      renames Bounded_Strings_IO.Put_Line;

   procedure Put_Line (
      Item : Bounded.Bounded_String)
      renames Bounded_Strings_IO.Put_Line;

   function Get_Line (
      File : File_Type) -- Input_File_Type
      return Bounded.Bounded_String
      renames Bounded_Strings_IO.Get_Line;

   function Get_Line
      return Bounded.Bounded_String
      renames Bounded_Strings_IO.Get_Line;

   procedure Get_Line (
      File : File_Type; -- Input_File_Type
      Item : out Bounded.Bounded_String)
      renames Bounded_Strings_IO.Get_Line;

   procedure Get_Line (
      Item : out Bounded.Bounded_String)
      renames Bounded_Strings_IO.Get_Line;

end Ada.Text_IO.Bounded_IO;
