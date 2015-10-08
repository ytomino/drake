pragma License (Unrestricted);
--  generalized unit of Ada.Text_IO.Unbounded_IO
with Ada.Strings.Generic_Unbounded;
generic
   with package Unbounded_Strings is new Strings.Generic_Unbounded (<>);
   with procedure Put (
      File : File_Type;
      Item : Unbounded_Strings.String_Type) is <>;
   with procedure Put_Line (
      File : File_Type;
      Item : Unbounded_Strings.String_Type) is <>;
   with procedure Get_Line (
      File : File_Type;
      Item : out Unbounded_Strings.String_Type;
      Last : out Natural) is <>;
package Ada.Text_IO.Generic_Unbounded_IO is

   procedure Put (
      File : File_Type; -- Output_File_Type
      Item : Unbounded_Strings.Unbounded_String);

   procedure Put (
      Item : Unbounded_Strings.Unbounded_String);

   procedure Put_Line (
      File : File_Type; -- Output_File_Type
      Item : Unbounded_Strings.Unbounded_String);

   procedure Put_Line (
      Item : Unbounded_Strings.Unbounded_String);

   function Get_Line (
      File : File_Type) -- Input_File_Type
      return Unbounded_Strings.Unbounded_String;

   function Get_Line
      return Unbounded_Strings.Unbounded_String;

   procedure Get_Line (
      File : File_Type; -- Input_File_Type
      Item : out Unbounded_Strings.Unbounded_String);

   procedure Get_Line (
      Item : out Unbounded_Strings.Unbounded_String);

end Ada.Text_IO.Generic_Unbounded_IO;
