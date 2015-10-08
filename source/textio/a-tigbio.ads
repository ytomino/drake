pragma License (Unrestricted);
--  generalized unit of Ada.Text_IO.Bounded_IO
with Ada.Strings.Generic_Bounded;
generic
   with package Bounded_Strings is new Strings.Generic_Bounded (<>);
   with package Bounded is new Bounded_Strings.Generic_Bounded_Length (<>);
   with procedure Put (
      File : File_Type;
      Item : Bounded_Strings.String_Type) is <>;
   with procedure Put_Line (
      File : File_Type;
      Item : Bounded_Strings.String_Type) is <>;
   with procedure Get_Line (
      File : File_Type;
      Item : out Bounded_Strings.String_Type;
      Last : out Natural) is <>;
package Ada.Text_IO.Generic_Bounded_IO is

   procedure Put (
      File : File_Type; -- Output_File_Type
      Item : Bounded.Bounded_String);

   procedure Put (
      Item : Bounded.Bounded_String);

   procedure Put_Line (
      File : File_Type; -- Output_File_Type
      Item : Bounded.Bounded_String);

   procedure Put_Line (
      Item : Bounded.Bounded_String);

   function Get_Line (
      File : File_Type) -- Input_File_Type
      return Bounded.Bounded_String;

   function Get_Line
      return Bounded.Bounded_String;

   procedure Get_Line (
      File : File_Type; -- Input_File_Type
      Item : out Bounded.Bounded_String);

   procedure Get_Line (
      Item : out Bounded.Bounded_String);

end Ada.Text_IO.Generic_Bounded_IO;
