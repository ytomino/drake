pragma License (Unrestricted);
with Ada.Strings.Bounded;
generic
   with package Bounded is new Strings.Bounded.Generic_Bounded_Length (<>);
package Ada.Text_IO.Bounded_IO is

   procedure Put (
      File : File_Type;
      Item : Bounded.Bounded_String);

   procedure Put (
      Item : Bounded.Bounded_String);

   procedure Put_Line (
      File : File_Type;
      Item : Bounded.Bounded_String);

   procedure Put_Line (
      Item : Bounded.Bounded_String);

   function Get_Line (
      File : File_Type)
      return Bounded.Bounded_String;

   function Get_Line
      return Bounded.Bounded_String;

   procedure Get_Line (
      File : File_Type;
      Item : out Bounded.Bounded_String);

   procedure Get_Line (
      Item : out Bounded.Bounded_String);

end Ada.Text_IO.Bounded_IO;
