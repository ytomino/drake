package body Ada.Text_IO.Generic_Bounded_IO is

   procedure Put (
      File : File_Type;
      Item : Bounded.Bounded_String) is
   begin
      Put (File, Item.Element (1 .. Item.Length)); -- checking the predicate
   end Put;

   procedure Put (
      Item : Bounded.Bounded_String) is
   begin
      Put (Current_Output.all, Item);
   end Put;

   procedure Put_Line (
      File : File_Type;
      Item : Bounded.Bounded_String) is
   begin
      Put_Line (
         File, -- checking the predicate
         Item.Element (1 .. Item.Length));
   end Put_Line;

   procedure Put_Line (
      Item : Bounded.Bounded_String) is
   begin
      Put_Line (Current_Output.all, Item);
   end Put_Line;

   function Get_Line (
      File : File_Type)
      return Bounded.Bounded_String is
   begin
      return Result : Bounded.Bounded_String do
         Get_Line (File, Result); -- checking the predicate
      end return;
   end Get_Line;

   function Get_Line
      return Bounded.Bounded_String is
   begin
      return Get_Line (Current_Input.all);
   end Get_Line;

   procedure Get_Line (
      File : File_Type;
      Item : out Bounded.Bounded_String) is
   begin
      Get_Line (File, Item.Element, Item.Length); -- checking the predicate
   end Get_Line;

   procedure Get_Line (
      Item : out Bounded.Bounded_String) is
   begin
      Get_Line (Current_Input.all, Item);
   end Get_Line;

end Ada.Text_IO.Generic_Bounded_IO;
