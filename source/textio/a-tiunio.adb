package body Ada.Text_IO.Unbounded_IO is

   procedure Put (
      File : File_Type;
      Item : Strings.Unbounded.Unbounded_String) is
   begin
      Put (File, Item.Constant_Reference.Element.all);
   end Put;

   procedure Put (
      Item : Strings.Unbounded.Unbounded_String) is
   begin
      Put (Current_Output.all, Item);
   end Put;

   procedure Put_Line (
      File : File_Type;
      Item : Strings.Unbounded.Unbounded_String) is
   begin
      Put_Line (File, Item.Constant_Reference.Element.all);
   end Put_Line;

   procedure Put_Line (
      Item : Strings.Unbounded.Unbounded_String) is
   begin
      Put_Line (Current_Output.all, Item);
   end Put_Line;

   function Get_Line (
      File : File_Type)
      return Strings.Unbounded.Unbounded_String is
   begin
      return Strings.Unbounded.To_Unbounded_String (Get_Line (File));
   end Get_Line;

   function Get_Line
      return Strings.Unbounded.Unbounded_String is
   begin
      return Get_Line (Current_Input.all);
   end Get_Line;

   procedure Get_Line (
      File : File_Type;
      Item : out Strings.Unbounded.Unbounded_String) is
   begin
      Item := Get_Line (File);
   end Get_Line;

   procedure Get_Line (
      Item : out Strings.Unbounded.Unbounded_String) is
   begin
      Get_Line (Current_Input.all, Item);
   end Get_Line;

end Ada.Text_IO.Unbounded_IO;
