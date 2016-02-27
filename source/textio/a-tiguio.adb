package body Ada.Text_IO.Generic_Unbounded_IO is

   procedure Put (
      File : File_Type;
      Item : Unbounded_Strings.Unbounded_String) is
   begin
      Put (
         File, -- checking the predicate
         Unbounded_Strings.Constant_Reference (Item).Element.all);
   end Put;

   procedure Put (
      Item : Unbounded_Strings.Unbounded_String) is
   begin
      Put (Current_Output.all, Item);
   end Put;

   procedure Put_Line (
      File : File_Type;
      Item : Unbounded_Strings.Unbounded_String) is
   begin
      Put_Line (
         File, -- checking the predicate
         Unbounded_Strings.Constant_Reference (Item).Element.all);
   end Put_Line;

   procedure Put_Line (
      Item : Unbounded_Strings.Unbounded_String) is
   begin
      Put_Line (Current_Output.all, Item);
   end Put_Line;

   function Get_Line (
      File : File_Type)
      return Unbounded_Strings.Unbounded_String is
   begin
      return Result : Unbounded_Strings.Unbounded_String do
         Get_Line (File, Result); -- checking the predicate
      end return;
   end Get_Line;

   function Get_Line
      return Unbounded_Strings.Unbounded_String is
   begin
      return Get_Line (Current_Input.all);
   end Get_Line;

   procedure Get_Line (
      File : File_Type;
      Item : out Unbounded_Strings.Unbounded_String)
   is
      Last : Natural := 0;
      Capacity : Natural := 256;
   begin
      loop
         Unbounded_Strings.Set_Length (Item, Capacity);
         Get_Line (
            File, -- checking the predicate
            Unbounded_Strings.Reference (Item).Element (Last + 1 .. Capacity),
            Last);
         exit when Last < Capacity;
         Capacity := Capacity * 2;
      end loop;
      Unbounded_Strings.Set_Length (Item, Last);
   end Get_Line;

   procedure Get_Line (
      Item : out Unbounded_Strings.Unbounded_String) is
   begin
      Get_Line (Current_Input.all, Item);
   end Get_Line;

end Ada.Text_IO.Generic_Unbounded_IO;
