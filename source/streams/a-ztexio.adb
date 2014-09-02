package body Ada.Wide_Wide_Text_IO is

   --  implementation

   procedure Get (Item : out Wide_Wide_Character) is
   begin
      Get (Current_Input.all, Item);
   end Get;

   procedure Get (Item : out Wide_Wide_String) is
   begin
      Get (Current_Input.all, Item);
   end Get;

   procedure Get_Immediate (Item : out Wide_Wide_Character) is
   begin
      Get_Immediate (Current_Input.all, Item);
   end Get_Immediate;

   procedure Get_Immediate (
      Item : out Wide_Wide_Character;
      Available : out Boolean) is
   begin
      Get_Immediate (Current_Input.all, Item, Available);
   end Get_Immediate;

   procedure Get_Line (
      Item : out Wide_Wide_String;
      Last : out Natural) is
   begin
      Get_Line (Current_Input.all, Item, Last);
   end Get_Line;

   function Get_Line return Wide_Wide_String is
   begin
      return Get_Line (Current_Input.all);
   end Get_Line;

   procedure Look_Ahead (
      Item : out Wide_Wide_Character;
      End_Of_Line : out Boolean) is
   begin
      Look_Ahead (Current_Input.all, Item, End_Of_Line);
   end Look_Ahead;

   procedure Put (Item : Wide_Wide_Character) is
   begin
      Put (Current_Output.all, Item);
   end Put;

   procedure Put (Item : Wide_Wide_String) is
   begin
      Put (Current_Output.all, Item);
   end Put;

   procedure Put_Line (Item : Wide_Wide_String) is
   begin
      Put_Line (Current_Output.all, Item);
   end Put_Line;

end Ada.Wide_Wide_Text_IO;
