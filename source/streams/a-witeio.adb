package body Ada.Wide_Text_IO is

   --  implementation

   function Current_Error return File_Access is
   begin
      return File_Type (Text_IO.Current_Error.all)'Unrestricted_Access;
   end Current_Error;

   function Current_Input return File_Access is
   begin
      return File_Type (Text_IO.Current_Input.all)'Unrestricted_Access;
   end Current_Input;

   function Current_Output return File_Access is
   begin
      return File_Type (Text_IO.Current_Output.all)'Unrestricted_Access;
   end Current_Output;

   procedure Get (Item : out Wide_Character) is
   begin
      Get (Current_Input.all, Item);
   end Get;

   procedure Get (Item : out Wide_String) is
   begin
      Get (Current_Input.all, Item);
   end Get;

   procedure Get_Immediate (Item : out Wide_Character) is
   begin
      Get_Immediate (Current_Input.all, Item);
   end Get_Immediate;

   procedure Get_Immediate (
      Item : out Wide_Character;
      Available : out Boolean) is
   begin
      Get_Immediate (Current_Input.all, Item, Available);
   end Get_Immediate;

   procedure Get_Line (
      Item : out Wide_String;
      Last : out Natural) is
   begin
      Get_Line (Current_Input.all, Item, Last);
   end Get_Line;

   function Get_Line return Wide_String is
   begin
      return Get_Line (Current_Input.all);
   end Get_Line;

   procedure Look_Ahead (
      Item : out Wide_Character;
      End_Of_Line : out Boolean) is
   begin
      Look_Ahead (Current_Input.all, Item, End_Of_Line);
   end Look_Ahead;

   procedure Put (Item : Wide_Character) is
   begin
      Put (Current_Output.all, Item);
   end Put;

   procedure Put (Item : Wide_String) is
   begin
      Put (Current_Output.all, Item);
   end Put;

   procedure Put_Line (Item : Wide_String) is
   begin
      Put_Line (Current_Output.all, Item);
   end Put_Line;

   function Standard_Error return File_Access is
   begin
      return File_Type (Text_IO.Standard_Error.all)'Unrestricted_Access;
   end Standard_Error;

   function Standard_Input return File_Access is
   begin
      return File_Type (Text_IO.Standard_Input.all)'Unrestricted_Access;
   end Standard_Input;

   function Standard_Output return File_Access is
   begin
      return File_Type (Text_IO.Standard_Output.all)'Unrestricted_Access;
   end Standard_Output;

end Ada.Wide_Text_IO;
