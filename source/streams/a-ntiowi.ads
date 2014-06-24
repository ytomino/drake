pragma License (Unrestricted);
--  implementation unit
package Ada.Naked_Text_IO.Wide is

   --  for Wide_Text_IO, Wide_Wide_Text_IO

   procedure Get (
      File : Non_Controlled_File_Type;
      Item : out Wide_Character);
   procedure Get (
      File : Non_Controlled_File_Type;
      Item : out Wide_Wide_Character);

   procedure Put (
      File : Non_Controlled_File_Type;
      Item : Wide_Character);
   procedure Put (
      File : Non_Controlled_File_Type;
      Item : Wide_Wide_Character);

   procedure Look_Ahead (
      File : Non_Controlled_File_Type;
      Item : out Wide_Character;
      End_Of_Line : out Boolean);
   procedure Look_Ahead (
      File : Non_Controlled_File_Type;
      Item : out Wide_Wide_Character;
      End_Of_Line : out Boolean);

   procedure Get_Immediate (
      File : Non_Controlled_File_Type;
      Item : out Wide_Character);
   procedure Get_Immediate (
      File : Non_Controlled_File_Type;
      Item : out Wide_Wide_Character);

   procedure Get_Immediate (
      File : Non_Controlled_File_Type;
      Item : out Wide_Character;
      Available : out Boolean);
   procedure Get_Immediate (
      File : Non_Controlled_File_Type;
      Item : out Wide_Wide_Character;
      Available : out Boolean);

private

   procedure Get_Immediate (
      File : Non_Controlled_File_Type;
      Item : out Wide_Character;
      Available : out Boolean;
      Wait : Boolean);
   procedure Get_Immediate (
      File : Non_Controlled_File_Type;
      Item : out Wide_Wide_Character;
      Available : out Boolean;
      Wait : Boolean);

end Ada.Naked_Text_IO.Wide;
