with Ada.Exceptions.Finally;
with Ada.Text_IO.Inside.Wide;
with Ada.Unchecked_Deallocation;
package body Ada.Wide_Text_IO is

   type Wide_String_Access is access Wide_String;
   procedure Free is
      new Unchecked_Deallocation (Wide_String, Wide_String_Access);

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

   procedure Get (File : File_Type; Item : out Wide_Character) is
   begin
      Text_IO.Inside.Wide.Get (Text_IO.File_Type (File), Item);
   end Get;

   procedure Get (Item : out Wide_Character) is
   begin
      Get (Current_Input.all, Item);
   end Get;

   procedure Get (File : File_Type; Item : out Wide_String) is
   begin
      for I in Item'Range loop
         Get (File, Item (I));
      end loop;
   end Get;

   procedure Get (Item : out Wide_String) is
   begin
      Get (Current_Input.all, Item);
   end Get;

   procedure Get_Immediate (File : File_Type; Item : out Wide_Character) is
   begin
      Text_IO.Inside.Wide.Get_Immediate (Text_IO.File_Type (File), Item);
   end Get_Immediate;

   procedure Get_Immediate (Item : out Wide_Character) is
   begin
      Get_Immediate (Current_Input.all, Item);
   end Get_Immediate;

   procedure Get_Immediate (
      File : File_Type;
      Item : out Wide_Character;
      Available : out Boolean) is
   begin
      Text_IO.Inside.Wide.Get_Immediate (
         Text_IO.File_Type (File),
         Item,
         Available);
   end Get_Immediate;

   procedure Get_Immediate (
      Item : out Wide_Character;
      Available : out Boolean) is
   begin
      Get_Immediate (Current_Input.all, Item, Available);
   end Get_Immediate;

   procedure Get_Line (
      File : File_Type;
      Item : out Wide_String;
      Last : out Natural) is
   begin
      Last := Item'First - 1;
      while Last < Item'Last loop
         if End_Of_Line (File) then
            Text_IO.Skip_Line (Text_IO.File_Type (File));
            exit;
         else
            Last := Last + 1;
            Get (File, Item (Last));
         end if;
      end loop;
   end Get_Line;

   procedure Get_Line (
      Item : out Wide_String;
      Last : out Natural) is
   begin
      Get_Line (Current_Input.all, Item, Last);
   end Get_Line;

   function Get_Line (File : File_Type) return Wide_String is
      Line_Buffer : aliased Wide_String_Access :=
         new Wide_String (1 .. 256);
      Next : Positive := 1;
      Last : Natural;
      procedure Finally (X : not null access Wide_String_Access);
      procedure Finally (X : not null access Wide_String_Access) is
      begin
         Free (X.all);
      end Finally;
      package Holder is
         new Exceptions.Finally.Scoped_Holder (Wide_String_Access, Finally);
   begin
      Holder.Assign (Line_Buffer'Access);
      loop
         Get_Line (File, Line_Buffer (Next .. Line_Buffer'Last), Last);
         exit when Last < Line_Buffer'Last;
         Next := Line_Buffer'Last + 1;
         declare
            New_Buffer : constant Wide_String_Access :=
               new Wide_String (1 .. Line_Buffer'Last * 2);
         begin
            New_Buffer (Line_Buffer'Range) := Line_Buffer.all;
            Free (Line_Buffer);
            Line_Buffer := New_Buffer;
         end;
      end loop;
      return Line_Buffer (1 .. Last);
   end Get_Line;

   function Get_Line return Wide_String is
   begin
      return Get_Line (Current_Input.all);
   end Get_Line;

   procedure Look_Ahead (
      File : File_Type;
      Item : out Wide_Character;
      End_Of_Line : out Boolean) is
   begin
      Text_IO.Inside.Wide.Look_Ahead (
         Text_IO.File_Type (File),
         Item,
         End_Of_Line);
   end Look_Ahead;

   procedure Look_Ahead (
      Item : out Wide_Character;
      End_Of_Line : out Boolean) is
   begin
      Look_Ahead (Current_Input.all, Item, End_Of_Line);
   end Look_Ahead;

   procedure Put (File : File_Type; Item : Wide_Character) is
   begin
      Text_IO.Inside.Wide.Put (Text_IO.File_Type (File), Item);
   end Put;

   procedure Put (Item : Wide_Character) is
   begin
      Put (Current_Output.all, Item);
   end Put;

   procedure Put (File : File_Type; Item : Wide_String) is
   begin
      for I in Item'Range loop
         Put (File, Item (I));
      end loop;
   end Put;

   procedure Put (Item : Wide_String) is
   begin
      Put (Current_Output.all, Item);
   end Put;

   procedure Put_Line (File : File_Type; Item : Wide_String) is
   begin
      Put (File, Item);
      Text_IO.New_Line (Text_IO.File_Type (File)); -- New_Line is ambiguous
   end Put_Line;

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
