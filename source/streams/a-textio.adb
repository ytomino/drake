--  ***************************************************************************
--
--  This implementation violates some ACATS intentionally.
--
--  Violated ACATS Tests: CE3106A, CE3106B, CE3406C
--
--  These test requires End_Of_Page/File looking over the line/page terminater.
--  But this behavior discards last line in file.
--
--  Violated ACATS Tests: CE3402C, CE3405A, CE3405D, CE3410C, CE3606A, CE3606B
--
--  With the same reason, CHECK_FILE fails at CHECK_END_OF_PAGE.
--
--  Please, look discussions on comp.lang.ada.
--  http://groups.google.com/group/comp.lang.ada/browse_frm/thread/
--     5afe598156615c8b/f690474efabf7a93#f690474efabf7a93
--  http://groups.google.com/group/comp.lang.ada/browse_frm/thread/
--     68cd50941308f5a9/5d2b3f163916189c#5d2b3f163916189c
--
--  ***************************************************************************
pragma Check_Policy (Finalize, Off);
with Ada.Exceptions.Finally;
with Ada.Text_IO.Inside; --  full view
with Ada.Unchecked_Deallocation;
package body Ada.Text_IO is

   type String_Access is access String;
   procedure Free is new Unchecked_Deallocation (String, String_Access);

   Standard_Input_Object : aliased File_Type := (
      Finalization.Limited_Controlled with
      Text => Inside.Standard_Input);

   Standard_Output_Object : aliased File_Type := (
      Finalization.Limited_Controlled with
      Text => Inside.Standard_Output);

   Standard_Error_Object : aliased File_Type := (
      Finalization.Limited_Controlled with
      Text => Inside.Standard_Error);

   Current_Input_Access : File_Access := Standard_Input_Object'Access;
   Current_Output_Access : File_Access := Standard_Output_Object'Access;
   Current_Error_Access : File_Access := Standard_Error_Object'Access;

   procedure Check_File_Mode (
      File : File_Type;
      Expected : File_Mode);
   procedure Check_File_Mode (
      File : File_Type;
      Expected : File_Mode) is
   begin
      if (Mode (File) = In_File) /= (Expected = In_File) then
         raise Mode_Error;
      end if;
   end Check_File_Mode;

   --  implementation

   procedure Close (File : in out File_Type) is
   begin
      Inside.Close (
         Inside.Non_Controlled_File_Type (File.Text),
         Raise_On_Error => True);
   end Close;

   function Col (File : File_Type) return Positive_Count is
   begin
      return Inside.Col (Inside.Non_Controlled_File_Type (File.Text));
   end Col;

   function Col return Positive_Count is
   begin
      return Col (Current_Output_Access.all);
   end Col;

   function Col (File : not null File_Access) return Positive_Count is
   begin
      return Col (File.all);
   end Col;

   procedure Create (
      File : in out File_Type;
      Mode : File_Mode := Out_File;
      Name : String := "";
      Form : String := "") is
   begin
      Inside.Create (
         Inside.Non_Controlled_File_Type (File.Text),
         Mode,
         Name,
         Form);
   end Create;

   function Create (
      Mode : File_Mode := Out_File;
      Name : String := "";
      Form : String := "")
      return File_Type is
   begin
      return Result : File_Type do
         Create (Result, Mode, Name, Form);
      end return;
   end Create;

   function Current_Error return File_Access is
   begin
      return Current_Error_Access;
   end Current_Error;

   function Current_Input return File_Access is
   begin
      return Current_Input_Access;
   end Current_Input;

   function Current_Output return File_Access is
   begin
      return Current_Output_Access;
   end Current_Output;

   procedure Delete (File : in out File_Type) is
   begin
      Inside.Delete (Inside.Non_Controlled_File_Type (File.Text));
   end Delete;

   function End_Of_File (File : File_Type) return Boolean is
   begin
      return Inside.End_Of_File (Inside.Non_Controlled_File_Type (File.Text));
   end End_Of_File;

   function End_Of_File return Boolean is
   begin
      return End_Of_File (Current_Input_Access.all);
   end End_Of_File;

   function End_Of_File (File : not null File_Access) return Boolean is
   begin
      return End_Of_File (File.all);
   end End_Of_File;

   function End_Of_Line (File : File_Type) return Boolean is
   begin
      return Inside.End_Of_Line (Inside.Non_Controlled_File_Type (File.Text));
   end End_Of_Line;

   function End_Of_Line return Boolean is
   begin
      return End_Of_Line (Current_Input_Access.all);
   end End_Of_Line;

   function End_Of_Page (File : File_Type) return Boolean is
   begin
      return Inside.End_Of_Page (Inside.Non_Controlled_File_Type (File.Text));
   end End_Of_Page;

   function End_Of_Page return Boolean is
   begin
      return End_Of_Page (Current_Input_Access.all);
   end End_Of_Page;

   function End_Of_Page (File : not null File_Access) return Boolean is
   begin
      return End_Of_Page (File.all);
   end End_Of_Page;

   overriding procedure Finalize (Object : in out File_Type) is
   begin
      pragma Check (Finalize, Debug.Put ("enter"));
      Inside.Close (
         Inside.Non_Controlled_File_Type (Object.Text),
         Raise_On_Error => False);
      pragma Check (Finalize, Debug.Put ("leave"));
   end Finalize;

   procedure Flush (File : File_Type) is
   begin
      Inside.Flush (Inside.Non_Controlled_File_Type (File.Text));
   end Flush;

   procedure Flush is
   begin
      Flush (Current_Output_Access.all);
   end Flush;

   function Form (File : File_Type) return String is
   begin
      return Inside.Form (Inside.Non_Controlled_File_Type (File.Text));
   end Form;

   procedure Get (File : File_Type; Item : out Character) is
   begin
      Inside.Get (Inside.Non_Controlled_File_Type (File.Text), Item);
   end Get;

   procedure Get (Item : out Character) is
   begin
      Get (Current_Input_Access.all, Item);
   end Get;

   procedure Get (File : not null File_Access; Item : out Character) is
   begin
      Get (File.all, Item);
   end Get;

   procedure Get (File : File_Type; Item : out String) is
   begin
      for I in Item'Range loop
         Get (File, Item (I));
      end loop;
   end Get;

   procedure Get (Item : out String) is
   begin
      Get (Current_Input_Access.all, Item);
   end Get;

   procedure Get (File : not null File_Access; Item : out String) is
   begin
      Get (File.all, Item);
   end Get;

   procedure Get_Immediate (File : File_Type; Item : out Character) is
   begin
      Inside.Get_Immediate (Inside.Non_Controlled_File_Type (File.Text), Item);
   end Get_Immediate;

   procedure Get_Immediate (Item : out Character) is
   begin
      Get_Immediate (Current_Input_Access.all, Item);
   end Get_Immediate;

   procedure Get_Immediate (
      File : File_Type;
      Item : out Character;
      Available : out Boolean) is
   begin
      Inside.Get_Immediate (
         Inside.Non_Controlled_File_Type (File.Text),
         Item,
         Available);
   end Get_Immediate;

   procedure Get_Immediate (
      Item : out Character;
      Available : out Boolean) is
   begin
      Get_Immediate (Current_Input_Access.all, Item, Available);
   end Get_Immediate;

   procedure Get_Line (
      File : File_Type;
      Item : out String;
      Last : out Natural) is
   begin
      if Item'Length > 0 then
         if End_Of_File (File) then
            raise End_Error;
         end if;
         Last := Item'First - 1;
         while Last < Item'Last loop
            if End_Of_Line (File) then
               Skip_Line (File);
               exit;
            else
               Last := Last + 1;
               Get (File, Item (Last));
            end if;
         end loop;
      end if;
   end Get_Line;

   procedure Get_Line (
      Item : out String;
      Last : out Natural) is
   begin
      Get_Line (Current_Input_Access.all, Item, Last);
   end Get_Line;

   procedure Get_Line (
      File : not null File_Access;
      Item : out String;
      Last : out Natural) is
   begin
      Get_Line (File.all, Item, Last);
   end Get_Line;

   function Get_Line (File : File_Type) return String is
      Line_Buffer : aliased String_Access :=
         new String (1 .. 256);
      Next : Positive := 1;
      Last : Natural;
      procedure Finally (X : not null access String_Access);
      procedure Finally (X : not null access String_Access) is
      begin
         Free (X.all);
      end Finally;
      package Holder is new Exceptions.Finally.Scoped_Holder (
         String_Access,
         Finally);
   begin
      Holder.Assign (Line_Buffer'Access);
      loop
         Get_Line (File, Line_Buffer (Next .. Line_Buffer'Last), Last);
         exit when Last < Line_Buffer'Last;
         Next := Line_Buffer'Last + 1;
         declare
            New_Buffer : constant String_Access :=
               new String (1 .. Line_Buffer'Last * 2);
         begin
            New_Buffer (Line_Buffer'Range) := Line_Buffer.all;
            Free (Line_Buffer);
            Line_Buffer := New_Buffer;
         end;
      end loop;
      return Line_Buffer (1 .. Last);
   end Get_Line;

   function Get_Line return String is
   begin
      return Get_Line (Current_Input_Access.all);
   end Get_Line;

   function Is_Open (File : File_Type) return Boolean is
   begin
      return Inside.Is_Open (Inside.Non_Controlled_File_Type (File.Text));
   end Is_Open;

   function Is_Open (File : not null File_Access) return Boolean is
   begin
      return Is_Open (File.all);
   end Is_Open;

   function Line (File : File_Type) return Positive_Count is
   begin
      return Inside.Line (Inside.Non_Controlled_File_Type (File.Text));
   end Line;

   function Line return Positive_Count is
   begin
      return Line (Current_Output_Access.all);
   end Line;

   function Line (File : not null File_Access) return Positive_Count is
   begin
      return Line (File.all);
   end Line;

   function Line_Length (File : File_Type) return Count is
   begin
      return Inside.Line_Length (Inside.Non_Controlled_File_Type (File.Text));
   end Line_Length;

   function Line_Length return Count is
   begin
      return Line_Length (Current_Output_Access.all);
   end Line_Length;

   procedure Look_Ahead (
      File : File_Type;
      Item : out Character;
      End_Of_Line : out Boolean) is
   begin
      Inside.Look_Ahead (
         Inside.Non_Controlled_File_Type (File.Text),
         Item,
         End_Of_Line);
   end Look_Ahead;

   procedure Look_Ahead (
      Item : out Character;
      End_Of_Line : out Boolean) is
   begin
      Look_Ahead (Current_Input_Access.all, Item, End_Of_Line);
   end Look_Ahead;

   function Mode (File : File_Type) return File_Mode is
   begin
      return Inside.Mode (Inside.Non_Controlled_File_Type (File.Text));
   end Mode;

   function Name (File : File_Type) return String is
   begin
      return Inside.Name (Inside.Non_Controlled_File_Type (File.Text));
   end Name;

   function Name (File : not null File_Access) return String is
   begin
      return Name (File.all);
   end Name;

   procedure New_Line (File : File_Type; Spacing : Positive_Count := 1) is
   begin
      Inside.New_Line (Inside.Non_Controlled_File_Type (File.Text), Spacing);
   end New_Line;

   procedure New_Line (Spacing : Positive_Count := 1) is
   begin
      New_Line (Current_Output_Access.all, Spacing);
   end New_Line;

   procedure New_Line (
      File : not null File_Access;
      Spacing : Positive_Count := 1) is
   begin
      New_Line (File.all, Spacing);
   end New_Line;

   procedure New_Page (File : File_Type) is
   begin
      Inside.New_Page (Inside.Non_Controlled_File_Type (File.Text));
   end New_Page;

   procedure New_Page is
   begin
      New_Page (Current_Output_Access.all);
   end New_Page;

   procedure New_Page (File : not null File_Access) is
   begin
      New_Page (File.all);
   end New_Page;

   procedure Open (
      File : in out File_Type;
      Mode : File_Mode;
      Name : String;
      Form : String := "") is
   begin
      Inside.Open (
         Inside.Non_Controlled_File_Type (File.Text),
         Mode,
         Name,
         Form);
   end Open;

   function Open (
      Mode : File_Mode;
      Name : String;
      Form : String := "")
      return File_Type is
   begin
      return Result : File_Type do
         Open (Result, Mode, Name, Form);
      end return;
   end Open;

   function Page (File : File_Type) return Positive_Count is
   begin
      return Inside.Page (Inside.Non_Controlled_File_Type (File.Text));
   end Page;

   function Page return Positive_Count is
   begin
      return Page (Current_Output_Access.all);
   end Page;

   function Page (File : not null File_Access) return Positive_Count is
   begin
      return Page (File.all);
   end Page;

   function Page_Length (File : File_Type) return Count is
   begin
      return Inside.Page_Length (Inside.Non_Controlled_File_Type (File.Text));
   end Page_Length;

   function Page_Length return Count is
   begin
      return Page_Length (Current_Output_Access.all);
   end Page_Length;

   procedure Put (File : File_Type; Item : Character) is
   begin
      Inside.Put (Inside.Non_Controlled_File_Type (File.Text), Item);
   end Put;

   procedure Put (Item : Character) is
   begin
      Put (Current_Output_Access.all, Item);
   end Put;

   procedure Put (File : not null File_Access; Item : Character) is
   begin
      Put (File.all, Item);
   end Put;

   procedure Put (File : File_Type; Item : String) is
   begin
      for I in Item'Range loop
         Put (File, Item (I));
      end loop;
   end Put;

   procedure Put (Item : String) is
   begin
      Put (Current_Output_Access.all, Item);
   end Put;

   procedure Put (File : not null File_Access; Item : String) is
   begin
      Put (File.all, Item);
   end Put;

   procedure Put_Line (File : File_Type; Item : String) is
   begin
      Put (File, Item);
      New_Line (File);
   end Put_Line;

   procedure Put_Line (Item : String) is
   begin
      Put_Line (Current_Output_Access.all, Item);
   end Put_Line;

   procedure Put_Line (File : not null File_Access; Item : String) is
   begin
      Put_Line (File.all, Item);
   end Put_Line;

   procedure Reset (File : in out File_Type; Mode : File_Mode) is
   begin
      if (File'Unrestricted_Access = Current_Input_Access
         or else File'Unrestricted_Access = Current_Output_Access
         or else File'Unrestricted_Access = Current_Error_Access)
         and then Text_IO.Mode (File) /= Mode
      then
         raise Mode_Error;
      else
         Inside.Reset (Inside.Non_Controlled_File_Type (File.Text), Mode);
      end if;
   end Reset;

   procedure Reset (File : in out File_Type) is
   begin
      Reset (File, Mode (File));
   end Reset;

   procedure Set_Col (File : File_Type; To : Positive_Count) is
   begin
      Inside.Set_Col (Inside.Non_Controlled_File_Type (File.Text), To);
   end Set_Col;

   procedure Set_Col (To : Positive_Count) is
   begin
      Set_Col (Current_Output_Access.all, To);
   end Set_Col;

   procedure Set_Col (File : not null File_Access; To : Positive_Count) is
   begin
      Set_Col (File.all, To);
   end Set_Col;

   procedure Set_Error (File : File_Type) is
   begin
      Set_Error (File'Unrestricted_Access);
   end Set_Error;

   procedure Set_Error (File : not null File_Access) is
   begin
      Check_File_Mode (File.all, Out_File);
      Current_Error_Access := File;
   end Set_Error;

   procedure Set_Input (File : File_Type) is
   begin
      Set_Input (File'Unrestricted_Access);
   end Set_Input;

   procedure Set_Input (File : not null File_Access) is
   begin
      Check_File_Mode (File.all, In_File);
      Current_Input_Access := File;
   end Set_Input;

   procedure Set_Line (File : File_Type; To : Positive_Count) is
   begin
      Inside.Set_Line (Inside.Non_Controlled_File_Type (File.Text), To);
   end Set_Line;

   procedure Set_Line (To : Positive_Count) is
   begin
      Set_Line (Current_Output_Access.all, To);
   end Set_Line;

   procedure Set_Line (File : not null File_Access; To : Positive_Count) is
   begin
      Set_Line (File.all, To);
   end Set_Line;

   procedure Set_Line_Length (File : File_Type; To : Count) is
   begin
      Inside.Set_Line_Length (Inside.Non_Controlled_File_Type (File.Text), To);
   end Set_Line_Length;

   procedure Set_Line_Length (To : Count) is
   begin
      Set_Line_Length (Current_Output_Access.all, To);
   end Set_Line_Length;

   procedure Set_Line_Length (File : not null File_Access; To : Count) is
   begin
      Set_Line_Length (File.all, To);
   end Set_Line_Length;

   procedure Set_Output (File : File_Type) is
   begin
      Set_Output (File'Unrestricted_Access);
   end Set_Output;

   procedure Set_Output (File : not null File_Access) is
   begin
      Check_File_Mode (File.all, Out_File);
      Current_Output_Access := File;
   end Set_Output;

   procedure Set_Page_Length (File : File_Type; To : Count) is
   begin
      Inside.Set_Page_Length (Inside.Non_Controlled_File_Type (File.Text), To);
   end Set_Page_Length;

   procedure Set_Page_Length (To : Count) is
   begin
      Set_Page_Length (Current_Output_Access.all, To);
   end Set_Page_Length;

   procedure Set_Page_Length (File : not null File_Access; To : Count) is
   begin
      Set_Page_Length (File.all, To);
   end Set_Page_Length;

   procedure Skip_Line (File : File_Type; Spacing : Positive_Count := 1) is
   begin
      Inside.Skip_Line (Inside.Non_Controlled_File_Type (File.Text), Spacing);
   end Skip_Line;

   procedure Skip_Line (Spacing : Positive_Count := 1) is
   begin
      Skip_Line (Current_Input_Access.all, Spacing);
   end Skip_Line;

   procedure Skip_Line (
      File : not null File_Access;
      Spacing : Positive_Count := 1) is
   begin
      Skip_Line (File.all, Spacing);
   end Skip_Line;

   procedure Skip_Page (File : File_Type) is
   begin
      Inside.Skip_Page (Inside.Non_Controlled_File_Type (File.Text));
   end Skip_Page;

   procedure Skip_Page is
   begin
      Skip_Page (Current_Input_Access.all);
   end Skip_Page;

   procedure Skip_Page (File : not null File_Access) is
   begin
      Skip_Page (File.all);
   end Skip_Page;

   function Standard_Error return File_Access is
   begin
      return Standard_Error_Object'Access;
   end Standard_Error;

   function Standard_Input return File_Access is
   begin
      return Standard_Input_Object'Access;
   end Standard_Input;

   function Standard_Output return File_Access is
   begin
      return Standard_Output_Object'Access;
   end Standard_Output;

end Ada.Text_IO;
