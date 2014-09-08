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
pragma Check_Policy (Trace, Off);
with Ada.Exception_Identification.From_Here;
with Ada.Exceptions.Finally;
with Ada.Streams.Naked_Stream_IO;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Reallocation;
package body Ada.Text_IO is
   use Exception_Identification.From_Here;

   function To_File_Access is
      new Unchecked_Conversion (Controlled.File_Access, File_Access);
   function To_Controlled_File_Access is
      new Unchecked_Conversion (File_Access, Controlled.File_Access);

   procedure Check_File_Mode (
      File : File_Type;
      Expected : File_Mode;
      Line : Natural := Debug.Line);
   procedure Check_File_Mode (
      File : File_Type;
      Expected : File_Mode;
      Line : Natural := Debug.Line) is
   begin
      if (Mode (File) = In_File) /= (Expected = In_File) then
         Raise_Exception (Mode_Error'Identity, Line => Line);
      end if;
   end Check_File_Mode;

   procedure Reallocate is
      new Unchecked_Reallocation (
         Positive,
         Character,
         String,
         String_Access);

   procedure Reallocate is
      new Unchecked_Reallocation (
         Positive,
         Wide_Character,
         Wide_String,
         Wide_String_Access);

   procedure Reallocate is
      new Unchecked_Reallocation (
         Positive,
         Wide_Wide_Character,
         Wide_Wide_String,
         Wide_Wide_String_Access);

   procedure Finally (X : not null access String_Access);
   procedure Finally (X : not null access String_Access) is
   begin
      Free (X.all);
   end Finally;

   procedure Finally (X : not null access Wide_String_Access);
   procedure Finally (X : not null access Wide_String_Access) is
   begin
      Free (X.all);
   end Finally;

   procedure Finally (X : not null access Wide_Wide_String_Access);
   procedure Finally (X : not null access Wide_Wide_String_Access) is
   begin
      Free (X.all);
   end Finally;

   procedure Raw_Get_Line (
      File : File_Type;
      Item : aliased out String_Access;
      Last : out Natural);
   procedure Raw_Get_Line (
      File : File_Type;
      Item : aliased out String_Access;
      Last : out Natural) is
   begin
      Item := new String (1 .. 256);
      Last := 0;
      loop
         Overloaded_Get_Line (File, Item (Last + 1 .. Item'Last), Last);
         exit when Last < Item'Last;
         Reallocate (Item, 1, Item'Last * 2);
      end loop;
   end Raw_Get_Line;

   procedure Raw_Get_Line (
      File : File_Type;
      Item : aliased out Wide_String_Access;
      Last : out Natural);
   procedure Raw_Get_Line (
      File : File_Type;
      Item : aliased out Wide_String_Access;
      Last : out Natural) is
   begin
      Item := new Wide_String (1 .. 256);
      Last := 0;
      loop
         Overloaded_Get_Line (File, Item (Last + 1 .. Item'Last), Last);
         exit when Last < Item'Last;
         Reallocate (Item, 1, Item'Last * 2);
      end loop;
   end Raw_Get_Line;

   procedure Raw_Get_Line (
      File : File_Type;
      Item : aliased out Wide_Wide_String_Access;
      Last : out Natural);
   procedure Raw_Get_Line (
      File : File_Type;
      Item : aliased out Wide_Wide_String_Access;
      Last : out Natural) is
   begin
      Item := new Wide_Wide_String (1 .. 256);
      Last := 0;
      loop
         Overloaded_Get_Line (File, Item (Last + 1 .. Item'Last), Last);
         exit when Last < Item'Last;
         Reallocate (Item, 1, Item'Last * 2);
      end loop;
   end Raw_Get_Line;

   --  implementation of File Management

   procedure Create (
      File : in out File_Type;
      Mode : File_Mode := Out_File;
      Name : String := "";
      Form : String) is
   begin
      Naked_Text_IO.Create (
         Reference (File).all,
         IO_Modes.File_Mode (Mode),
         Name => Name,
         Form => Naked_Text_IO.Pack (Form));
   end Create;

   procedure Create (
      File : in out File_Type;
      Mode : File_Mode := Out_File;
      Name : String := "";
      Shared : IO_Modes.File_Shared_Spec := IO_Modes.By_Mode;
      Wait : Boolean := False;
      Overwrite : Boolean := True;
      External : IO_Modes.File_External_Spec := IO_Modes.By_Target;
      New_Line : IO_Modes.File_New_Line_Spec := IO_Modes.By_Target) is
   begin
      Naked_Text_IO.Create (
         Reference (File).all,
         IO_Modes.File_Mode (Mode),
         Name => Name,
         Form => ((Shared, Wait, Overwrite), External, New_Line));
   end Create;

   function Create (
      Mode : File_Mode := Out_File;
      Name : String := "";
      Shared : IO_Modes.File_Shared_Spec := IO_Modes.By_Mode;
      Wait : Boolean := False;
      Overwrite : Boolean := True;
      External : IO_Modes.File_External_Spec := IO_Modes.By_Target;
      New_Line : IO_Modes.File_New_Line_Spec := IO_Modes.By_Target)
      return File_Type is
   begin
      return Result : File_Type do
         Naked_Text_IO.Create (
            Reference (Result).all,
            IO_Modes.File_Mode (Mode),
            Name => Name,
            Form => ((Shared, Wait, Overwrite), External, New_Line));
      end return;
   end Create;

   procedure Open (
      File : in out File_Type;
      Mode : File_Mode;
      Name : String;
      Form : String) is
   begin
      Naked_Text_IO.Open (
         Reference (File).all,
         IO_Modes.File_Mode (Mode),
         Name => Name,
         Form => Naked_Text_IO.Pack (Form));
   end Open;

   procedure Open (
      File : in out File_Type;
      Mode : File_Mode;
      Name : String;
      Shared : IO_Modes.File_Shared_Spec := IO_Modes.By_Mode;
      Wait : Boolean := False;
      Overwrite : Boolean := True;
      External : IO_Modes.File_External_Spec := IO_Modes.By_Target;
      New_Line : IO_Modes.File_New_Line_Spec := IO_Modes.By_Target) is
   begin
      Naked_Text_IO.Open (
         Reference (File).all,
         IO_Modes.File_Mode (Mode),
         Name => Name,
         Form => ((Shared, Wait, Overwrite), External, New_Line));
   end Open;

   function Open (
      Mode : File_Mode;
      Name : String;
      Shared : IO_Modes.File_Shared_Spec := IO_Modes.By_Mode;
      Wait : Boolean := False;
      Overwrite : Boolean := True;
      External : IO_Modes.File_External_Spec := IO_Modes.By_Target;
      New_Line : IO_Modes.File_New_Line_Spec := IO_Modes.By_Target)
      return File_Type is
   begin
      return Result : File_Type do
         Naked_Text_IO.Open (
            Reference (Result).all,
            IO_Modes.File_Mode (Mode),
            Name => Name,
            Form => ((Shared, Wait, Overwrite), External, New_Line));
      end return;
   end Open;

   procedure Close (File : in out File_Type) is
   begin
      Naked_Text_IO.Close (Reference (File).all, Raise_On_Error => True);
   end Close;

   procedure Delete (File : in out File_Type) is
   begin
      Naked_Text_IO.Delete (Reference (File).all);
   end Delete;

   procedure Reset (File : in out File_Type; Mode : File_Mode) is
   begin
      if (File'Unrestricted_Access = Current_Input
         or else File'Unrestricted_Access = Current_Output
         or else File'Unrestricted_Access = Current_Error)
         and then Text_IO.Mode (File) /= Mode
      then
         Raise_Exception (Mode_Error'Identity);
      else
         Naked_Text_IO.Reset (Reference (File).all, IO_Modes.File_Mode (Mode));
      end if;
   end Reset;

   procedure Reset (File : in out File_Type) is
   begin
      Reset (File, Mode (File));
   end Reset;

   function Mode (File : File_Type) return File_Mode is
   begin
      return File_Mode (Naked_Text_IO.Mode (Reference (File).all));
   end Mode;

   function Name (File : File_Type) return String is
   begin
      return Naked_Text_IO.Name (Reference (File).all);
   end Name;

   function Name (File : not null File_Access) return String is
   begin
      return Name (File.all);
   end Name;

   function Form (File : File_Type) return String is
      Non_Controlled_File : constant Naked_Text_IO.Non_Controlled_File_Type :=
         Reference (File).all;
      Result : Streams.Naked_Stream_IO.Form_String;
      Last : Natural;
   begin
      Naked_Text_IO.Unpack (
         Naked_Text_IO.Form (Non_Controlled_File),
         Result,
         Last);
      return Result (Result'First .. Last);
   end Form;

   function Is_Open (File : File_Type) return Boolean is
   begin
      return Naked_Text_IO.Is_Open (Reference (File).all);
   end Is_Open;

   function Is_Open (File : not null File_Access) return Boolean is
   begin
      return Is_Open (File.all);
   end Is_Open;

   --  implementation of Control of default input and output files

   procedure Set_Input (File : File_Type) is
   begin
      Set_Input (File'Unrestricted_Access);
   end Set_Input;

   procedure Set_Input (File : not null File_Access) is
   begin
      Check_File_Mode (File.all, In_File);
      Controlled.Reference_Current_Input.all :=
         To_Controlled_File_Access (File);
   end Set_Input;

   procedure Set_Output (File : File_Type) is
   begin
      Set_Output (File'Unrestricted_Access);
   end Set_Output;

   procedure Set_Output (File : not null File_Access) is
   begin
      Check_File_Mode (File.all, Out_File);
      Controlled.Reference_Current_Output.all :=
         To_Controlled_File_Access (File);
   end Set_Output;

   procedure Set_Error (File : File_Type) is
   begin
      Set_Error (File'Unrestricted_Access);
   end Set_Error;

   procedure Set_Error (File : not null File_Access) is
   begin
      Check_File_Mode (File.all, Out_File);
      Controlled.Reference_Current_Error.all :=
         To_Controlled_File_Access (File);
   end Set_Error;

   function Standard_Input return File_Access is
   begin
      return To_File_Access (Controlled.Standard_Input);
   end Standard_Input;

   function Standard_Output return File_Access is
   begin
      return To_File_Access (Controlled.Standard_Output);
   end Standard_Output;

   function Standard_Error return File_Access is
   begin
      return To_File_Access (Controlled.Standard_Error);
   end Standard_Error;

   function Current_Input return File_Access is
   begin
      return To_File_Access (Controlled.Reference_Current_Input.all);
   end Current_Input;

   function Current_Output return File_Access is
   begin
      return To_File_Access (Controlled.Reference_Current_Output.all);
   end Current_Output;

   function Current_Error return File_Access is
   begin
      return To_File_Access (Controlled.Reference_Current_Error.all);
   end Current_Error;

   --  implementation of Buffer control

   procedure Flush (File : File_Type) is
   begin
      Naked_Text_IO.Flush (Reference (File).all);
   end Flush;

   procedure Flush is
   begin
      Flush (Current_Output.all);
   end Flush;

   --  implementation of Specification of line and page lengths

   procedure Set_Line_Length (File : File_Type; To : Count) is
   begin
      Naked_Text_IO.Set_Line_Length (Reference (File).all, Integer (To));
   end Set_Line_Length;

   procedure Set_Line_Length (To : Count) is
   begin
      Set_Line_Length (Current_Output.all, To);
   end Set_Line_Length;

   procedure Set_Line_Length (File : not null File_Access; To : Count) is
   begin
      Set_Line_Length (File.all, To);
   end Set_Line_Length;

   procedure Set_Page_Length (File : File_Type; To : Count) is
   begin
      Naked_Text_IO.Set_Page_Length (Reference (File).all, Integer (To));
   end Set_Page_Length;

   procedure Set_Page_Length (To : Count) is
   begin
      Set_Page_Length (Current_Output.all, To);
   end Set_Page_Length;

   procedure Set_Page_Length (File : not null File_Access; To : Count) is
   begin
      Set_Page_Length (File.all, To);
   end Set_Page_Length;

   function Line_Length (File : File_Type) return Count is
   begin
      return Count (Naked_Text_IO.Line_Length (Reference (File).all));
   end Line_Length;

   function Line_Length return Count is
   begin
      return Line_Length (Current_Output.all);
   end Line_Length;

   function Page_Length (File : File_Type) return Count is
   begin
      return Count (Naked_Text_IO.Page_Length (Reference (File).all));
   end Page_Length;

   function Page_Length return Count is
   begin
      return Page_Length (Current_Output.all);
   end Page_Length;

   --  implementation of Column, Line, and Page Control

   procedure New_Line (File : File_Type; Spacing : Positive_Count := 1) is
   begin
      Naked_Text_IO.New_Line (Reference (File).all, Integer (Spacing));
   end New_Line;

   procedure New_Line (Spacing : Positive_Count := 1) is
   begin
      New_Line (Current_Output.all, Spacing);
   end New_Line;

   procedure New_Line (
      File : not null File_Access;
      Spacing : Positive_Count := 1) is
   begin
      New_Line (File.all, Spacing);
   end New_Line;

   procedure Skip_Line (File : File_Type; Spacing : Positive_Count := 1) is
   begin
      Naked_Text_IO.Skip_Line (Reference (File).all, Integer (Spacing));
   end Skip_Line;

   procedure Skip_Line (Spacing : Positive_Count := 1) is
   begin
      Skip_Line (Current_Input.all, Spacing);
   end Skip_Line;

   procedure Skip_Line (
      File : not null File_Access;
      Spacing : Positive_Count := 1) is
   begin
      Skip_Line (File.all, Spacing);
   end Skip_Line;

   function End_Of_Line (File : File_Type) return Boolean is
   begin
      return Naked_Text_IO.End_Of_Line (Reference (File).all);
   end End_Of_Line;

   function End_Of_Line return Boolean is
   begin
      return End_Of_Line (Current_Input.all);
   end End_Of_Line;

   procedure New_Page (File : File_Type) is
   begin
      Naked_Text_IO.New_Page (Reference (File).all);
   end New_Page;

   procedure New_Page is
   begin
      New_Page (Current_Output.all);
   end New_Page;

   procedure New_Page (File : not null File_Access) is
   begin
      New_Page (File.all);
   end New_Page;

   procedure Skip_Page (File : File_Type) is
   begin
      Naked_Text_IO.Skip_Page (Reference (File).all);
   end Skip_Page;

   procedure Skip_Page is
   begin
      Skip_Page (Current_Input.all);
   end Skip_Page;

   procedure Skip_Page (File : not null File_Access) is
   begin
      Skip_Page (File.all);
   end Skip_Page;

   function End_Of_Page (File : File_Type) return Boolean is
   begin
      return Naked_Text_IO.End_Of_Page (Reference (File).all);
   end End_Of_Page;

   function End_Of_Page return Boolean is
   begin
      return End_Of_Page (Current_Input.all);
   end End_Of_Page;

   function End_Of_Page (File : not null File_Access) return Boolean is
   begin
      return End_Of_Page (File.all);
   end End_Of_Page;

   function End_Of_File (File : File_Type) return Boolean is
   begin
      return Naked_Text_IO.End_Of_File (Reference (File).all);
   end End_Of_File;

   function End_Of_File return Boolean is
   begin
      return End_Of_File (Current_Input.all);
   end End_Of_File;

   function End_Of_File (File : not null File_Access) return Boolean is
   begin
      return End_Of_File (File.all);
   end End_Of_File;

   procedure Set_Col (File : File_Type; To : Positive_Count) is
   begin
      Naked_Text_IO.Set_Col (Reference (File).all, Integer (To));
   end Set_Col;

   procedure Set_Col (To : Positive_Count) is
   begin
      Set_Col (Current_Output.all, To);
   end Set_Col;

   procedure Set_Col (File : not null File_Access; To : Positive_Count) is
   begin
      Set_Col (File.all, To);
   end Set_Col;

   procedure Set_Line (File : File_Type; To : Positive_Count) is
   begin
      Naked_Text_IO.Set_Line (Reference (File).all, Integer (To));
   end Set_Line;

   procedure Set_Line (To : Positive_Count) is
   begin
      Set_Line (Current_Output.all, To);
   end Set_Line;

   procedure Set_Line (File : not null File_Access; To : Positive_Count) is
   begin
      Set_Line (File.all, To);
   end Set_Line;

   function Col (File : File_Type) return Positive_Count is
   begin
      return Count (Naked_Text_IO.Col (Reference (File).all));
   end Col;

   function Col return Positive_Count is
   begin
      return Col (Current_Output.all);
   end Col;

   function Col (File : not null File_Access) return Positive_Count is
   begin
      return Col (File.all);
   end Col;

   function Line (File : File_Type) return Positive_Count is
   begin
      return Count (Naked_Text_IO.Line (Reference (File).all));
   end Line;

   function Line return Positive_Count is
   begin
      return Line (Current_Output.all);
   end Line;

   function Line (File : not null File_Access) return Positive_Count is
   begin
      return Line (File.all);
   end Line;

   function Page (File : File_Type) return Positive_Count is
   begin
      return Count (Naked_Text_IO.Page (Reference (File).all));
   end Page;

   function Page return Positive_Count is
   begin
      return Page (Current_Output.all);
   end Page;

   function Page (File : not null File_Access) return Positive_Count is
   begin
      return Page (File.all);
   end Page;

   --  implementation of Character Input-Output

   procedure Overloaded_Get (
      File : File_Type;
      Item : out Character) is
   begin
      Naked_Text_IO.Get (Reference (File).all, Item);
   end Overloaded_Get;

   procedure Overloaded_Get (
      File : File_Type;
      Item : out Wide_Character) is
   begin
      Naked_Text_IO.Get (Reference (File).all, Item);
   end Overloaded_Get;

   procedure Overloaded_Get (
      File : File_Type;
      Item : out Wide_Wide_Character) is
   begin
      Naked_Text_IO.Get (Reference (File).all, Item);
   end Overloaded_Get;

   procedure Overloaded_Get (Item : out Character) is
   begin
      Overloaded_Get (Current_Input.all, Item);
   end Overloaded_Get;

   procedure Overloaded_Get (Item : out Wide_Character) is
   begin
      Overloaded_Get (Current_Input.all, Item);
   end Overloaded_Get;

   procedure Overloaded_Get (Item : out Wide_Wide_Character) is
   begin
      Overloaded_Get (Current_Input.all, Item);
   end Overloaded_Get;

   procedure Get (File : not null File_Access; Item : out Character) is
   begin
      Get (File.all, Item);
   end Get;

   procedure Overloaded_Put (File : File_Type; Item : Character) is
   begin
      Naked_Text_IO.Put (Reference (File).all, Item);
   end Overloaded_Put;

   procedure Overloaded_Put (File : File_Type; Item : Wide_Character) is
   begin
      Naked_Text_IO.Put (Reference (File).all, Item);
   end Overloaded_Put;

   procedure Overloaded_Put (File : File_Type; Item : Wide_Wide_Character) is
   begin
      Naked_Text_IO.Put (Reference (File).all, Item);
   end Overloaded_Put;

   procedure Overloaded_Put (Item : Character) is
   begin
      Overloaded_Put (Current_Output.all, Item);
   end Overloaded_Put;

   procedure Overloaded_Put (Item : Wide_Character) is
   begin
      Overloaded_Put (Current_Output.all, Item);
   end Overloaded_Put;

   procedure Overloaded_Put (Item : Wide_Wide_Character) is
   begin
      Overloaded_Put (Current_Output.all, Item);
   end Overloaded_Put;

   procedure Put (File : not null File_Access; Item : Character) is
   begin
      Put (File.all, Item);
   end Put;

   procedure Overloaded_Look_Ahead (
      File : File_Type;
      Item : out Character;
      End_Of_Line : out Boolean) is
   begin
      Naked_Text_IO.Look_Ahead (Reference (File).all, Item, End_Of_Line);
   end Overloaded_Look_Ahead;

   procedure Overloaded_Look_Ahead (
      File : File_Type;
      Item : out Wide_Character;
      End_Of_Line : out Boolean) is
   begin
      Naked_Text_IO.Look_Ahead (Reference (File).all, Item, End_Of_Line);
   end Overloaded_Look_Ahead;

   procedure Overloaded_Look_Ahead (
      File : File_Type;
      Item : out Wide_Wide_Character;
      End_Of_Line : out Boolean) is
   begin
      Naked_Text_IO.Look_Ahead (Reference (File).all, Item, End_Of_Line);
   end Overloaded_Look_Ahead;

   procedure Overloaded_Look_Ahead (
      Item : out Character;
      End_Of_Line : out Boolean) is
   begin
      Overloaded_Look_Ahead (Current_Input.all, Item, End_Of_Line);
   end Overloaded_Look_Ahead;

   procedure Overloaded_Look_Ahead (
      Item : out Wide_Character;
      End_Of_Line : out Boolean) is
   begin
      Overloaded_Look_Ahead (Current_Input.all, Item, End_Of_Line);
   end Overloaded_Look_Ahead;

   procedure Overloaded_Look_Ahead (
      Item : out Wide_Wide_Character;
      End_Of_Line : out Boolean) is
   begin
      Overloaded_Look_Ahead (Current_Input.all, Item, End_Of_Line);
   end Overloaded_Look_Ahead;

   procedure Skip_Ahead (File : File_Type) is
   begin
      Naked_Text_IO.Skip_Ahead (Reference (File).all);
   end Skip_Ahead;

   procedure Overloaded_Get_Immediate (
      File : File_Type;
      Item : out Character) is
   begin
      Naked_Text_IO.Get_Immediate (Reference (File).all, Item);
   end Overloaded_Get_Immediate;

   procedure Overloaded_Get_Immediate (
      File : File_Type;
      Item : out Wide_Character) is
   begin
      Naked_Text_IO.Get_Immediate (Reference (File).all, Item);
   end Overloaded_Get_Immediate;

   procedure Overloaded_Get_Immediate (
      File : File_Type;
      Item : out Wide_Wide_Character) is
   begin
      Naked_Text_IO.Get_Immediate (Reference (File).all, Item);
   end Overloaded_Get_Immediate;

   procedure Overloaded_Get_Immediate (Item : out Character) is
   begin
      Overloaded_Get_Immediate (Current_Input.all, Item);
   end Overloaded_Get_Immediate;

   procedure Overloaded_Get_Immediate (Item : out Wide_Character) is
   begin
      Overloaded_Get_Immediate (Current_Input.all, Item);
   end Overloaded_Get_Immediate;

   procedure Overloaded_Get_Immediate (Item : out Wide_Wide_Character) is
   begin
      Overloaded_Get_Immediate (Current_Input.all, Item);
   end Overloaded_Get_Immediate;

   procedure Overloaded_Get_Immediate (
      File : File_Type;
      Item : out Character;
      Available : out Boolean) is
   begin
      Naked_Text_IO.Get_Immediate (Reference (File).all, Item, Available);
   end Overloaded_Get_Immediate;

   procedure Overloaded_Get_Immediate (
      File : File_Type;
      Item : out Wide_Character;
      Available : out Boolean) is
   begin
      Naked_Text_IO.Get_Immediate (Reference (File).all, Item, Available);
   end Overloaded_Get_Immediate;

   procedure Overloaded_Get_Immediate (
      File : File_Type;
      Item : out Wide_Wide_Character;
      Available : out Boolean) is
   begin
      Naked_Text_IO.Get_Immediate (Reference (File).all, Item, Available);
   end Overloaded_Get_Immediate;

   procedure Overloaded_Get_Immediate (
      Item : out Character;
      Available : out Boolean) is
   begin
      Overloaded_Get_Immediate (Current_Input.all, Item, Available);
   end Overloaded_Get_Immediate;

   procedure Overloaded_Get_Immediate (
      Item : out Wide_Character;
      Available : out Boolean) is
   begin
      Overloaded_Get_Immediate (Current_Input.all, Item, Available);
   end Overloaded_Get_Immediate;

   procedure Overloaded_Get_Immediate (
      Item : out Wide_Wide_Character;
      Available : out Boolean) is
   begin
      Overloaded_Get_Immediate (Current_Input.all, Item, Available);
   end Overloaded_Get_Immediate;

   --  implementation of String Input-Output

   procedure Overloaded_Get (File : File_Type; Item : out String) is
   begin
      for I in Item'Range loop
         Overloaded_Get (File, Item (I));
      end loop;
   end Overloaded_Get;

   procedure Overloaded_Get (File : File_Type; Item : out Wide_String) is
   begin
      for I in Item'Range loop
         Overloaded_Get (File, Item (I));
      end loop;
   end Overloaded_Get;

   procedure Overloaded_Get (File : File_Type; Item : out Wide_Wide_String) is
   begin
      for I in Item'Range loop
         Overloaded_Get (File, Item (I));
      end loop;
   end Overloaded_Get;

   procedure Overloaded_Get (Item : out String) is
   begin
      Overloaded_Get (Current_Input.all, Item);
   end Overloaded_Get;

   procedure Overloaded_Get (Item : out Wide_String) is
   begin
      Overloaded_Get (Current_Input.all, Item);
   end Overloaded_Get;

   procedure Overloaded_Get (Item : out Wide_Wide_String) is
   begin
      Overloaded_Get (Current_Input.all, Item);
   end Overloaded_Get;

   procedure Get (File : not null File_Access; Item : out String) is
   begin
      Get (File.all, Item);
   end Get;

   procedure Overloaded_Put (File : File_Type; Item : String) is
   begin
      for I in Item'Range loop
         Overloaded_Put (File, Item (I));
      end loop;
   end Overloaded_Put;

   procedure Overloaded_Put (File : File_Type; Item : Wide_String) is
   begin
      for I in Item'Range loop
         Overloaded_Put (File, Item (I));
      end loop;
   end Overloaded_Put;

   procedure Overloaded_Put (File : File_Type; Item : Wide_Wide_String) is
   begin
      for I in Item'Range loop
         Overloaded_Put (File, Item (I));
      end loop;
   end Overloaded_Put;

   procedure Overloaded_Put (Item : String) is
   begin
      Overloaded_Put (Current_Output.all, Item);
   end Overloaded_Put;

   procedure Overloaded_Put (Item : Wide_String) is
   begin
      Overloaded_Put (Current_Output.all, Item);
   end Overloaded_Put;

   procedure Overloaded_Put (Item : Wide_Wide_String) is
   begin
      Overloaded_Put (Current_Output.all, Item);
   end Overloaded_Put;

   procedure Put (File : not null File_Access; Item : String) is
   begin
      Put (File.all, Item);
   end Put;

   procedure Overloaded_Get_Line (
      File : File_Type;
      Item : out String;
      Last : out Natural) is
   begin
      if Item'Length > 0 then
         if End_Of_File (File) then
            Raise_Exception (End_Error'Identity);
         end if;
         Last := Item'First - 1;
         while Last < Item'Last loop
            if End_Of_Line (File) then
               Skip_Line (File);
               exit;
            else
               Last := Last + 1;
               Overloaded_Get (File, Item (Last));
            end if;
         end loop;
      end if;
   end Overloaded_Get_Line;

   procedure Overloaded_Get_Line (
      File : File_Type;
      Item : out Wide_String;
      Last : out Natural) is
   begin
      if Item'Length > 0 then
         if End_Of_File (File) then
            Raise_Exception (End_Error'Identity);
         end if;
         Last := Item'First - 1;
         while Last < Item'Last loop
            if End_Of_Line (File) then
               Skip_Line (File);
               exit;
            else
               Last := Last + 1;
               Overloaded_Get (File, Item (Last));
            end if;
         end loop;
      end if;
   end Overloaded_Get_Line;

   procedure Overloaded_Get_Line (
      File : File_Type;
      Item : out Wide_Wide_String;
      Last : out Natural) is
   begin
      if Item'Length > 0 then
         if End_Of_File (File) then
            Raise_Exception (End_Error'Identity);
         end if;
         Last := Item'First - 1;
         while Last < Item'Last loop
            if End_Of_Line (File) then
               Skip_Line (File);
               exit;
            else
               Last := Last + 1;
               Overloaded_Get (File, Item (Last));
            end if;
         end loop;
      end if;
   end Overloaded_Get_Line;

   procedure Overloaded_Get_Line (
      Item : out String;
      Last : out Natural) is
   begin
      Overloaded_Get_Line (Current_Input.all, Item, Last);
   end Overloaded_Get_Line;

   procedure Overloaded_Get_Line (
      Item : out Wide_String;
      Last : out Natural) is
   begin
      Overloaded_Get_Line (Current_Input.all, Item, Last);
   end Overloaded_Get_Line;

   procedure Overloaded_Get_Line (
      Item : out Wide_Wide_String;
      Last : out Natural) is
   begin
      Overloaded_Get_Line (Current_Input.all, Item, Last);
   end Overloaded_Get_Line;

   procedure Get_Line (
      File : not null File_Access;
      Item : out String;
      Last : out Natural) is
   begin
      Get_Line (File.all, Item, Last);
   end Get_Line;

   procedure Overloaded_Get_Line (
      File : File_Type;
      Item : out String_Access)
   is
      Aliased_Item : aliased String_Access;
      Last : Natural;
      package Holder is
         new Exceptions.Finally.Scoped_Holder (
            String_Access,
            Finally);
   begin
      Holder.Assign (Aliased_Item'Access);
      Raw_Get_Line (File, Aliased_Item, Last);
      Reallocate (Aliased_Item, 1, Last);
      Holder.Clear;
      Item := Aliased_Item;
   end Overloaded_Get_Line;

   procedure Overloaded_Get_Line (
      File : File_Type;
      Item : out Wide_String_Access)
   is
      Aliased_Item : aliased Wide_String_Access;
      Last : Natural;
      package Holder is
         new Exceptions.Finally.Scoped_Holder (
            Wide_String_Access,
            Finally);
   begin
      Holder.Assign (Aliased_Item'Access);
      Raw_Get_Line (File, Aliased_Item, Last);
      Reallocate (Aliased_Item, 1, Last);
      Holder.Clear;
      Item := Aliased_Item;
   end Overloaded_Get_Line;

   procedure Overloaded_Get_Line (
      File : File_Type;
      Item : out Wide_Wide_String_Access)
   is
      Aliased_Item : aliased Wide_Wide_String_Access;
      Last : Natural;
      package Holder is
         new Exceptions.Finally.Scoped_Holder (
            Wide_Wide_String_Access,
            Finally);
   begin
      Holder.Assign (Aliased_Item'Access);
      Raw_Get_Line (File, Aliased_Item, Last);
      Reallocate (Aliased_Item, 1, Last);
      Holder.Clear;
      Item := Aliased_Item;
   end Overloaded_Get_Line;

   function Overloaded_Get_Line (File : File_Type) return String is
      Aliased_Item : aliased String_Access;
      Last : Natural;
      package Holder is
         new Exceptions.Finally.Scoped_Holder (
            String_Access,
            Finally);
   begin
      Holder.Assign (Aliased_Item'Access);
      Raw_Get_Line (File, Aliased_Item, Last);
      return Aliased_Item (Aliased_Item'First .. Last);
   end Overloaded_Get_Line;

   function Overloaded_Get_Line (File : File_Type) return Wide_String is
      Aliased_Item : aliased Wide_String_Access;
      Last : Natural;
      package Holder is
         new Exceptions.Finally.Scoped_Holder (
            Wide_String_Access,
            Finally);
   begin
      Holder.Assign (Aliased_Item'Access);
      Raw_Get_Line (File, Aliased_Item, Last);
      return Aliased_Item (Aliased_Item'First .. Last);
   end Overloaded_Get_Line;

   function Overloaded_Get_Line (File : File_Type) return Wide_Wide_String is
      Aliased_Item : aliased Wide_Wide_String_Access;
      Last : Natural;
      package Holder is
         new Exceptions.Finally.Scoped_Holder (
            Wide_Wide_String_Access,
            Finally);
   begin
      Holder.Assign (Aliased_Item'Access);
      Raw_Get_Line (File, Aliased_Item, Last);
      return Aliased_Item (Aliased_Item'First .. Last);
   end Overloaded_Get_Line;

   function Overloaded_Get_Line return String is
   begin
      return Overloaded_Get_Line (Current_Input.all);
   end Overloaded_Get_Line;

   function Overloaded_Get_Line return Wide_String is
   begin
      return Overloaded_Get_Line (Current_Input.all);
   end Overloaded_Get_Line;

   function Overloaded_Get_Line return Wide_Wide_String is
   begin
      return Overloaded_Get_Line (Current_Input.all);
   end Overloaded_Get_Line;

   procedure Overloaded_Put_Line (File : File_Type; Item : String) is
   begin
      Overloaded_Put (File, Item);
      New_Line (File);
   end Overloaded_Put_Line;

   procedure Overloaded_Put_Line (File : File_Type; Item : Wide_String) is
   begin
      Overloaded_Put (File, Item);
      New_Line (File);
   end Overloaded_Put_Line;

   procedure Overloaded_Put_Line (File : File_Type; Item : Wide_Wide_String) is
   begin
      Overloaded_Put (File, Item);
      New_Line (File);
   end Overloaded_Put_Line;

   procedure Overloaded_Put_Line (Item : String) is
   begin
      Overloaded_Put_Line (Current_Output.all, Item);
   end Overloaded_Put_Line;

   procedure Overloaded_Put_Line (Item : Wide_String) is
   begin
      Overloaded_Put_Line (Current_Output.all, Item);
   end Overloaded_Put_Line;

   procedure Overloaded_Put_Line (Item : Wide_Wide_String) is
   begin
      Overloaded_Put_Line (Current_Output.all, Item);
   end Overloaded_Put_Line;

   procedure Put_Line (File : not null File_Access; Item : String) is
   begin
      Put_Line (File.all, Item);
   end Put_Line;

   package body Controlled is

      Standard_Input_Object : aliased File_Type := (
         Finalization.Limited_Controlled with
         Text => Naked_Text_IO.Standard_Input);

      Standard_Output_Object : aliased File_Type := (
         Finalization.Limited_Controlled with
         Text => Naked_Text_IO.Standard_Output);

      Standard_Error_Object : aliased File_Type := (
         Finalization.Limited_Controlled with
         Text => Naked_Text_IO.Standard_Error);

      Current_Input : aliased File_Access := Standard_Input_Object'Access;
      Current_Output : aliased File_Access := Standard_Output_Object'Access;
      Current_Error : aliased File_Access := Standard_Error_Object'Access;

      --  implementation

      function Standard_Input return File_Access is
      begin
         return Standard_Input_Object'Access;
      end Standard_Input;

      function Standard_Output return File_Access is
      begin
         return Standard_Output_Object'Access;
      end Standard_Output;

      function Standard_Error return File_Access is
      begin
         return Standard_Error_Object'Access;
      end Standard_Error;

      function Reference_Current_Input return access File_Access is
      begin
         return Current_Input'Access;
      end Reference_Current_Input;

      function Reference_Current_Output return access File_Access is
      begin
         return Current_Output'Access;
      end Reference_Current_Output;

      function Reference_Current_Error return access File_Access is
      begin
         return Current_Error'Access;
      end Reference_Current_Error;

      function Reference (File : File_Type)
         return not null access Naked_Text_IO.Non_Controlled_File_Type is
      begin
         return File.Text'Unrestricted_Access;
      end Reference;

      overriding procedure Finalize (Object : in out File_Type) is
      begin
         pragma Check (Trace, Debug.Put ("enter"));
         Naked_Text_IO.Close (Reference (Object).all, Raise_On_Error => False);
         pragma Check (Trace, Debug.Put ("leave"));
      end Finalize;

   end Controlled;

end Ada.Text_IO;
