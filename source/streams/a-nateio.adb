with Ada.Exception_Identification.From_Here;
with Ada.Exceptions.Finally;
with Ada.Unchecked_Deallocation;
with System.Form_Parameters;
with System.UTF_Conversions;
package body Ada.Naked_Text_IO is
   use Exception_Identification.From_Here;
   use type IO_Modes.File_External;
   use type IO_Modes.File_External_Spec;
   use type IO_Modes.File_Mode;
   use type IO_Modes.File_New_Line;
   use type IO_Modes.File_New_Line_Spec;
   use type IO_Modes.File_SUB;
   use type Streams.Stream_Element_Offset;
   use type System.UTF_Conversions.UCS_4;

   procedure unreachable;
   pragma No_Return (unreachable);
   pragma Import (Intrinsic, unreachable, "__builtin_unreachable");

   --  the parameter Form

   function Select_External (Spec : IO_Modes.File_External_Spec)
      return IO_Modes.File_External;
   function Select_External (Spec : IO_Modes.File_External_Spec)
      return IO_Modes.File_External is
   begin
      case Spec is
         when IO_Modes.UTF_8 =>
            return IO_Modes.UTF_8;
         when IO_Modes.Locale =>
            declare
               Locale_Support : constant Boolean :=
                  System.Native_IO.Text_IO.Default_External =
                  IO_Modes.Locale;
            begin
               if Locale_Support then
                  return IO_Modes.Locale; -- Windows
               else
                  return IO_Modes.UTF_8; -- POSIX
               end if;
            end;
         when IO_Modes.By_Target =>
            return System.Native_IO.Text_IO.Default_External;
      end case;
   end Select_External;

   function Select_New_Line (Spec : IO_Modes.File_New_Line_Spec)
      return IO_Modes.File_New_Line;
   function Select_New_Line (Spec : IO_Modes.File_New_Line_Spec)
      return IO_Modes.File_New_Line is
   begin
      case Spec is
         when IO_Modes.LF | IO_Modes.CR | IO_Modes.CR_LF =>
            return IO_Modes.File_New_Line (Spec);
         when IO_Modes.By_Target =>
            return System.Native_IO.Text_IO.Default_New_Line;
      end case;
   end Select_New_Line;

   --  implementation of the parameter Form

   procedure Set (
      Form : in out System.Native_IO.Text_IO.Packed_Form;
      Keyword : String;
      Item : String) is
   begin
      if Keyword = "external" then
         if Item'Length > 0 and then Item (Item'First) = 'd' then -- dbcs
            Form.External := IO_Modes.Locale;
         elsif Item'Length > 0
            and then Item (Item'First) = 'u'
            and then Item (Item'Last) = '8'
         then -- utf-8
            Form.External := IO_Modes.UTF_8;
         end if;
      elsif Keyword = "wcem" then
         --  compatibility with GNAT runtime
         if Item'Length > 0 and then Item (Item'First) = '8' then
            Form.External := IO_Modes.UTF_8;
         end if;
      elsif Keyword = "nl" then -- abbr or new_line
         if Item'Length > 0 and then Item (Item'First) = 'l' then -- lf
            Form.New_Line := IO_Modes.LF;
         elsif Item'Length > 0 and then Item (Item'First) = 'c' then -- cr
            Form.New_Line := IO_Modes.CR;
         elsif Item'Length > 0 and then Item (Item'First) = 'm' then
            Form.New_Line := IO_Modes.CR_LF;
         end if;
      elsif Keyword = "sub" then
         if Item'Length > 0 and then Item (Item'First) = 'e' then -- eof
            Form.SUB := IO_Modes.End_Of_File;
         elsif Item'Length > 0 and then Item (Item'First) = 'o' then
            Form.SUB := IO_Modes.Ordinary;
         end if;
      else
         Streams.Naked_Stream_IO.Set (Form.Stream_Form, Keyword, Item);
      end if;
   end Set;

   function Pack (Form : String) return System.Native_IO.Text_IO.Packed_Form is
      Keyword_First : Positive;
      Keyword_Last : Natural;
      Item_First : Positive;
      Item_Last : Natural;
      Last : Natural;
   begin
      return Result : System.Native_IO.Text_IO.Packed_Form := Default_Form do
         Last := Form'First - 1;
         while Last < Form'Last loop
            System.Form_Parameters.Get (
               Form (Last + 1 .. Form'Last),
               Keyword_First,
               Keyword_Last,
               Item_First,
               Item_Last,
               Last);
            Set (
               Result,
               Form (Keyword_First .. Keyword_Last),
               Form (Item_First .. Item_Last));
         end loop;
      end return;
   end Pack;

   procedure Unpack (
      Form : System.Native_IO.Text_IO.Packed_Form;
      Result : out Streams.Naked_Stream_IO.Form_String;
      Last : out Natural)
   is
      subtype Valid_File_External_Spec is
         IO_Modes.File_External_Spec range IO_Modes.UTF_8 .. IO_Modes.Locale;
      New_Last : Natural;
   begin
      Streams.Naked_Stream_IO.Unpack (Form.Stream_Form, Result, Last);
      if Form.External /= IO_Modes.By_Target then
         if Last /= Streams.Naked_Stream_IO.Form_String'First - 1 then
            New_Last := Last + 1;
            Result (New_Last) := ',';
            Last := New_Last;
         end if;
         case Valid_File_External_Spec (Form.External) is
            when IO_Modes.UTF_8 =>
               New_Last := Last + 14;
               Result (Last + 1 .. New_Last) := "external=utf-8";
            when IO_Modes.Locale =>
               New_Last := Last + 13;
               Result (Last + 1 .. New_Last) := "external=dbcs";
         end case;
         Last := New_Last;
      end if;
      if Form.New_Line /= IO_Modes.By_Target then
         if Last /= Streams.Naked_Stream_IO.Form_String'First - 1 then
            New_Last := Last + 1;
            Result (New_Last) := ',';
            Last := New_Last;
         end if;
         case IO_Modes.File_New_Line (Form.New_Line) is
            when IO_Modes.LF =>
               New_Last := Last + 5;
               Result (Last + 1 .. New_Last) := "lm=lf";
               Last := New_Last;
            when IO_Modes.CR =>
               New_Last := Last + 5;
               Result (Last + 1 .. New_Last) := "lm=cr";
               Last := New_Last;
            when IO_Modes.CR_LF =>
               New_Last := Last + 4;
               Result (Last + 1 .. New_Last) := "lm=m";
               Last := New_Last;
         end case;
      end if;
      if Form.SUB /= IO_Modes.Ordinary then
         if Last /= Streams.Naked_Stream_IO.Form_String'First - 1 then
            New_Last := Last + 1;
            Result (New_Last) := ',';
            Last := New_Last;
         end if;
         New_Last := Last + 7;
         Result (Last + 1 .. New_Last) := "sub=eof";
         Last := New_Last;
      end if;
   end Unpack;

   --  non-controlled

   procedure Free is
      new Unchecked_Deallocation (Text_Type, Non_Controlled_File_Type);

   procedure Finally (X : not null access Non_Controlled_File_Type);
   procedure Finally (X : not null access Non_Controlled_File_Type) is
   begin
      Free (X.all);
   end Finally;

   procedure Check_File_Mode (
      File : Non_Controlled_File_Type;
      Expected : IO_Modes.File_Mode;
      Line : Natural := Ada.Debug.Line);
   procedure Check_File_Open (
      File : Non_Controlled_File_Type;
      Line : Natural := Ada.Debug.Line);
   procedure Check_Stream_IO_Open (
      File : Non_Controlled_File_Type;
      Line : Natural := Ada.Debug.Line);

   procedure Check_File_Mode (
      File : Non_Controlled_File_Type;
      Expected : IO_Modes.File_Mode;
      Line : Natural := Ada.Debug.Line) is
   begin
      if (Mode (File) = IO_Modes.In_File) /= (Expected = IO_Modes.In_File) then
         Raise_Exception (Mode_Error'Identity, Line => Line);
      end if;
   end Check_File_Mode;

   procedure Check_File_Open (
      File : Non_Controlled_File_Type;
      Line : Natural := Ada.Debug.Line) is
   begin
      if not Is_Open (File) then
         Raise_Exception (Status_Error'Identity, Line => Line);
      end if;
   end Check_File_Open;

   procedure Check_Stream_IO_Open (
      File : Non_Controlled_File_Type;
      Line : Natural := Ada.Debug.Line) is
   begin
      if not Is_Open (File)
         or else not Streams.Naked_Stream_IO.Is_Open (File.File)
      then
         Raise_Exception (Status_Error'Identity, Line => Line);
      end if;
   end Check_Stream_IO_Open;

   type Open_Access is not null access procedure (
      File : in out Streams.Naked_Stream_IO.Non_Controlled_File_Type;
      Mode : IO_Modes.File_Mode;
      Name : String;
      Form : System.Native_IO.Packed_Form);

   procedure Open_File (
      Open_Proc : Open_Access;
      File : in out Non_Controlled_File_Type;
      Mode : IO_Modes.File_Mode;
      Name : String;
      Form : System.Native_IO.Text_IO.Packed_Form);
   procedure Open_File (
      Open_Proc : Open_Access;
      File : in out Non_Controlled_File_Type;
      Mode : IO_Modes.File_Mode;
      Name : String;
      Form : System.Native_IO.Text_IO.Packed_Form)
   is
      New_File : aliased Non_Controlled_File_Type := new Text_Type'(
         Name_Length => 0,
         Stream => <>,
         Mode => Mode,
         External => <>,
         New_Line => <>,
         SUB => <>,
         Name => "",
         others => <>);
      package Holder is
         new Exceptions.Finally.Scoped_Holder (
            Non_Controlled_File_Type,
            Finally);
   begin
      Holder.Assign (New_File'Access);
      --  open
      Open_Proc (
         File => New_File.File,
         Mode => Mode,
         Name => Name,
         Form => Form.Stream_Form);
      declare
         function To_Address (Value : access Streams.Root_Stream_Type'Class)
            return System.Address;
         pragma Import (Intrinsic, To_Address);
      begin
         New_File.Stream := To_Address (
            Streams.Naked_Stream_IO.Stream (New_File.File));
      end;
      --  select encoding
      if System.Native_IO.Is_Terminal (
         Streams.Naked_Stream_IO.Handle (New_File.File))
      then
         New_File.External := IO_Modes.Terminal;
         New_File.New_Line := System.Native_IO.Text_IO.Default_New_Line;
         New_File.SUB := IO_Modes.Ordinary;
      else
         New_File.External := Select_External (Form.External);
         New_File.New_Line := Select_New_Line (Form.New_Line);
         New_File.SUB := Form.SUB;
      end if;
      --  complete
      Holder.Clear;
      File := New_File;
   end Open_File;

   function Unchecked_Stream (File : Non_Controlled_File_Type)
      return not null access Streams.Root_Stream_Type'Class;
   function Unchecked_Stream (File : Non_Controlled_File_Type)
      return not null access Streams.Root_Stream_Type'Class
   is
      function To_Pointer (Value : System.Address)
         return access Streams.Root_Stream_Type'Class;
      pragma Import (Intrinsic, To_Pointer);
   begin
      return To_Pointer (File.Stream).all'Unchecked_Access;
   end Unchecked_Stream;

   --  Input
   --  * Read_Buffer sets (or keeps) Ahead_Col.
   --  * Get adds Ahead_Col to current Col.
   --  * Take_Buffer clears Ahead_Col.

   procedure Read_Buffer (
      File : Non_Controlled_File_Type;
      Wanted : Positive := 1;
      Wait : Boolean := True);
   procedure Take_Buffer (File : Non_Controlled_File_Type);
   procedure Take_Page (File : Non_Controlled_File_Type);
   procedure Take_Line (File : Non_Controlled_File_Type);

   procedure Read_Buffer (
      File : Non_Controlled_File_Type;
      Wanted : Positive := 1;
      Wait : Boolean := True) is
   begin
      if not File.End_Of_File and then File.Ahead_Last < Wanted then
         if File.External = IO_Modes.Terminal then
            declare
               Read_Length : Streams.Stream_Element_Offset;
            begin
               if Wait then
                  System.Native_IO.Text_IO.Terminal_Get (
                     Streams.Naked_Stream_IO.Handle (File.File),
                     File.Buffer (File.Last + 1)'Address,
                     1,
                     Read_Length); -- Read_Length can be > 1
               else
                  System.Native_IO.Text_IO.Terminal_Get_Immediate (
                     Streams.Naked_Stream_IO.Handle (File.File),
                     File.Buffer (File.Last + 1)'Address,
                     1,
                     Read_Length); -- Read_Length can be > 1
               end if;
               if Read_Length < 0 then
                  Raise_Exception (Device_Error'Identity);
               end if;
               File.Last := File.Last + Natural (Read_Length);
            end;
         else
            --  read next single character
            declare
               Old_Last : constant Natural := File.Last;
               Buffer : Streams.Stream_Element_Array (
                  Streams.Stream_Element_Offset (Old_Last + 1) ..
                  Streams.Stream_Element_Offset (Old_Last + 1));
               for Buffer'Address use File.Buffer (Old_Last + 1)'Address;
               Last : Streams.Stream_Element_Offset;
            begin
               Streams.Read (Unchecked_Stream (File).all, Buffer, Last);
               File.Last := Natural'Base (Last);
               if Wait and then File.Last = Old_Last then
                  File.End_Of_File := True;
               end if;
            exception
               when End_Error =>
                  File.End_Of_File := True;
            end;
         end if;
      end if;
      if File.Last > 0 and then File.Ahead_Last = 0 then
         if File.External = IO_Modes.Terminal then
            File.Ahead_Last := File.Last;
         elsif File.External = IO_Modes.Locale
            and then File.Buffer (1) >= Character'Val (16#80#)
         then
            declare
               Locale_Support : constant Boolean :=
                  System.Native_IO.Text_IO.Default_External = IO_Modes.Locale;
            begin
               if not Locale_Support then
                  unreachable;
               end if;
            end;
            declare
               DBCS_Buffer : aliased
                  System.Native_IO.Text_IO.DBCS_Buffer_Type;
               New_Buffer : System.Native_IO.Text_IO.Buffer_Type;
               New_Ahead_Last : Natural;
               New_Last : Natural;
            begin
               DBCS_Buffer (1) := File.Buffer (1);
               DBCS_Buffer (2) := File.Buffer (2);
               System.Native_IO.Text_IO.To_UTF_8 (
                  DBCS_Buffer,
                  File.Last,
                  New_Buffer,
                  New_Ahead_Last);
               if New_Ahead_Last > 0 then
                  New_Last := New_Ahead_Last + (File.Last - 2);
                  File.Buffer (New_Ahead_Last + 1 .. New_Last) :=
                     File.Buffer (3 .. File.Last);
                  File.Buffer (1 .. New_Ahead_Last) :=
                     New_Buffer (1 .. New_Ahead_Last);
                  File.Last := New_Last;
                  File.Ahead_Last := New_Ahead_Last;
                  File.Ahead_Col := 2;
               elsif File.End_Of_File then
                  --  expected trailing byte is missing
                  File.Ahead_Last := File.Last;
                  File.Ahead_Col := File.Last;
               end if;
            end;
         else
            File.Ahead_Last := 1;
            File.Ahead_Col := 1;
         end if;
      end if;
   end Read_Buffer;

   procedure Take_Buffer (File : Non_Controlled_File_Type) is
      New_Last : constant Natural := File.Last - 1;
   begin
      File.Buffer (1 .. New_Last) := File.Buffer (2 .. File.Last);
      File.Last := New_Last;
      File.Ahead_Last := File.Ahead_Last - 1;
      File.Ahead_Col := 0;
      File.Dummy_Mark := None;
   end Take_Buffer;

   procedure Take_Page (File : Non_Controlled_File_Type) is
   begin
      Take_Buffer (File);
      File.Dummy_Mark := EOP;
      File.Page := File.Page + 1;
      File.Line := 1;
      File.Col := 1;
   end Take_Page;

   procedure Take_Line (File : Non_Controlled_File_Type) is
      C : constant Character := File.Buffer (1);
   begin
      File.Line := File.Line + 1;
      File.Col := 1;
      Take_Buffer (File);
      if C = Character'Val (16#0d#) then
         Read_Buffer (File);
         if File.Buffer (1) = Character'Val (16#0a#) then
            Take_Buffer (File);
         end if;
      end if;
   end Take_Line;

   procedure Get_Immediate (
      File : Non_Controlled_File_Type;
      Item : out String; -- 1 .. 6
      Last : out Natural;
      Wait : Boolean);
   procedure Get_Immediate (
      File : Non_Controlled_File_Type;
      Item : out String; -- 1 .. 6
      Last : out Natural;
      Wait : Boolean)
   is
      Handle : System.Native_IO.Handle_Type;
      Old_Settings : aliased System.Native_IO.Text_IO.Setting;
   begin
      Check_File_Mode (File, IO_Modes.In_File);
      if File.External = IO_Modes.Terminal then
         Handle := Streams.Naked_Stream_IO.Handle (File.File);
         System.Native_IO.Text_IO.Set_Non_Canonical_Mode (
            Handle,
            Wait, -- only POSIX
            Old_Settings);
      end if;
      Last := Item'First - 1;
      Multi_Character : for I in Item'Range loop
         Single_Character : loop
            Read_Buffer (File, Wait => Wait);
            if File.Ahead_Last > 0 then
               Item (I) := File.Buffer (1);
               Last := I;
               File.Col := File.Col + File.Ahead_Col;
               Take_Buffer (File); -- not add File.Text.Col
               exit Single_Character; -- next character
            elsif File.End_Of_File then
               Raise_Exception (End_Error'Identity);
            elsif not Wait then
               exit Multi_Character;
            end if;
         end loop Single_Character;
      end loop Multi_Character;
      if File.External = IO_Modes.Terminal then
         System.Native_IO.Text_IO.Restore (Handle, Old_Settings);
      end if;
   end Get_Immediate;

   procedure Look_Ahead_Trailing (
      File : Non_Controlled_File_Type;
      Item : out String; -- 1 .. 6
      Last : out Natural);
   procedure Store_Second (
      File : Non_Controlled_File_Type;
      C : Wide_Wide_Character;
      Item : out Wide_Character);
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

   procedure Look_Ahead_Trailing (
      File : Non_Controlled_File_Type;
      Item : out String; -- 1 .. 6
      Last : out Natural)
   is
      Wanted_Length : constant Natural := Item'Length;
      Buffer_Last : Natural;
   begin
      --  Check_File_Mode is omitted
      --  this subprogram should be called after single-character Look_Ahead
      while not File.End_Of_File and then File.Ahead_Last < Wanted_Length loop
         Read_Buffer (File, Wanted_Length);
      end loop;
      Buffer_Last := File.Last; -- File.Last > 0
      for I in 1 .. File.Last loop
         if File.Buffer (I) = Character'Val (16#0d#)
            or else File.Buffer (I) = Character'Val (16#0a#)
            or else File.Buffer (I) = Character'Val (16#0c#)
            or else (
               File.SUB = IO_Modes.End_Of_File
                  and then File.Buffer (I) = Character'Val (16#1a#))
         then
            Buffer_Last := I - 1;
            exit;
         end if;
      end loop;
      Last := Item'First + Buffer_Last - 1;
      Item (Item'First .. Last) := File.Buffer (1 .. Buffer_Last);
   end Look_Ahead_Trailing;

   procedure Store_Second (
      File : Non_Controlled_File_Type;
      C : Wide_Wide_Character;
      Item : out Wide_Character)
   is
      W_Buffer : Wide_String (1 .. System.UTF_Conversions.UTF_16_Max_Length);
      W_Last : Natural;
      To_Status : System.UTF_Conversions.To_Status_Type; -- ignore
   begin
      System.UTF_Conversions.To_UTF_16 (
         Wide_Wide_Character'Pos (C),
         W_Buffer,
         W_Last,
         To_Status);
      Item := W_Buffer (1);
      if W_Last >= 2 then
         --  store second of surrogate pair
         declare
            Buffer : String (1 .. System.UTF_Conversions.UTF_8_Max_Length);
            Last : Natural;
         begin
            System.UTF_Conversions.To_UTF_8 (
               Wide_Character'Pos (W_Buffer (2)),
               Buffer,
               Last,
               To_Status);
            File.Last := Last;
            File.Ahead_Last := Last;
            File.Buffer (1 .. Last) := Buffer (1 .. Last);
         end;
      end if;
   end Store_Second;

   procedure Get_Immediate (
      File : Non_Controlled_File_Type;
      Item : out Wide_Character;
      Available : out Boolean;
      Wait : Boolean)
   is
      C : Wide_Wide_Character;
   begin
      Get_Immediate (File, C, Available, Wait); -- Wide_Wide
      if Available then
         Store_Second (File, C, Item);
      end if;
   end Get_Immediate;

   procedure Get_Immediate (
      File : Non_Controlled_File_Type;
      Item : out Wide_Wide_Character;
      Available : out Boolean;
      Wait : Boolean)
   is
      S : String (1 .. System.UTF_Conversions.UTF_8_Max_Length);
      Read_Last : Natural;
      Length : Natural;
      From_Status : System.UTF_Conversions.From_Status_Type; -- ignore
   begin
      Get_Immediate (
         File,
         S (1 .. 1),
         Read_Last,
         Wait);
      Available := Read_Last > 0;
      System.UTF_Conversions.UTF_8_Sequence (S (1), Length, From_Status);
      declare
         Last : Natural;
         Code : System.UTF_Conversions.UCS_4;
      begin
         if Length >= 2 then
            Get_Immediate (
               File,
               S (2 .. Length),
               Read_Last,
               Wait => True);
         end if;
         System.UTF_Conversions.From_UTF_8 (
            S (1 .. Length),
            Last,
            Code,
            From_Status);
         Item := Wide_Wide_Character'Val (Code);
      end;
   end Get_Immediate;

   --  Output
   --  * Write_Buffer sets Ahead_Col to written width.
   --  * Put adds Ahead_Col to current Col.

   procedure Raw_New_Page (File : Non_Controlled_File_Type);
   procedure Raw_New_Line (File : Non_Controlled_File_Type);
   procedure Raw_New_Line (
      File : Non_Controlled_File_Type;
      Spacing : Positive);

   procedure Write_Buffer (
      File : Non_Controlled_File_Type;
      Sequence_Length : Natural);

   procedure Raw_New_Page (File : Non_Controlled_File_Type) is
   begin
      if File.External = IO_Modes.Terminal then
         System.Native_IO.Text_IO.Terminal_Clear (
            Streams.Naked_Stream_IO.Handle (File.File));
      else
         declare
            Code : constant Streams.Stream_Element_Array := (1 => 16#0c#);
         begin
            Streams.Write (Unchecked_Stream (File).all, Code);
         end;
      end if;
      File.Page := File.Page + 1;
      File.Line := 1;
      File.Col := 1;
   end Raw_New_Page;

   procedure Raw_New_Line (File : Non_Controlled_File_Type) is
   begin
      if File.Page_Length /= 0 and then File.Line >= File.Page_Length then
         Raw_New_Page (File);
      else
         declare
            Line_Mark : constant Streams.Stream_Element_Array (0 .. 1) :=
               (16#0d#, 16#0a#);
            F, L : Streams.Stream_Element_Offset;
         begin
            F := Boolean'Pos (File.New_Line = IO_Modes.LF);
            L := Boolean'Pos (File.New_Line /= IO_Modes.CR);
            Streams.Write (Unchecked_Stream (File).all, Line_Mark (F .. L));
         end;
         File.Line := File.Line + 1;
         File.Col := 1;
      end if;
   end Raw_New_Line;

   procedure Raw_New_Line (
      File : Non_Controlled_File_Type;
      Spacing : Positive) is
   begin
      for I in 1 .. Spacing loop
         Raw_New_Line (File);
      end loop;
   end Raw_New_Line;

   procedure Write_Buffer (
      File : Non_Controlled_File_Type;
      Sequence_Length : Natural)
   is
      Length : constant Natural := File.Last; -- >= Sequence_Length
   begin
      if File.External = IO_Modes.Terminal then
         declare
            Written_Length : Streams.Stream_Element_Offset;
         begin
            System.Native_IO.Text_IO.Terminal_Put (
               Streams.Naked_Stream_IO.Handle (File.File),
               File.Buffer'Address,
               Streams.Stream_Element_Offset (Sequence_Length),
               Written_Length);
            if Written_Length < 0 then
               Raise_Exception (Device_Error'Identity);
            end if;
         end;
      elsif File.External = IO_Modes.Locale
         and then File.Buffer (1) >= Character'Val (16#80#)
      then
         declare
            Locale_Support : constant Boolean :=
               System.Native_IO.Text_IO.Default_External =
               IO_Modes.Locale;
         begin
            if not Locale_Support then
               unreachable;
            end if;
         end;
         declare
            DBCS_Buffer : aliased System.Native_IO.Text_IO.DBCS_Buffer_Type;
            DBCS_Last : Natural;
         begin
            System.Native_IO.Text_IO.To_DBCS (
               File.Buffer,
               Sequence_Length,
               DBCS_Buffer,
               DBCS_Last);
            if DBCS_Last = 0 then
               DBCS_Buffer (1) := '?';
               DBCS_Last := 1;
            end if;
            if File.Line_Length /= 0
               and then File.Col + Natural (DBCS_Last - 1) > File.Line_Length
            then
               Raw_New_Line (File);
            end if;
            declare
               Buffer : Streams.Stream_Element_Array (
                  1 ..
                  Streams.Stream_Element_Offset (DBCS_Last));
               for Buffer'Address use DBCS_Buffer'Address;
            begin
               Streams.Write (Unchecked_Stream (File).all, Buffer);
            end;
            File.Ahead_Col := DBCS_Last;
         end;
      else
         if File.Line_Length /= 0
            and then File.Col + Sequence_Length - 1 > File.Line_Length
         then
            Raw_New_Line (File);
         end if;
         declare
            Buffer : Streams.Stream_Element_Array (
               1 ..
               Streams.Stream_Element_Offset (Sequence_Length));
            for Buffer'Address use File.Buffer'Address;
         begin
            Streams.Write (Unchecked_Stream (File).all, Buffer);
         end;
         File.Ahead_Col := Sequence_Length;
      end if;
      File.Last := Length - Sequence_Length;
      File.Buffer (1 .. File.Last) :=
         File.Buffer (Sequence_Length + 1 .. Length);
   end Write_Buffer;

   --  implementation of non-controlled

   procedure Create (
      File : in out Non_Controlled_File_Type;
      Mode : IO_Modes.File_Mode := IO_Modes.Out_File;
      Name : String := "";
      Form : System.Native_IO.Text_IO.Packed_Form := Default_Form) is
   begin
      if Is_Open (File) then
         Raise_Exception (Status_Error'Identity);
      end if;
      Open_File (
         Open_Proc => Streams.Naked_Stream_IO.Create'Access,
         File => File,
         Mode => Mode,
         Name => Name,
         Form => Form);
   end Create;

   procedure Open (
      File : in out Non_Controlled_File_Type;
      Mode : IO_Modes.File_Mode;
      Name : String;
      Form : System.Native_IO.Text_IO.Packed_Form := Default_Form) is
   begin
      if Is_Open (File) then
         Raise_Exception (Status_Error'Identity);
      end if;
      Open_File (
         Open_Proc => Streams.Naked_Stream_IO.Open'Access,
         File => File,
         Mode => Mode,
         Name => Name,
         Form => Form);
   end Open;

   procedure Close (
      File : aliased in out Non_Controlled_File_Type;
      Raise_On_Error : Boolean := True) is
   begin
      if Is_Open (File) then
         declare
            Internal : aliased
               Streams.Naked_Stream_IO.Non_Controlled_File_Type :=
               File.File;
         begin
            if not Streams.Naked_Stream_IO.Is_Standard (Internal) then
               Free (File);
            end if;
            if Streams.Naked_Stream_IO.Is_Open (Internal) then
               Streams.Naked_Stream_IO.Close (
                  Internal,
                  Raise_On_Error => Raise_On_Error);
            end if;
         end;
      elsif Raise_On_Error then
         Raise_Exception (Status_Error'Identity);
      end if;
   end Close;

   procedure Delete (File : aliased in out Non_Controlled_File_Type) is
   begin
      Check_File_Open (File);
      declare
         Internal : aliased
            Streams.Naked_Stream_IO.Non_Controlled_File_Type :=
            File.File;
      begin
         Free (File);
         Streams.Naked_Stream_IO.Delete (Internal);
      end;
   end Delete;

   procedure Reset (
      File : aliased in out Non_Controlled_File_Type;
      Mode : IO_Modes.File_Mode)
   is
      Current_Mode : constant IO_Modes.File_Mode := Naked_Text_IO.Mode (File);
   begin
      if Current_Mode /= Mode
         and then Streams.Naked_Stream_IO.Is_Standard (File.File)
      then
         Raise_Exception (Mode_Error'Identity);
      elsif not Streams.Naked_Stream_IO.Is_Open (File.File) then
         --  External stream mode
         Raise_Exception (Status_Error'Identity);
      else
         if Current_Mode /= IO_Modes.In_File then
            Flush (File);
         end if;
         declare
            package Holder is
               new Exceptions.Finally.Scoped_Holder (
                  Non_Controlled_File_Type,
                  Finally);
         begin
            Holder.Assign (File'Access);
            Streams.Naked_Stream_IO.Reset (File.File, Mode);
            Holder.Clear;
         end;
         declare
            function To_Address (Value : access Streams.Root_Stream_Type'Class)
               return System.Address;
            pragma Import (Intrinsic, To_Address);
         begin
            File.Stream := To_Address (
               Streams.Naked_Stream_IO.Stream (File.File));
         end;
         File.Page := 1;
         File.Line := 1;
         File.Col := 1;
         File.Line_Length := 0;
         File.Page_Length := 0;
         File.Ahead_Col := 0;
         File.Ahead_Last := 0;
         File.Last := 0;
         File.End_Of_File := False;
         File.Dummy_Mark := None;
         File.Mode := Mode;
      end if;
   end Reset;

   function Mode (File : Non_Controlled_File_Type) return IO_Modes.File_Mode is
   begin
      Check_File_Open (File);
      if Streams.Naked_Stream_IO.Is_Open (File.File) then
         return Streams.Naked_Stream_IO.Mode (File.File);
      else
         return File.Mode;
      end if;
   end Mode;

   function Name (File : Non_Controlled_File_Type) return String is
   begin
      Check_File_Open (File);
      if Streams.Naked_Stream_IO.Is_Open (File.File) then
         return Streams.Naked_Stream_IO.Name (File.File);
      else
         return File.Name;
      end if;
   end Name;

   function Form (File : Non_Controlled_File_Type)
      return System.Native_IO.Text_IO.Packed_Form is
   begin
      Check_File_Open (File);
      declare
         Stream_Form : System.Native_IO.Packed_Form;
         External : IO_Modes.File_External_Spec;
      begin
         if Streams.Naked_Stream_IO.Is_Open (File.File) then
            Stream_Form := Streams.Naked_Stream_IO.Form (File.File);
         else
            Stream_Form := Streams.Naked_Stream_IO.Default_Form;
         end if;
         if File.External = IO_Modes.Terminal then
            External := IO_Modes.File_External_Spec (
               System.Native_IO.Text_IO.Default_External);
         else
            External := IO_Modes.File_External_Spec (File.External);
         end if;
         return (
            Stream_Form,
            External,
            IO_Modes.File_New_Line_Spec (File.New_Line),
            File.SUB);
      end;
   end Form;

   function External (File : Non_Controlled_File_Type)
      return IO_Modes.File_External is
   begin
      Check_File_Open (File);
      return File.External;
   end External;

   function Is_Open (File : Non_Controlled_File_Type) return Boolean is
   begin
      return File /= null;
   end Is_Open;

   procedure Flush (File : Non_Controlled_File_Type) is
   begin
      Check_File_Mode (File, IO_Modes.Out_File);
      if File.Last > 0 then
         Write_Buffer (File, File.Last);
         File.Col := File.Col + File.Ahead_Col;
      end if;
      if Streams.Naked_Stream_IO.Is_Open (File.File)
         and then File.External /=
            IO_Modes.Terminal -- console can not flush
      then
         Streams.Naked_Stream_IO.Flush (File.File);
      end if;
   end Flush;

   procedure Set_Size (
      File : Non_Controlled_File_Type;
      Line_Length, Page_Length : Natural) is
   begin
      Check_File_Mode (File, IO_Modes.Out_File);
      if File.External = IO_Modes.Terminal then
         if Line_Length = 0 or else Page_Length = 0 then
            Raise_Exception (Device_Error'Identity);
         end if;
         System.Native_IO.Text_IO.Set_Terminal_Size (
            Streams.Naked_Stream_IO.Handle (File.File),
            Line_Length,
            Page_Length);
      else
         File.Line_Length := Line_Length;
         File.Page_Length := Page_Length;
      end if;
   end Set_Size;

   procedure Set_Line_Length (File : Non_Controlled_File_Type; To : Natural) is
      Current_Line_Length, Current_Page_Length : Natural;
   begin
      Size (File, Current_Line_Length, Current_Page_Length);
      if File.External /= IO_Modes.Terminal or else To > 0 then
         Set_Size (File, To, Current_Page_Length);
      end if;
   end Set_Line_Length;

   procedure Set_Page_Length (File : Non_Controlled_File_Type; To : Natural) is
      Current_Line_Length, Current_Page_Length : Natural;
   begin
      Size (File, Current_Line_Length, Current_Page_Length);
      if File.External /= IO_Modes.Terminal or else To > 0 then
         Set_Size (File, Current_Line_Length, To);
      end if;
   end Set_Page_Length;

   procedure Size (
      File : Non_Controlled_File_Type;
      Line_Length, Page_Length : out Natural) is
   begin
      Check_File_Mode (File, IO_Modes.Out_File);
      if File.External = IO_Modes.Terminal then
         System.Native_IO.Text_IO.Terminal_Size (
            Streams.Naked_Stream_IO.Handle (File.File),
            Line_Length,
            Page_Length);
      else
         Line_Length := File.Line_Length;
         Page_Length := File.Page_Length;
      end if;
   end Size;

   function Line_Length (File : Non_Controlled_File_Type) return Natural is
      Line_Length, Page_Length : Natural;
   begin
      Size (File, Line_Length, Page_Length);
      return Line_Length;
   end Line_Length;

   function Page_Length (File : Non_Controlled_File_Type) return Natural is
      Line_Length, Page_Length : Natural;
   begin
      Size (File, Line_Length, Page_Length);
      return Page_Length;
   end Page_Length;

   procedure New_Line (
      File : Non_Controlled_File_Type;
      Spacing : Positive := 1) is
   begin
      Check_File_Mode (File, IO_Modes.Out_File);
      if File.Last > 0 then
         Write_Buffer (File, File.Last);
      end if;
      Raw_New_Line (File, Spacing);
   end New_Line;

   procedure Skip_Line (
      File : Non_Controlled_File_Type;
      Spacing : Positive := 1) is
   begin
      Check_File_Mode (File, IO_Modes.In_File);
      for I in 1 .. Spacing loop
         loop
            Read_Buffer (File);
            if File.Last > 0 then -- ASCII
               declare
                  C : constant Character := File.Buffer (1);
               begin
                  case C is
                     when Character'Val (16#0d#) | Character'Val (16#0a#) =>
                        Take_Line (File);
                        exit;
                     when Character'Val (16#0c#) =>
                        Take_Page (File);
                        exit;
                     when Character'Val (16#1a#) =>
                        if File.SUB = IO_Modes.End_Of_File then
                           File.End_Of_File := True; -- for next loop
                           File.Ahead_Last := 0;
                           File.Last := 0;
                        else
                           File.Col := File.Col + File.Ahead_Col;
                           Take_Buffer (File);
                        end if;
                     when others =>
                        File.Col := File.Col + File.Ahead_Col;
                        Take_Buffer (File);
                  end case;
               end;
            elsif File.End_Of_File then
               if File.Dummy_Mark <= EOP then
                  File.Dummy_Mark := EOP_EOF;
                  File.Page := File.Page + 1;
                  File.Line := 1;
                  File.Col := 1;
                  exit;
               else
                  Raise_Exception (End_Error'Identity);
               end if;
            end if;
         end loop;
      end loop;
   end Skip_Line;

   function End_Of_Line (File : Non_Controlled_File_Type) return Boolean is
   begin
      if End_Of_File (File) then -- End_Of_File calls Read_Buffer
         return True;
      else
         return File.Last > 0 and then ( -- line mark is ASCII
            File.Buffer (1) = Character'Val (16#0d#)
            or else File.Buffer (1) = Character'Val (16#0a#)
            or else File.Buffer (1) = Character'Val (16#0c#)
            or else (
               File.SUB = IO_Modes.End_Of_File
                  and then File.Buffer (1) = Character'Val (16#1a#)));
      end if;
   end End_Of_Line;

   procedure New_Page (File : Non_Controlled_File_Type) is
   begin
      Check_File_Mode (File, IO_Modes.Out_File);
      if File.Last > 0 then
         Write_Buffer (File, File.Last);
      end if;
      Raw_New_Page (File);
   end New_Page;

   procedure Skip_Page (File : Non_Controlled_File_Type) is
   begin
      Check_File_Mode (File, IO_Modes.In_File);
      while not End_Of_Page (File) loop
         Skip_Line (File);
      end loop;
      case File.Dummy_Mark is
         when EOF =>
            Raise_Exception (End_Error'Identity);
         when EOP =>
            File.Dummy_Mark := None;
         when EOP_EOF =>
            File.Dummy_Mark := EOF;
         when others =>
            if End_Of_File (File) then
               File.Dummy_Mark := EOF;
            else
               Take_Buffer (File);
            end if;
            File.Page := File.Page + 1;
            File.Line := 1;
            File.Col := 1;
      end case;
   end Skip_Page;

   function End_Of_Page (File : Non_Controlled_File_Type) return Boolean is
   begin
      if End_Of_File (File) -- End_Of_File calls Read_Buffer
         or else File.Dummy_Mark = EOP
         or else File.Dummy_Mark = EOP_EOF
      then
         return True;
      else
         return File.Last > 0 -- page mark is ASCII
            and then File.Buffer (1) = Character'Val (16#0c#);
      end if;
   end End_Of_Page;

   function End_Of_File (File : Non_Controlled_File_Type) return Boolean is
   begin
      Check_File_Mode (File, IO_Modes.In_File);
      Read_Buffer (File);
      if File.Last > 0 then
         return False;
      else
         return File.End_Of_File;
      end if;
   end End_Of_File;

   procedure Set_Col (File : Non_Controlled_File_Type; To : Positive) is
   begin
      if Mode (File) = IO_Modes.In_File then
         --  In_File
         loop
            if End_Of_Page (File) then
               Skip_Page (File); -- raise End_Error when End_Of_File.
            elsif End_Of_Line (File) then
               Skip_Line (File);
            else
               exit when Col (File) = To;
               declare
                  C : Character;
               begin
                  Get (File, C);
               end;
            end if;
         end loop;
      else
         --  Out_File (or Append_File)
         if File.External = IO_Modes.Terminal then
            System.Native_IO.Text_IO.Set_Terminal_Col (
               Streams.Naked_Stream_IO.Handle (File.File),
               To);
         else
            if File.Line_Length /= 0 and then To > File.Line_Length then
               Raise_Exception (Layout_Error'Identity);
            end if;
            if File.Col > To then
               Raw_New_Line (File);
            end if;
            while File.Col < To loop
               Put (File, Character'Val (16#20#));
            end loop;
         end if;
      end if;
   end Set_Col;

   procedure Set_Line (File : Non_Controlled_File_Type; To : Positive) is
   begin
      if Mode (File) = IO_Modes.In_File then
         --  In_File
         while To /= File.Line or else End_Of_Page (File) loop
            Skip_Line (File);
         end loop;
      else
         --  Out_File (or Append_File)
         if File.External = IO_Modes.Terminal then
            System.Native_IO.Text_IO.Set_Terminal_Position (
               Streams.Naked_Stream_IO.Handle (File.File),
               Col => 1,
               Line => To);
         else
            if File.Page_Length /= 0 and then To > File.Page_Length then
               Raise_Exception (Layout_Error'Identity);
            end if;
            if File.Line > To then
               Raw_New_Page (File);
            end if;
            if File.Line < To then
               Raw_New_Line (File, To - File.Line);
            end if;
         end if;
      end if;
   end Set_Line;

   procedure Position (
      File : Non_Controlled_File_Type;
      Col, Line : out Positive) is
   begin
      Check_File_Open (File);
      if File.External = IO_Modes.Terminal then
         System.Native_IO.Text_IO.Terminal_Position (
            Streams.Naked_Stream_IO.Handle (File.File),
            Col,
            Line);
      else
         Col := File.Col;
         Line := File.Line;
      end if;
   end Position;

   function Col (File : Non_Controlled_File_Type) return Positive is
      Col, Line : Positive;
   begin
      Position (File, Col, Line);
      return Col;
   end Col;

   function Line (File : Non_Controlled_File_Type) return Positive is
      Col, Line : Positive;
   begin
      Position (File, Col, Line);
      return Line;
   end Line;

   function Page (File : Non_Controlled_File_Type) return Positive is
   begin
      Check_File_Open (File);
      return File.Page;
   end Page;

   procedure Get (
      File : Non_Controlled_File_Type;
      Item : out Character) is
   begin
      Check_File_Mode (File, IO_Modes.In_File);
      loop
         Read_Buffer (File);
         if File.Ahead_Last > 0 then
            declare
               C : constant Character := File.Buffer (1);
            begin
               case C is
                  when Character'Val (16#0d#) | Character'Val (16#0a#) =>
                     Take_Line (File);
                  when Character'Val (16#0c#) =>
                     Take_Page (File);
                  when Character'Val (16#1a#) =>
                     if File.SUB = IO_Modes.End_Of_File then
                        File.End_Of_File := True; -- for next loop
                        File.Ahead_Last := 0;
                        File.Last := 0;
                     else
                        Item := C;
                        File.Col := File.Col + File.Ahead_Col;
                        Take_Buffer (File);
                        exit;
                     end if;
                  when others =>
                     Item := C;
                     File.Col := File.Col + File.Ahead_Col;
                     Take_Buffer (File);
                     exit;
               end case;
            end;
         elsif File.End_Of_File then
            Raise_Exception (End_Error'Identity);
         end if;
      end loop;
   end Get;

   procedure Get (
      File : Non_Controlled_File_Type;
      Item : out Wide_Character)
   is
      C : Wide_Wide_Character;
   begin
      Get (File, C); -- Wide_Wide
      Store_Second (File, C, Item);
   end Get;

   procedure Get (
      File : Non_Controlled_File_Type;
      Item : out Wide_Wide_Character)
   is
      S : String (1 .. System.UTF_Conversions.UTF_8_Max_Length);
      Length : Natural;
      From_Status : System.UTF_Conversions.From_Status_Type; -- ignore
   begin
      Get (File, S (1));
      System.UTF_Conversions.UTF_8_Sequence (S (1), Length, From_Status);
      declare
         Last : Natural;
         Code : System.UTF_Conversions.UCS_4;
      begin
         if Length >= 2 then
            for I in 2 .. Length loop
               Get (File, S (I));
            end loop;
         end if;
         System.UTF_Conversions.From_UTF_8 (
            S (1 .. Length),
            Last,
            Code,
            From_Status);
         Item := Wide_Wide_Character'Val (Code);
      end;
   end Get;

   procedure Put (
      File : Non_Controlled_File_Type;
      Item : Character)
   is
      Sequence_Length : Natural;
      Sequence_Status : System.UTF_Conversions.Sequence_Status_Type; -- ignore
   begin
      Check_File_Mode (File, IO_Modes.Out_File);
      --  if Item is not trailing byte, flush the buffer
      if (File.Line_Length /= 0 or else File.External /= IO_Modes.UTF_8)
         and then File.Last > 0
         and then Character'Pos (Item) not in 2#10000000# .. 2#10111111#
      then
         Write_Buffer (File, File.Last);
         File.Col := File.Col + File.Ahead_Col;
      end if;
      --  write to the buffer
      File.Last := File.Last + 1;
      File.Buffer (File.Last) := Item;
      if File.Line_Length /= 0 or else File.External /= IO_Modes.UTF_8 then
         System.UTF_Conversions.UTF_8_Sequence (
            File.Buffer (1),
            Sequence_Length,
            Sequence_Status);
         if File.Last >= Sequence_Length then
            Write_Buffer (File, Sequence_Length);
            File.Col := File.Col + File.Ahead_Col;
         end if;
      else
         Write_Buffer (File, File.Last);
         File.Col := File.Col + File.Ahead_Col;
      end if;
   end Put;

   procedure Put (
      File : Non_Controlled_File_Type;
      Item : Wide_Character)
   is
      From_Status : System.UTF_Conversions.From_Status_Type; -- ignore
   begin
      if File.Last > 0 then
         declare
            First : System.UTF_Conversions.UCS_4;
            Code : System.UTF_Conversions.UCS_4;
         begin
            declare
               Last : Natural;
               Length : Natural;
            begin
               --  restore first of surrogate pair
               System.UTF_Conversions.From_UTF_8 (
                  File.Buffer (1 .. File.Last),
                  Last,
                  First,
                  From_Status);
               System.UTF_Conversions.UTF_16_Sequence (
                  Wide_Character'Val (First),
                  Length,
                  From_Status);
               if Length /= 2
                  or else First >= 16#ffff#
                  or else Last /= File.Last
               then
                  --  previous data is wrong
                  Raise_Exception (Data_Error'Identity);
               end if;
            end;
            declare
               Wide_Buffer : Wide_String (1 .. 2);
               Wide_Last : Natural;
            begin
               Wide_Buffer (1) := Wide_Character'Val (First);
               Wide_Buffer (2) := Item;
               System.UTF_Conversions.From_UTF_16 (
                  Wide_Buffer,
                  Wide_Last,
                  Code,
                  From_Status);
            end;
            File.Last := 0;
            Put (File, Wide_Wide_Character'Val (Code));
         end;
      else
         declare
            Length : Natural;
            To_Status : System.UTF_Conversions.To_Status_Type; -- ignore
         begin
            System.UTF_Conversions.UTF_16_Sequence (Item, Length, From_Status);
            if Length = 2 then
               --  store first of surrogate pair
               System.UTF_Conversions.To_UTF_8 (
                  Wide_Character'Pos (Item),
                  File.Buffer,
                  File.Last,
                  To_Status);
            else
               --  single character
               Put (File, Wide_Wide_Character'Val (Wide_Character'Pos (Item)));
            end if;
         end;
      end if;
   end Put;

   procedure Put (
      File : Non_Controlled_File_Type;
      Item : Wide_Wide_Character) is
   begin
      if File.Last > 0 then
         --  previous data is rested
         Raise_Exception (Data_Error'Identity);
      else
         declare
            Buffer : String (1 .. System.UTF_Conversions.UTF_8_Max_Length);
            Last : Natural;
            To_Status : System.UTF_Conversions.To_Status_Type; -- ignore
         begin
            System.UTF_Conversions.To_UTF_8 (
               Wide_Wide_Character'Pos (Item),
               Buffer,
               Last,
               To_Status);
            for I in 1 .. Last loop
               Put (File, Buffer (I));
            end loop;
         end;
      end if;
   end Put;

   procedure Look_Ahead (
      File : Non_Controlled_File_Type;
      Item : out Character;
      End_Of_Line : out Boolean) is
   begin
      Check_File_Mode (File, IO_Modes.In_File);
      loop
         if Naked_Text_IO.End_Of_Line (File) then -- calls Read_Buffer
            End_Of_Line := True;
            Item := Character'Val (0);
            exit;
         elsif File.Ahead_Last > 0 then
            End_Of_Line := False;
            Item := File.Buffer (1);
            exit;
         end if;
      end loop;
   end Look_Ahead;

   procedure Look_Ahead (
      File : Non_Controlled_File_Type;
      Item : out Wide_Character;
      End_Of_Line : out Boolean)
   is
      C : Wide_Wide_Character;
   begin
      Look_Ahead (File, C, End_Of_Line); -- Wide_Wide
      if End_Of_Line then
         Item := Wide_Character'Val (0);
      else
         declare
            Wide_Buffer : Wide_String (1 .. 2);
            Wide_Last : Natural;
            To_Status : System.UTF_Conversions.To_Status_Type; -- ignore
         begin
            System.UTF_Conversions.To_UTF_16 (
               Wide_Wide_Character'Pos (C),
               Wide_Buffer,
               Wide_Last,
               To_Status);
            Item := Wide_Buffer (1);
         end;
      end if;
   end Look_Ahead;

   procedure Look_Ahead (
      File : Non_Controlled_File_Type;
      Item : out Wide_Wide_Character;
      End_Of_Line : out Boolean)
   is
      C : Character;
   begin
      Look_Ahead (File, C, End_Of_Line);
      if End_Of_Line then
         Item := Wide_Wide_Character'Val (0);
      else
         declare
            Length : Natural;
            From_Status : System.UTF_Conversions.From_Status_Type; -- ignore
         begin
            System.UTF_Conversions.UTF_8_Sequence (C, Length, From_Status);
            declare
               S : String (1 .. Length);
               Last : Natural;
               Conv_Last : Natural;
               Code : System.UTF_Conversions.UCS_4;
            begin
               Look_Ahead_Trailing (File, S, Last);
               System.UTF_Conversions.From_UTF_8 (
                  S (1 .. Last),
                  Conv_Last,
                  Code,
                  From_Status);
               Item := Wide_Wide_Character'Val (Code);
            end;
         end;
      end if;
   end Look_Ahead;

   procedure Get_Immediate (
      File : Non_Controlled_File_Type;
      Item : out Character)
   is
      S : String (1 .. 1);
      Last : Natural;
   begin
      Get_Immediate (File, S, Last, Wait => True);
      Item := S (1);
   end Get_Immediate;

   procedure Get_Immediate (
      File : Non_Controlled_File_Type;
      Item : out Wide_Character)
   is
      Available : Boolean;
   begin
      Get_Immediate (File, Item, Available, Wait => True);
   end Get_Immediate;

   procedure Get_Immediate (
      File : Non_Controlled_File_Type;
      Item : out Wide_Wide_Character)
   is
      Available : Boolean;
   begin
      Get_Immediate (File, Item, Available, Wait => True);
   end Get_Immediate;

   procedure Get_Immediate (
      File : Non_Controlled_File_Type;
      Item : out Character;
      Available : out Boolean)
   is
      S : String (1 .. 1);
      Last : Natural;
   begin
      Get_Immediate (File, S, Last, Wait => False);
      Available := Last > 0;
      if Available then
         Item := S (1);
      else
         Item := Character'Val (0);
      end if;
   end Get_Immediate;

   procedure Get_Immediate (
      File : Non_Controlled_File_Type;
      Item : out Wide_Character;
      Available : out Boolean) is
   begin
      Get_Immediate (File, Item, Available, Wait => False);
   end Get_Immediate;

   procedure Get_Immediate (
      File : Non_Controlled_File_Type;
      Item : out Wide_Wide_Character;
      Available : out Boolean) is
   begin
      Get_Immediate (File, Item, Available, Wait => False);
   end Get_Immediate;

   --  implementation of handle of stream for non-controlled

   procedure Open (
      File : in out Non_Controlled_File_Type;
      Mode : IO_Modes.File_Mode;
      Stream : not null access Streams.Root_Stream_Type'Class;
      Name : String := "";
      Form : System.Native_IO.Text_IO.Packed_Form := Default_Form)
   is
      function To_Address (Value : access Streams.Root_Stream_Type'Class)
         return System.Address;
      pragma Import (Intrinsic, To_Address);
   begin
      if Is_Open (File) then
         Raise_Exception (Status_Error'Identity);
      end if;
      File := new Text_Type'(
         Name_Length => Name'Length + 1,
         Stream => To_Address (Stream),
         Mode => Mode,
         External => Select_External (Form.External),
         New_Line => Select_New_Line (Form.New_Line),
         SUB => Form.SUB,
         Name => '*' & Name,
         others => <>);
   end Open;

   function Stream (File : Non_Controlled_File_Type)
      return not null access Streams.Root_Stream_Type'Class is
   begin
      Check_File_Open (File);
      return Unchecked_Stream (File);
   end Stream;

   function Stream_IO (File : Non_Controlled_File_Type)
      return not null access
         Streams.Naked_Stream_IO.Non_Controlled_File_Type is
   begin
      Check_Stream_IO_Open (File);
      return File.File'Access;
   end Stream_IO;

   function Terminal_Handle (File : Non_Controlled_File_Type)
      return System.Native_IO.Handle_Type is
   begin
      Check_Stream_IO_Open (File);
      if File.External /= IO_Modes.Terminal then
         Raise_Exception (Device_Error'Identity);
      end if;
      return Streams.Naked_Stream_IO.Handle (File.File);
   end Terminal_Handle;

   --  initialization

   procedure Init_Standard_File (File : not null Non_Controlled_File_Type);
   procedure Init_Standard_File (File : not null Non_Controlled_File_Type) is
      function To_Address (Value : access Streams.Root_Stream_Type'Class)
         return System.Address;
      pragma Import (Intrinsic, To_Address);
   begin
      File.Stream := To_Address (Streams.Naked_Stream_IO.Stream (File.File));
      if System.Native_IO.Is_Terminal (
         Streams.Naked_Stream_IO.Handle (File.File))
      then
         File.External := IO_Modes.Terminal;
      end if;
   end Init_Standard_File;

begin
   Init_Standard_File (Standard_Input_Text'Access);
   Init_Standard_File (Standard_Output_Text'Access);
   Init_Standard_File (Standard_Error_Text'Access);
end Ada.Naked_Text_IO;
