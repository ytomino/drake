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
   use type Streams.Stream_Element_Offset;
   use type System.UTF_Conversions.UCS_4;

   procedure unreachable
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_unreachable";
   pragma No_Return (unreachable);

   function To_Pointer (Value : System.Address)
      return access Streams.Root_Stream_Type'Class
      with Import, Convention => Intrinsic;

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
                  System.Native_Text_IO.Default_External = IO_Modes.Locale;
            begin
               if Locale_Support then
                  return IO_Modes.Locale; -- Windows
               else
                  return IO_Modes.UTF_8; -- POSIX
               end if;
            end;
         when IO_Modes.By_Target =>
            return System.Native_Text_IO.Default_External;
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
            return System.Native_Text_IO.Default_New_Line;
      end case;
   end Select_New_Line;

   --  implementation of the parameter Form

   procedure Set (
      Form : in out System.Native_Text_IO.Packed_Form;
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
      else
         Streams.Naked_Stream_IO.Set (Form.Stream_Form, Keyword, Item);
      end if;
   end Set;

   function Pack (Form : String) return System.Native_Text_IO.Packed_Form is
      Keyword_First : Positive;
      Keyword_Last : Natural;
      Item_First : Positive;
      Item_Last : Natural;
      Last : Natural;
   begin
      return Result : System.Native_Text_IO.Packed_Form := Default_Form do
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
      Form : System.Native_Text_IO.Packed_Form;
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
   end Unpack;

   --  non-controlled

   procedure Free (X : in out Non_Controlled_File_Type);
   procedure Free (X : in out Non_Controlled_File_Type) is
      procedure Raw_Free is
         new Unchecked_Deallocation (Text_Type, Non_Controlled_File_Type);
   begin
      System.Native_IO.Free (X.Name);
      Raw_Free (X);
   end Free;

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
      Form : System.Native_Text_IO.Packed_Form);
   procedure Open_File (
      Open_Proc : Open_Access;
      File : in out Non_Controlled_File_Type;
      Mode : IO_Modes.File_Mode;
      Name : String;
      Form : System.Native_Text_IO.Packed_Form)
   is
      New_File : aliased Non_Controlled_File_Type := new Text_Type'(
         Stream => <>,
         Name => null,
         Mode => Mode,
         External => <>,
         New_Line => <>,
         others => <>);
      package Holder is
         new Exceptions.Finally.Scoped_Holder (Non_Controlled_File_Type, Free);
   begin
      Holder.Assign (New_File);
      --  open
      Open_Proc (
         File => New_File.File,
         Mode => Mode,
         Name => Name,
         Form => Form.Stream_Form);
      declare
         function To_Address (Value : access Streams.Root_Stream_Type'Class)
            return System.Address
            with Import, Convention => Intrinsic;
      begin
         New_File.Stream := To_Address (
            Streams.Naked_Stream_IO.Stream (New_File.File));
      end;
      --  select encoding
      if System.Native_IO.Is_Terminal (
         Streams.Naked_Stream_IO.Handle (New_File.File))
      then
         New_File.External := IO_Modes.Terminal;
         New_File.New_Line := System.Native_Text_IO.Default_New_Line;
      else
         New_File.External := Select_External (Form.External);
         New_File.New_Line := Select_New_Line (Form.New_Line);
      end if;
      --  complete
      Holder.Clear;
      File := New_File;
   end Open_File;

   --  Input
   --  * Read_Buffer sets (or keeps) Ahead_Col.
   --  * Get adds Ahead_Col to current Col.
   --  * Take_Buffer clears Ahead_Col.

   procedure Read_Buffer (
      File : Non_Controlled_File_Type;
      Wanted : Positive := 1;
      Wait : Boolean := True);
   procedure Take_Buffer (
      File : Non_Controlled_File_Type;
      Length : Positive := 1);
   procedure Take_Sequence (File : Non_Controlled_File_Type);
   procedure Take_Page (File : Non_Controlled_File_Type);
   procedure Take_Line (File : Non_Controlled_File_Type);
   procedure Take_Line_Immediate (File : Non_Controlled_File_Type);

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
                  System.Native_Text_IO.Terminal_Get (
                     Streams.Naked_Stream_IO.Handle (File.File),
                     File.Buffer (File.Last + 1)'Address,
                     1,
                     Read_Length);
                  if Read_Length = 0 then
                     File.End_Of_File := True;
                  end if;
               else
                  System.Native_Text_IO.Terminal_Get_Immediate (
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
               Streams.Read (Stream (File).all, Buffer, Last);
               File.Last := Natural'Base (Last);
               if Wait and then File.Last = Old_Last then
                  File.End_Of_File := True;
               end if;
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
                  System.Native_Text_IO.Default_External = IO_Modes.Locale;
            begin
               if not Locale_Support then
                  unreachable;
               end if;
            end;
            declare
               DBCS_Buffer : aliased System.Native_Text_IO.DBCS_Buffer_Type;
               New_Last : Natural;
            begin
               DBCS_Buffer (1) := File.Buffer (1);
               DBCS_Buffer (2) := File.Buffer (2);
               System.Native_Text_IO.To_UTF_8 (
                  DBCS_Buffer,
                  File.Last,
                  File.Buffer,
                  New_Last);
               if New_Last > 0 then
                  --  all elements in buffer are converted
                  File.Ahead_Last := New_Last;
                  File.Ahead_Col := File.Last;
                  File.Last := New_Last;
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

   procedure Take_Buffer (
      File : Non_Controlled_File_Type;
      Length : Positive := 1)
   is
      New_Last : constant Natural := File.Last - Length;
   begin
      File.Buffer (1 .. New_Last) := File.Buffer (1 + Length .. File.Last);
      File.Last := New_Last;
      File.Ahead_Last := File.Ahead_Last - Length;
      File.Ahead_Col := 0;
      File.Looked_Ahead_Last := 0;
      File.Virtual_Mark := None;
   end Take_Buffer;

   procedure Take_Sequence (File : Non_Controlled_File_Type) is
   begin
      File.Col := File.Col + File.Ahead_Col;
      Take_Buffer (File, Length => File.Looked_Ahead_Last);
      if File.Looked_Ahead_Second (1) /= Character'Val (0) then
         --  Prepend a second of surrogate pair to the buffer.
         declare
            New_Last : constant Natural := File.Last + 3;
         begin
            File.Buffer (4 .. New_Last) := File.Buffer (1 .. File.Last);
            File.Buffer (1 .. 3) := File.Looked_Ahead_Second;
            File.Last := New_Last;
            File.Ahead_Last := File.Ahead_Last + 3;
         end;
      end if;
   end Take_Sequence;

   procedure Take_Page (File : Non_Controlled_File_Type) is
   begin
      Take_Buffer (File);
      File.Virtual_Mark := EOP;
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

   procedure Take_Line_Immediate (File : Non_Controlled_File_Type) is
      C : constant Character := File.Buffer (1);
   begin
      Take_Buffer (File);
      if C = Character'Val (16#0d#) then
         Read_Buffer (File);
         if File.Buffer (1) = Character'Val (16#0a#) then
            File.Col := File.Col + 1; -- do not get LF here
         else
            File.Line := File.Line + 1; -- CR without LF
            File.Col := 1;
         end if;
      else
         File.Line := File.Line + 1; -- LF
         File.Col := 1;
      end if;
   end Take_Line_Immediate;

   type Restore_Type is record
      Handle : System.Native_IO.Handle_Type;
      Old_Settings : aliased System.Native_Text_IO.Setting;
   end record;
   pragma Suppress_Initialization (Restore_Type);

   procedure Finally (X : in out Restore_Type);
   procedure Finally (X : in out Restore_Type) is
   begin
      System.Native_Text_IO.Restore (X.Handle, X.Old_Settings);
   end Finally;

   procedure Look_Ahead_Immediate (
      File : Non_Controlled_File_Type;
      Item : out Character;
      Available : out Boolean;
      Wait : Boolean);
   procedure Skip_Ahead_Immediate (File : Non_Controlled_File_Type);

   procedure Look_Ahead_Immediate (
      File : Non_Controlled_File_Type;
      Item : out Character;
      Available : out Boolean;
      Wait : Boolean) is
   begin
      loop
         Read_Buffer (File, Wait => Wait);
         if File.Ahead_Last > 0 then
            File.Looked_Ahead_Last := 1; -- implies CR-LF sequence
            File.Looked_Ahead_Second (1) := Character'Val (0);
            Item := File.Buffer (1);
            Available := True;
            exit;
         elsif File.End_Of_File then
            File.Looked_Ahead_Last := 1;
            File.Looked_Ahead_Second (1) := Character'Val (0);
            Raise_Exception (End_Error'Identity);
         elsif not Wait then
            Item := Character'Val (0);
            Available := False;
            exit;
         end if;
      end loop;
   end Look_Ahead_Immediate;

   procedure Skip_Ahead_Immediate (File : Non_Controlled_File_Type) is
   begin
      if File.External = IO_Modes.Terminal then
         --  Do not wait CR-LF sequence, nor update Col and Line.
         Take_Buffer (File, Length => File.Looked_Ahead_Last);
      else
         --  Update Col and Line.
         declare
            C : constant Character := File.Buffer (1);
         begin
            case C is
               when Character'Val (16#0d#) | Character'Val (16#0a#) =>
                  Take_Line_Immediate (File);
               when Character'Val (16#0c#) =>
                  Take_Page (File);
               when others =>
                  Take_Sequence (File);
            end case;
         end;
      end if;
   end Skip_Ahead_Immediate;

   procedure Get_Immediate (
      File : Non_Controlled_File_Type;
      Item : out Character;
      Available : out Boolean;
      Wait : Boolean);
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

   procedure Get_Immediate (
      File : Non_Controlled_File_Type;
      Item : out Character;
      Available : out Boolean;
      Wait : Boolean)
   is
      package Holder is
         new Exceptions.Finally.Scoped_Holder (Restore_Type, Finally);
      X : aliased Restore_Type;
   begin
      if File.External = IO_Modes.Terminal then
         X.Handle := Streams.Naked_Stream_IO.Handle (File.File);
         System.Native_Text_IO.Set_Non_Canonical_Mode (
            X.Handle,
            Wait, -- only POSIX
            X.Old_Settings);
         Holder.Assign (X);
      end if;
      Look_Ahead_Immediate (File, Item, Available, Wait);
      if Available then
         Skip_Ahead_Immediate (File);
      end if;
   end Get_Immediate;

   procedure Get_Immediate (
      File : Non_Controlled_File_Type;
      Item : out Wide_Character;
      Available : out Boolean;
      Wait : Boolean)
   is
      package Holder is
         new Exceptions.Finally.Scoped_Holder (Restore_Type, Finally);
      X : aliased Restore_Type;
      C : Character;
   begin
      if File.External = IO_Modes.Terminal then
         X.Handle := Streams.Naked_Stream_IO.Handle (File.File);
         System.Native_Text_IO.Set_Non_Canonical_Mode (
            X.Handle,
            Wait, -- only POSIX
            X.Old_Settings);
         Holder.Assign (X);
      end if;
      Look_Ahead_Immediate (File, C, Available, Wait);
      if Available then
         declare
            End_Of_Line : Boolean;
         begin
            if Character'Pos (C) < 16#80# then
               --  Get_Immediate returns CR, LF and FF.
               Item := Wide_Character'Val (Character'Pos (C));
            else
               --  Waiting is OK for trailing bytes.
               Look_Ahead (File, Item, End_Of_Line); -- Wide
            end if;
         end;
         Skip_Ahead_Immediate (File);
      else
         Item := Wide_Character'Val (0);
      end if;
   end Get_Immediate;

   procedure Get_Immediate (
      File : Non_Controlled_File_Type;
      Item : out Wide_Wide_Character;
      Available : out Boolean;
      Wait : Boolean)
   is
      package Holder is
         new Exceptions.Finally.Scoped_Holder (Restore_Type, Finally);
      X : aliased Restore_Type;
      C : Character;
   begin
      if File.External = IO_Modes.Terminal then
         X.Handle := Streams.Naked_Stream_IO.Handle (File.File);
         System.Native_Text_IO.Set_Non_Canonical_Mode (
            X.Handle,
            Wait, -- only POSIX
            X.Old_Settings);
         Holder.Assign (X);
      end if;
      Look_Ahead_Immediate (File, C, Available, Wait);
      if Available then
         declare
            End_Of_Line : Boolean;
         begin
            if Character'Pos (C) < 16#80# then
               --  Get_Immediate returns CR, LF and FF.
               Item := Wide_Wide_Character'Val (Character'Pos (C));
            else
               --  Waiting is OK for trailing bytes.
               Look_Ahead (File, Item, End_Of_Line); -- Wide_Wide
            end if;
         end;
         Skip_Ahead_Immediate (File);
      else
         Item := Wide_Wide_Character'Val (0);
      end if;
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
         System.Native_Text_IO.Terminal_Clear (
            Streams.Naked_Stream_IO.Handle (File.File));
      else
         declare
            Code : constant Streams.Stream_Element_Array := (1 => 16#0c#);
         begin
            Streams.Write (Stream (File).all, Code);
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
            First, Last : Streams.Stream_Element_Offset;
         begin
            First := Boolean'Pos (File.New_Line = IO_Modes.LF);
            Last := Boolean'Pos (File.New_Line /= IO_Modes.CR);
            Streams.Write (Stream (File).all, Line_Mark (First .. Last));
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
            System.Native_Text_IO.Terminal_Put (
               Streams.Naked_Stream_IO.Handle (File.File),
               File.Buffer'Address,
               Streams.Stream_Element_Offset (Sequence_Length),
               Written_Length);
            if Written_Length < 0 then
               Raise_Exception (Device_Error'Identity);
            end if;
         end;
         File.Ahead_Col := Sequence_Length; -- for fallback
      elsif File.External = IO_Modes.Locale
         and then File.Buffer (1) >= Character'Val (16#80#)
      then
         declare
            Locale_Support : constant Boolean :=
               System.Native_Text_IO.Default_External = IO_Modes.Locale;
         begin
            if not Locale_Support then
               unreachable;
            end if;
         end;
         declare
            DBCS_Buffer : aliased System.Native_Text_IO.DBCS_Buffer_Type;
            DBCS_Last : Natural;
         begin
            System.Native_Text_IO.To_DBCS (
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
               DBCS_Buffer_As_SEA : Streams.Stream_Element_Array (
                  1 ..
                  Streams.Stream_Element_Offset (DBCS_Last));
               for DBCS_Buffer_As_SEA'Address use DBCS_Buffer'Address;
            begin
               Streams.Write (Stream (File).all, DBCS_Buffer_As_SEA);
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
            Streams.Write (Stream (File).all, Buffer);
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
      Form : System.Native_Text_IO.Packed_Form := Default_Form)
   is
      pragma Check (Pre,
         Check => not Is_Open (File) or else raise Status_Error);
   begin
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
      Form : System.Native_Text_IO.Packed_Form := Default_Form)
   is
      pragma Check (Pre,
         Check => not Is_Open (File) or else raise Status_Error);
   begin
      Open_File (
         Open_Proc => Streams.Naked_Stream_IO.Open'Access,
         File => File,
         Mode => Mode,
         Name => Name,
         Form => Form);
   end Open;

   procedure Close (
      File : aliased in out Non_Controlled_File_Type;
      Raise_On_Error : Boolean := True)
   is
      pragma Check (Pre,
         Check => Is_Open (File) or else raise Status_Error);
      Internal : aliased Streams.Naked_Stream_IO.Non_Controlled_File_Type :=
         File.File;
   begin
      if not Streams.Naked_Stream_IO.Is_Open (Internal)
         or else not Streams.Naked_Stream_IO.Is_Standard (Internal)
      then
         Free (File);
      end if;
      if Streams.Naked_Stream_IO.Is_Open (Internal) then
         Streams.Naked_Stream_IO.Close (
            Internal,
            Raise_On_Error => Raise_On_Error);
      end if;
   end Close;

   procedure Delete (File : aliased in out Non_Controlled_File_Type) is
      pragma Check (Pre,
         Check =>
            (Is_Open (File)
               and then Streams.Naked_Stream_IO.Is_Open (File.File)
               and then not Streams.Naked_Stream_IO.Is_Standard (File.File))
            or else raise Status_Error);
      Internal : aliased Streams.Naked_Stream_IO.Non_Controlled_File_Type :=
         File.File;
   begin
      Free (File);
      Streams.Naked_Stream_IO.Delete (Internal);
   end Delete;

   procedure Reset (
      File : aliased in out Non_Controlled_File_Type;
      Mode : IO_Modes.File_Mode)
   is
      pragma Check (Pre,
         Check => Is_Open (File) or else raise Status_Error);
      pragma Check (Pre,
         Check =>
            not Streams.Naked_Stream_IO.Is_Standard (File.File)
            or else Naked_Text_IO.Mode (File) = Mode
            or else raise Mode_Error);
      pragma Check (Pre,
         Check =>
            Streams.Naked_Stream_IO.Is_Open (File.File)
            or else raise Status_Error); -- external stream mode
      Current_Mode : constant IO_Modes.File_Mode := Naked_Text_IO.Mode (File);
   begin
      if Current_Mode /= IO_Modes.In_File then
         Flush (File);
      end if;
      declare
         package Holder is
            new Exceptions.Finally.Scoped_Holder (
               Non_Controlled_File_Type,
               Free);
      begin
         Holder.Assign (File);
         Streams.Naked_Stream_IO.Reset (File.File, Mode);
         Holder.Clear;
      end;
      declare
         function To_Address (Value : access Streams.Root_Stream_Type'Class)
            return System.Address
            with Import, Convention => Intrinsic;
      begin
         File.Stream := To_Address (
            Streams.Naked_Stream_IO.Stream (File.File));
      end;
      File.Page := 1;
      File.Line := 1;
      File.Col := 1;
      File.Line_Length := 0;
      File.Page_Length := 0;
      File.Last := 0;
      File.Ahead_Last := 0;
      File.Ahead_Col := 0;
      File.Looked_Ahead_Last := 0;
      File.End_Of_File := False;
      File.Virtual_Mark := None;
      File.Mode := Mode;
   end Reset;

   function Mode (File : Non_Controlled_File_Type) return IO_Modes.File_Mode is
   begin
      if Streams.Naked_Stream_IO.Is_Open (File.File) then
         return Streams.Naked_Stream_IO.Mode (File.File);
      else
         return File.Mode;
      end if;
   end Mode;

   function Name (File : Non_Controlled_File_Type) return String is
   begin
      if Streams.Naked_Stream_IO.Is_Open (File.File) then
         return Streams.Naked_Stream_IO.Name (File.File);
      else
         return System.Native_IO.Value (File.Name);
      end if;
   end Name;

   function Form (File : Non_Controlled_File_Type)
      return System.Native_Text_IO.Packed_Form
   is
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
            System.Native_Text_IO.Default_External);
      else
         External := IO_Modes.File_External_Spec (File.External);
      end if;
      return (
         Stream_Form,
         External,
         IO_Modes.File_New_Line_Spec (File.New_Line));
   end Form;

   function External (File : Non_Controlled_File_Type)
      return IO_Modes.File_External is
   begin
      return File.External;
   end External;

   function Is_Open (File : Non_Controlled_File_Type) return Boolean is
   begin
      return File /= null;
   end Is_Open;

   procedure Flush (File : Non_Controlled_File_Type) is
   begin
      if File.Last > 0 then
         Write_Buffer (File, File.Last);
         File.Col := File.Col + File.Ahead_Col;
      end if;
      if Streams.Naked_Stream_IO.Is_Open (File.File)
         and then File.External /= IO_Modes.Terminal -- console can not flush
      then
         Streams.Naked_Stream_IO.Flush (File.File);
      end if;
   end Flush;

   procedure Set_Size (
      File : Non_Controlled_File_Type;
      Line_Length, Page_Length : Natural) is
   begin
      if File.External = IO_Modes.Terminal then
         if Line_Length = 0 or else Page_Length = 0 then
            Raise_Exception (Device_Error'Identity);
         end if;
         System.Native_Text_IO.Set_Terminal_Size (
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
      if File.External = IO_Modes.Terminal then
         System.Native_Text_IO.Terminal_Size (
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
      if File.Last > 0 then
         Write_Buffer (File, File.Last);
      end if;
      Raw_New_Line (File, Spacing);
   end New_Line;

   procedure Skip_Line (
      File : Non_Controlled_File_Type;
      Spacing : Positive := 1) is
   begin
      for I in 1 .. Spacing loop
         loop
            declare
               C : Character;
               End_Of_Line : Boolean;
            begin
               Look_Ahead (File, C, End_Of_Line);
               Skip_Ahead (File);
               exit when End_Of_Line;
            end;
         end loop;
      end loop;
   end Skip_Line;

   function End_Of_Line (File : Non_Controlled_File_Type) return Boolean is
      C : Character;
      Result : Boolean;
   begin
      Look_Ahead (File, C, Result);
      return Result;
   end End_Of_Line;

   procedure New_Page (File : Non_Controlled_File_Type) is
   begin
      if File.Last > 0 then
         Write_Buffer (File, File.Last);
      end if;
      Raw_New_Page (File);
   end New_Page;

   procedure Skip_Page (File : Non_Controlled_File_Type) is
   begin
      while not End_Of_Page (File) loop
         Skip_Line (File);
      end loop;
      case File.Virtual_Mark is
         when EOF =>
            Raise_Exception (End_Error'Identity);
         when EOP =>
            File.Virtual_Mark := None;
         when EOP_EOF =>
            File.Virtual_Mark := EOF;
         when others =>
            if End_Of_File (File) then
               File.Virtual_Mark := EOF;
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
      case File.Virtual_Mark is
         when EOP | EOP_EOF =>
            return True;
         when others =>
            return End_Of_File (File) -- End_Of_File calls Read_Buffer
               or else (
                  File.Last > 0
                  and then File.Buffer (1) = Character'Val (16#0c#));
                  --  page mark is ASCII
      end case;
   end End_Of_Page;

   function End_Of_File (File : Non_Controlled_File_Type) return Boolean is
   begin
      loop
         Read_Buffer (File);
         if File.Last > 0 then
            return False;
         elsif File.End_Of_File then
            return True;
         end if;
      end loop;
   end End_Of_File;

   procedure Set_Col (File : Non_Controlled_File_Type; To : Positive) is
   begin
      if File.External = IO_Modes.Terminal
         and then System.Native_Text_IO.Use_Terminal_Position (
            Streams.Naked_Stream_IO.Handle (File.File))
      then
         System.Native_Text_IO.Set_Terminal_Col (
            Streams.Naked_Stream_IO.Handle (File.File),
            To);
      else
         if Mode (File) = IO_Modes.In_File then
            --  In_File
            loop
               declare
                  C : Character;
                  End_Of_Line : Boolean;
               begin
                  Look_Ahead (File, C, End_Of_Line);
                  exit when not End_Of_Line and then File.Col = To;
                  Skip_Ahead (File); -- raise End_Error when End_Of_File
               end;
            end loop;
         else
            --  Out_File (or Append_File)
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
      if File.External = IO_Modes.Terminal
         and then System.Native_Text_IO.Use_Terminal_Position (
            Streams.Naked_Stream_IO.Handle (File.File))
      then
         System.Native_Text_IO.Set_Terminal_Position (
            Streams.Naked_Stream_IO.Handle (File.File),
            Col => 1,
            Line => To);
      else
         if Mode (File) = IO_Modes.In_File then
            --  In_File
            while To /= File.Line or else End_Of_Page (File) loop
               Skip_Line (File);
            end loop;
         else
            --  Out_File (or Append_File)
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
      if File.External = IO_Modes.Terminal
         and then System.Native_Text_IO.Use_Terminal_Position (
            Streams.Naked_Stream_IO.Handle (File.File))
      then
         System.Native_Text_IO.Terminal_Position (
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
      return File.Page;
   end Page;

   procedure Get (
      File : Non_Controlled_File_Type;
      Item : out Character) is
   begin
      loop
         declare
            End_Of_Line : Boolean;
         begin
            Look_Ahead (File, Item, End_Of_Line);
            Skip_Ahead (File);
            exit when not End_Of_Line;
         end;
      end loop;
   end Get;

   procedure Get (
      File : Non_Controlled_File_Type;
      Item : out Wide_Character) is
   begin
      loop
         declare
            End_Of_Line : Boolean;
         begin
            Look_Ahead (File, Item, End_Of_Line);
            Skip_Ahead (File);
            exit when not End_Of_Line;
         end;
      end loop;
   end Get;

   procedure Get (
      File : Non_Controlled_File_Type;
      Item : out Wide_Wide_Character) is
   begin
      loop
         declare
            End_Of_Line : Boolean;
         begin
            Look_Ahead (File, Item, End_Of_Line);
            Skip_Ahead (File);
            exit when not End_Of_Line;
         end;
      end loop;
   end Get;

   procedure Put (
      File : Non_Controlled_File_Type;
      Item : Character)
   is
      Sequence_Length : Natural;
      Sequence_Status : System.UTF_Conversions.Sequence_Status_Type; -- ignore
   begin
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
         if Item in
            Wide_Character'Val (16#dc00#) .. Wide_Character'Val (16#dfff#)
         then
            declare
               First : System.UTF_Conversions.UCS_4;
               Code : System.UTF_Conversions.UCS_4;
               Last : Natural;
               Length : Natural;
               Wide_Buffer : Wide_String (1 .. 2);
               Wide_Last : Natural;
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
               Wide_Buffer (1) := Wide_Character'Val (First);
               Wide_Buffer (2) := Item;
               System.UTF_Conversions.From_UTF_16 (
                  Wide_Buffer,
                  Wide_Last,
                  Code,
                  From_Status);
               File.Last := 0;
               Put (File, Wide_Wide_Character'Val (Code));
               return; -- done
            end;
         else
            Write_Buffer (File, File.Last);
            File.Col := File.Col + File.Ahead_Col;
         end if;
      end if;
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
   end Put;

   procedure Put (
      File : Non_Controlled_File_Type;
      Item : Wide_Wide_Character) is
   begin
      if File.Last > 0 then
         --  previous data is rested
         Write_Buffer (File, File.Last);
         File.Col := File.Col + File.Ahead_Col;
      end if;
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
   end Put;

   procedure Look_Ahead (
      File : Non_Controlled_File_Type;
      Item : out Character;
      End_Of_Line : out Boolean) is
   begin
      loop
         Read_Buffer (File);
         if File.Ahead_Last > 0 then
            declare
               C : constant Character := File.Buffer (1);
            begin
               case C is
                  when Character'Val (16#0d#) | Character'Val (16#0a#)
                     | Character'Val (16#0c#) =>
                     File.Looked_Ahead_Last := 1; -- implies CR-LF sequence
                     File.Looked_Ahead_Second (1) := Character'Val (0);
                     End_Of_Line := True;
                     Item := Character'Val (0);
                     exit;
                  when others =>
                     File.Looked_Ahead_Last := 1;
                     File.Looked_Ahead_Second (1) := Character'Val (0);
                     End_Of_Line := False;
                     Item := C;
                     exit;
               end case;
            end;
         elsif File.End_Of_File then
            File.Looked_Ahead_Last := 1; -- Look_Ahead is called
            File.Looked_Ahead_Second (1) := Character'Val (0);
            End_Of_Line := True;
            Item := Character'Val (0);
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
            if Wide_Last > 1 then
               declare
                  Last : Natural;
               begin
                  System.UTF_Conversions.To_UTF_8 (
                     Wide_Character'Pos (Wide_Buffer (2)),
                     File.Looked_Ahead_Second,
                     Last,
                     To_Status);
               end;
            end if;
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
      --  File.Buffer is already converted even through File.External = Locale
      if End_Of_Line then
         Item := Wide_Wide_Character'Val (0);
      else
         if File.External = IO_Modes.Locale then
            declare
               Locale_Support : constant Boolean :=
                  System.Native_Text_IO.Default_External = IO_Modes.Locale;
            begin
               if not Locale_Support then
                  unreachable;
               end if;
            end;
            File.Looked_Ahead_Last := File.Ahead_Last; -- shortcut
         else
            declare
               Sequence_Length : Natural;
               Buffer_Last : Natural;
               From_Status : System.UTF_Conversions.From_Status_Type; -- ignore
            begin
               System.UTF_Conversions.UTF_8_Sequence (
                  C,
                  Sequence_Length,
                  From_Status);
               Buffer_Last := 1;
               while Buffer_Last < Sequence_Length loop
                  if File.Last < Sequence_Length then
                     Read_Buffer (File, Wanted => Sequence_Length);
                  end if;
                  if File.Last > Buffer_Last then
                     if File.Buffer (Buffer_Last + 1) in
                        Character'Val (2#10000000#) ..
                        Character'Val (2#10111111#)
                     then
                        Buffer_Last := Buffer_Last + 1;
                     else
                        exit;
                     end if;
                  elsif File.End_Of_File then
                     exit;
                  end if;
               end loop;
               File.Ahead_Last := Buffer_Last;
               if File.Ahead_Col > 0 -- skip second element of UTF-16
                  and then File.External = IO_Modes.UTF_8
               then
                  File.Ahead_Col := Buffer_Last;
               end if;
               File.Looked_Ahead_Last := Buffer_Last;
            end;
         end if;
         declare
            Conv_Last : Natural;
            Code : System.UTF_Conversions.UCS_4;
            From_Status : System.UTF_Conversions.From_Status_Type; -- ignore
         begin
            System.UTF_Conversions.From_UTF_8 (
               File.Buffer (1 .. File.Looked_Ahead_Last),
               Conv_Last,
               Code,
               From_Status);
            Item := Wide_Wide_Character'Val (Code);
         end;
      end if;
   end Look_Ahead;

   procedure Skip_Ahead (File : Non_Controlled_File_Type) is
      pragma Check (Pre,
         Check =>
            File.Looked_Ahead_Last /= 0
            or else raise Status_Error); -- Look_Ahead should be called before
   begin
      if File.Ahead_Last = 0 then -- File.End_Of_File = True
         --  Skip_Ahead can be used instead of Skip_Line
         if File.Virtual_Mark <= EOP then
            File.Virtual_Mark := EOP_EOF;
            File.Page := File.Page + 1;
            File.Line := 1;
            File.Col := 1;
         else
            Raise_Exception (End_Error'Identity);
         end if;
      else
         declare
            C : constant Character := File.Buffer (1);
         begin
            case C is
               when Character'Val (16#0d#) | Character'Val (16#0a#) =>
                  Take_Line (File);
               when Character'Val (16#0c#) =>
                  Take_Page (File);
               when others =>
                  Take_Sequence (File);
            end case;
         end;
      end if;
   end Skip_Ahead;

   procedure Get_Immediate (
      File : Non_Controlled_File_Type;
      Item : out Character)
   is
      Available : Boolean;
   begin
      Get_Immediate (File, Item, Available, Wait => True);
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
      Available : out Boolean) is
   begin
      Get_Immediate (File, Item, Available, Wait => False);
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
      Form : System.Native_Text_IO.Packed_Form := Default_Form)
   is
      pragma Check (Pre,
         Check => not Is_Open (File) or else raise Status_Error);
      function To_Address (Value : access Streams.Root_Stream_Type'Class)
         return System.Address
         with Import, Convention => Intrinsic;
      package Name_Holder is
         new Exceptions.Finally.Scoped_Holder (
            System.Native_IO.Name_Pointer,
            System.Native_IO.Free);
      Full_Name : aliased System.Native_IO.Name_Pointer;
   begin
      Name_Holder.Assign (Full_Name);
      System.Native_IO.New_External_Name (Name, Full_Name); -- '*' & Name & NUL
      File := new Text_Type'(
         Stream => To_Address (Stream),
         Name => Full_Name,
         Mode => Mode,
         External => Select_External (Form.External),
         New_Line => Select_New_Line (Form.New_Line),
         others => <>);
      --  complete
      Name_Holder.Clear;
   end Open;

   function Stream (File : not null Non_Controlled_File_Type)
      return not null access Streams.Root_Stream_Type'Class is
   begin
      return To_Pointer (File.Stream);
   end Stream;

   function Stream_IO (File : Non_Controlled_File_Type)
      return not null access
         Streams.Naked_Stream_IO.Non_Controlled_File_Type
   is
      pragma Check (Pre,
         Check =>
            Streams.Naked_Stream_IO.Is_Open (File.File)
            or else raise Status_Error); -- external stream mode
   begin
      return File.File'Access;
   end Stream_IO;

   function Terminal_Handle (File : Non_Controlled_File_Type)
      return System.Native_IO.Handle_Type
   is
      pragma Check (Pre,
         Check =>
            Streams.Naked_Stream_IO.Is_Open (File.File)
            or else raise Status_Error); -- external stream mode
   begin
      if File.External /= IO_Modes.Terminal then
         Raise_Exception (Device_Error'Identity);
      end if;
      return Streams.Naked_Stream_IO.Handle (File.File);
   end Terminal_Handle;

   --  initialization

   procedure Init_Standard_File (File : not null Non_Controlled_File_Type);
   procedure Init_Standard_File (File : not null Non_Controlled_File_Type) is
      function To_Address (Value : access Streams.Root_Stream_Type'Class)
         return System.Address
         with Import, Convention => Intrinsic;
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
