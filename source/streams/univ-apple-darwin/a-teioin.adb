with Ada.Exceptions.Finally;
with Ada.Unchecked_Deallocation;
with System.Form_Parameters;
with System.Formatting;
with System.UTF_Conversions;
with C.sys.types;
with C.termios;
with C.unistd;
package body Ada.Text_IO.Inside is
   use type IO_Text_Modes.File_External;
   use type IO_Text_Modes.File_New_Line;
   use type IO_Text_Modes.File_SUB;
   use type Streams.Stream_Element;
   use type Streams.Stream_Element_Offset;
   use type Streams.Stream_IO.Inside.Handle_Type;
   use type Streams.Stream_IO.Stream_Access;
   use type C.unsigned_int;
   use type C.unsigned_long;
   pragma Warnings (Off); -- ssize_t = Handle_Type = int in some platforms
   use type C.sys.types.ssize_t;
   pragma Warnings (On);

   procedure tcgetsetattr (
      Handle : Streams.Stream_IO.Inside.Handle_Type;
      Mask : C.termios.tcflag_t;
      Min : C.termios.cc_t;
      Saved_Settings : not null access C.termios.struct_termios);
   procedure tcgetsetattr (
      Handle : Streams.Stream_IO.Inside.Handle_Type;
      Mask : C.termios.tcflag_t;
      Min : C.termios.cc_t;
      Saved_Settings : not null access C.termios.struct_termios)
   is
      Settings : aliased C.termios.struct_termios;
      Dummy : C.signed_int;
      pragma Unreferenced (Dummy);
   begin
      --  get current terminal mode
      Dummy := C.termios.tcgetattr (Handle, Saved_Settings);
      --  set non-canonical mode
      Settings := Saved_Settings.all;
      Settings.c_lflag := Settings.c_lflag and Mask;
      Settings.c_cc (C.termios.VTIME) := 0; -- wait 0.0 sec
      Settings.c_cc (C.termios.VMIN) := Min; -- wait Min bytes
      Dummy := C.termios.tcsetattr (
         Handle,
         C.termios.TCSAFLUSH,
         Settings'Access);
   end tcgetsetattr;

   procedure write (
      Handle : Streams.Stream_IO.Inside.Handle_Type;
      Item : String);
   procedure write (
      Handle : Streams.Stream_IO.Inside.Handle_Type;
      Item : String) is
   begin
      if C.unistd.write (Handle, Item'Address, Item'Length) < 0 then
         Exceptions.Raise_Exception_From_Here (Use_Error'Identity);
      end if;
   end write;

   procedure read_Escape_Sequence (
      Handle : Streams.Stream_IO.Inside.Handle_Type;
      Item : out String;
      Last : out Natural;
      Read_Until : Character);
   procedure read_Escape_Sequence (
      Handle : Streams.Stream_IO.Inside.Handle_Type;
      Item : out String;
      Last : out Natural;
      Read_Until : Character) is
   begin
      Last := Item'First - 1;
      while C.unistd.read (
         Handle,
         C.void_ptr (Item (Last + 1)'Address),
         1) = 1
      loop
         if Last < Item'First then
            if Item (Last + 1) = Character'Val (16#1b#) then
               Last := Last + 1;
            end if;
         else
            Last := Last + 1;
            exit when Item (Last) = Read_Until or else Last >= Item'Last;
         end if;
      end loop;
   end read_Escape_Sequence;

   procedure Parse_Escape_Sequence (
      Item : String;
      Prefix : String;
      Postfix : Character;
      X1, X2 : out System.Formatting.Unsigned);
   procedure Parse_Escape_Sequence (
      Item : String;
      Prefix : String;
      Postfix : Character;
      X1, X2 : out System.Formatting.Unsigned)
   is
      P : Natural;
      Error : Boolean;
   begin
      if Item'Length >= Prefix'Length
         and then Item (Item'First .. Item'First + Prefix'Length - 1) = Prefix
      then
         System.Formatting.Value (
            Item (Item'First + Prefix'Length .. Item'Last),
            P,
            X1,
            Error => Error);
         if not Error
            and then P < Item'Last
            and then Item (P + 1) = ';'
         then
            System.Formatting.Value (
               Item (P + 2 .. Item'Last),
               P,
               X2,
               Error => Error);
            if not Error
               and then P + 1 = Item'Last
               and then Item (P + 1) = Postfix
            then
               return;
            end if;
         end if;
      end if;
      Exceptions.Raise_Exception_From_Here (Data_Error'Identity);
   end Parse_Escape_Sequence;

   --  implementation of the parameter Form

   procedure Set (Form : in out Packed_Form; Keyword, Item : String) is
   begin
      if Keyword = "nl" then -- abbr or new_line
         if Item'Length > 0 and then Item (Item'First) = 'l' then -- lf
            Form.New_Line := IO_Text_Modes.LF;
         elsif Item'Length > 0 and then Item (Item'First) = 'c' then -- cr
            Form.New_Line := IO_Text_Modes.CR;
         elsif Item'Length > 0 and then Item (Item'First) = 'm' then
            Form.New_Line := IO_Text_Modes.CR_LF;
         end if;
      elsif Keyword = "sub" then
         if Item'Length > 0 and then Item (Item'First) = 'e' then -- eof
            Form.SUB := IO_Text_Modes.End_Of_File;
         elsif Item'Length > 0 and then Item (Item'First) = 'o' then
            Form.SUB := IO_Text_Modes.Ordinary;
         end if;
      else
         Streams.Stream_IO.Inside.Set (Form.Stream_Form, Keyword, Item);
      end if;
   end Set;

   function Pack (Form : String) return Packed_Form is
      Keyword_First : Positive;
      Keyword_Last : Natural;
      Item_First : Positive;
      Item_Last : Natural;
      Last : Natural;
   begin
      return Result : Packed_Form := Default_Form do
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
      Form : Packed_Form;
      Result : out Streams.Stream_IO.Inside.Form_String;
      Last : out Natural)
   is
      subtype Not_LF is IO_Text_Modes.File_New_Line range
         IO_Text_Modes.CR ..
         IO_Text_Modes.CR_LF;
      New_Last : Natural;
   begin
      Streams.Stream_IO.Inside.Unpack (Form.Stream_Form, Result, Last);
      if Form.New_Line /= IO_Text_Modes.LF then
         if Last /= Streams.Stream_IO.Inside.Form_String'First - 1 then
            New_Last := Last + 1;
            Result (New_Last) := ',';
            Last := New_Last;
         end if;
         case Not_LF (Form.New_Line) is
            when IO_Text_Modes.CR =>
               New_Last := Last + 5;
               Result (Last + 1 .. New_Last) := "lm=cr";
               Last := New_Last;
            when IO_Text_Modes.CR_LF =>
               New_Last := Last + 4;
               Result (Last + 1 .. New_Last) := "lm=m";
               Last := New_Last;
         end case;
      end if;
      if Form.SUB /= IO_Text_Modes.Ordinary then
         if Last /= Streams.Stream_IO.Inside.Form_String'First - 1 then
            New_Last := Last + 1;
            Result (New_Last) := ',';
            Last := New_Last;
         end if;
         New_Last := Last + 7;
         Result (Last + 1 .. New_Last) := "sub=eof";
         Last := New_Last;
      end if;
   end Unpack;

   --  implementation of handle of stream

   procedure Open (
      File : in out File_Type;
      Stream : Streams.Stream_IO.Stream_Access;
      Mode : File_Mode;
      Name : String := "";
      Form : String) is
   begin
      Open (
         File => Reference (File).all,
         Stream => Stream,
         Mode => Mode,
         Name => Name,
         Form => Pack (Form));
   end Open;

   procedure Open (
      File : in out File_Type;
      Stream : Streams.Stream_IO.Stream_Access;
      Mode : File_Mode;
      Name : String := "";
      Form : Packed_Form := Default_Form) is
   begin
      Open (
         File => Reference (File).all,
         Stream => Stream,
         Mode => Mode,
         Name => Name,
         Form => Form);
   end Open;

   function Stream (File : File_Type) return Streams.Stream_IO.Stream_Access is
   begin
      return Stream (Reference (File).all);
   end Stream;

   function Stream_IO (File : File_Type)
      return not null access
         Streams.Stream_IO.Inside.Non_Controlled_File_Type is
   begin
      return Stream_IO (Reference (File).all);
   end Stream_IO;

   --  non-controlled

   procedure Free is new Unchecked_Deallocation (
      Text_Type,
      Non_Controlled_File_Type);

   procedure Finally (X : not null access Non_Controlled_File_Type);
   procedure Finally (X : not null access Non_Controlled_File_Type) is
   begin
      Free (X.all);
   end Finally;

   procedure Check_File_Mode (
      File : Non_Controlled_File_Type;
      Expected : File_Mode);
   procedure Check_File_Open (File : Non_Controlled_File_Type);
   procedure Check_Stream_IO_Open (File : Non_Controlled_File_Type);

   procedure Check_File_Mode (
      File : Non_Controlled_File_Type;
      Expected : File_Mode) is
   begin
      if (Mode (File) = In_File) /= (Expected = In_File) then
         Exceptions.Raise_Exception_From_Here (Mode_Error'Identity);
      end if;
   end Check_File_Mode;

   procedure Check_File_Open (File : Non_Controlled_File_Type) is
   begin
      if not Is_Open (File) then
         Exceptions.Raise_Exception_From_Here (Status_Error'Identity);
      end if;
   end Check_File_Open;

   procedure Check_Stream_IO_Open (File : Non_Controlled_File_Type) is
   begin
      if not Is_Open (File)
         or else not Streams.Stream_IO.Inside.Is_Open (File.File)
      then
         Exceptions.Raise_Exception_From_Here (Status_Error'Identity);
      end if;
   end Check_Stream_IO_Open;

   type Open_Access is not null access procedure (
      File : in out Streams.Stream_IO.Inside.Non_Controlled_File_Type;
      Mode : Streams.Stream_IO.File_Mode;
      Name : String;
      Form : Streams.Stream_IO.Inside.Packed_Form);

   procedure Open_File (
      Open_Proc : Open_Access;
      File : in out Non_Controlled_File_Type;
      Mode : File_Mode;
      Name : String;
      Form : Packed_Form);
   procedure Open_File (
      Open_Proc : Open_Access;
      File : in out Non_Controlled_File_Type;
      Mode : File_Mode;
      Name : String;
      Form : Packed_Form)
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
      package Holder is new Exceptions.Finally.Scoped_Holder (
         Non_Controlled_File_Type,
         Finally);
   begin
      Holder.Assign (New_File'Access);
      --  open
      Open_Proc.all (
         File => New_File.File,
         Mode => Streams.Stream_IO.File_Mode (Mode),
         Name => Name,
         Form => Form.Stream_Form);
      New_File.Stream := Streams.Stream_IO.Inside.Stream (New_File.File);
      --  select encoding
      if Streams.Stream_IO.Inside.Is_Terminal (
         Streams.Stream_IO.Inside.Handle (New_File.File))
      then
         New_File.External := IO_Text_Modes.Terminal;
         New_File.New_Line := IO_Text_Modes.LF;
         New_File.SUB := IO_Text_Modes.Ordinary;
      else
         New_File.External := IO_Text_Modes.UTF_8;
         New_File.New_Line := Form.New_Line;
         New_File.SUB := Form.SUB;
      end if;
      --  complete
      Holder.Clear;
      File := New_File;
   end Open_File;

   procedure Raw_New_Page (File : Non_Controlled_File_Type);
   procedure Raw_New_Page (File : Non_Controlled_File_Type) is
   begin
      if File.External = IO_Text_Modes.Terminal then
         declare -- clear screen
            Code : constant Streams.Stream_Element_Array := (
               16#1b#,
               Character'Pos ('['),
               Character'Pos ('2'),
               Character'Pos ('J'),
               16#1b#,
               Character'Pos ('['),
               Character'Pos ('0'),
               Character'Pos (';'),
               Character'Pos ('0'),
               Character'Pos ('H'));
         begin
            Streams.Write (File.Stream.all, Code);
         end;
      else
         declare
            Code : constant Streams.Stream_Element_Array := (1 => 16#0c#);
         begin
            Streams.Write (File.Stream.all, Code);
         end;
      end if;
      File.Page := File.Page + 1;
      File.Line := 1;
      File.Col := 1;
   end Raw_New_Page;

   procedure Raw_New_Line (File : Non_Controlled_File_Type);
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
            F := Boolean'Pos (File.New_Line = IO_Text_Modes.LF);
            L := Boolean'Pos (File.New_Line /= IO_Text_Modes.CR);
            Streams.Write (File.Stream.all, Line_Mark (F .. L));
         end;
         File.Line := File.Line + 1;
         File.Col := 1;
      end if;
   end Raw_New_Line;

   procedure Raw_New_Line (
      File : Non_Controlled_File_Type;
      Spacing : Positive_Count);
   procedure Raw_New_Line (
      File : Non_Controlled_File_Type;
      Spacing : Positive_Count) is
   begin
      for I in 1 .. Spacing loop
         Raw_New_Line (File);
      end loop;
   end Raw_New_Line;

   procedure Read_Buffer (
      File : Non_Controlled_File_Type;
      Wanted : Natural := 1);
   procedure Take_Buffer (File : Non_Controlled_File_Type);
   procedure Take_Page (File : Non_Controlled_File_Type);
   procedure Take_Line (File : Non_Controlled_File_Type);

   procedure Write_Buffer (
      File : Non_Controlled_File_Type;
      Sequence_Length : Natural);

   --  Input
   --  * Read_Buffer sets (or keep) Buffer_Col.
   --  * Get adds Buffer_Col to current Col.
   --  * Take_Buffer clear Buffer_Col.
   --  Output
   --  * Write_Buffer sets Buffer_Col to written width.
   --  * Put adds Buffer_Col to current Col.

   procedure Read_Buffer (
      File : Non_Controlled_File_Type;
      Wanted : Natural := 1)
   is
      Wanted_or_1 : constant Positive := Integer'Max (1, Wanted);
   begin
      if not File.End_Of_File and then File.Last < Wanted_or_1 then
         declare
            Old_Last : constant Natural := File.Last;
            --  read next single character
            Buffer : Streams.Stream_Element_Array (
               Streams.Stream_Element_Offset (Old_Last + 1) ..
               Streams.Stream_Element_Offset (Wanted_or_1));
            for Buffer'Address use
               File.Buffer (Old_Last + 1 .. Wanted_or_1)'Address;
            Last : Streams.Stream_Element_Offset := 0;
         begin
            begin
               Streams.Read (File.Stream.all, Buffer, Last);
            exception
               when End_Error =>
                  File.End_Of_File := True;
            end;
            if Integer (Last) < Wanted then
               File.End_Of_File := True;
            end if;
            File.Last := Integer (Last);
            File.Buffer_Col := Count (Integer (Last) - Old_Last);
         end;
      end if;
   end Read_Buffer;

   procedure Take_Buffer (File : Non_Controlled_File_Type) is
      New_Last : constant Natural := File.Last - 1;
   begin
      File.Buffer (1 .. New_Last) := File.Buffer (2 .. File.Last);
      File.Last := New_Last;
      File.Buffer_Col := 0;
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

   procedure Write_Buffer (
      File : Non_Controlled_File_Type;
      Sequence_Length : Natural)
   is
      Length : constant Natural := File.Last; -- >= Sequence_Length
   begin
      if File.Line_Length /= 0
         and then File.Col + Count (Sequence_Length - 1) > File.Line_Length
      then
         Raw_New_Line (File);
      end if;
      declare
         Buffer : Streams.Stream_Element_Array (
            1 ..
            Streams.Stream_Element_Offset (Sequence_Length));
         for Buffer'Address use File.Buffer'Address;
      begin
         Streams.Write (File.Stream.all, Buffer);
      end;
      File.Buffer_Col := Count (Sequence_Length);
      File.Last := Length - Sequence_Length;
      File.Buffer (1 .. File.Last) :=
         File.Buffer (Sequence_Length + 1 .. Length);
   end Write_Buffer;

   --  implementation of non-controlled

   procedure Create (
      File : in out Non_Controlled_File_Type;
      Mode : File_Mode := Out_File;
      Name : String := "";
      Form : Packed_Form := Default_Form) is
   begin
      if Is_Open (File) then
         Exceptions.Raise_Exception_From_Here (Status_Error'Identity);
      end if;
      Open_File (
         Open_Proc => Streams.Stream_IO.Inside.Create'Access,
         File => File,
         Mode => Mode,
         Name => Name,
         Form => Form);
   end Create;

   procedure Open (
      File : in out Non_Controlled_File_Type;
      Mode : File_Mode;
      Name : String;
      Form : Packed_Form := Default_Form) is
   begin
      if Is_Open (File) then
         Exceptions.Raise_Exception_From_Here (Status_Error'Identity);
      end if;
      Open_File (
         Open_Proc => Streams.Stream_IO.Inside.Open'Access,
         File => File,
         Mode => Mode,
         Name => Name,
         Form => Form);
   end Open;

   procedure Close (
      File : in out Non_Controlled_File_Type;
      Raise_On_Error : Boolean := True) is
   begin
      if Is_Open (File) then
         declare
            Internal : Streams.Stream_IO.Inside.Non_Controlled_File_Type :=
               File.File;
         begin
            if not Streams.Stream_IO.Inside.Is_Standard (Internal) then
               Free (File);
            end if;
            if Streams.Stream_IO.Inside.Is_Open (Internal) then
               Streams.Stream_IO.Inside.Close (
                  Internal,
                  Raise_On_Error => Raise_On_Error);
            end if;
         end;
      elsif Raise_On_Error then
         Exceptions.Raise_Exception_From_Here (Status_Error'Identity);
      end if;
   end Close;

   procedure Delete (File : in out Non_Controlled_File_Type) is
   begin
      Check_File_Open (File);
      Streams.Stream_IO.Inside.Delete (File.File);
      Free (File);
   end Delete;

   procedure Reset (
      File : not null access Non_Controlled_File_Type;
      Mode : File_Mode)
   is
      Current_Mode : constant File_Mode := Inside.Mode (File.all);
   begin
      if Current_Mode /= Mode
         and then Streams.Stream_IO.Inside.Is_Standard (File.all.File)
      then
         Exceptions.Raise_Exception_From_Here (Mode_Error'Identity);
      elsif not Streams.Stream_IO.Inside.Is_Open (File.all.File) then
         --  External stream mode
         Exceptions.Raise_Exception_From_Here (Status_Error'Identity);
      else
         if Current_Mode /= In_File then
            Flush (File.all);
         end if;
         declare
            package Holder is new Exceptions.Finally.Scoped_Holder (
               Non_Controlled_File_Type,
               Finally);
         begin
            Holder.Assign (File);
            Streams.Stream_IO.Inside.Reset (
               File.all.File'Access,
               Streams.Stream_IO.File_Mode (Mode));
            Holder.Clear;
         end;
         File.all.Stream := Streams.Stream_IO.Inside.Stream (File.all.File);
         File.all.Page := 1;
         File.all.Line := 1;
         File.all.Col := 1;
         File.all.Line_Length := 0;
         File.all.Page_Length := 0;
         File.all.Buffer_Col := 0;
         File.all.Last := 0;
         File.all.End_Of_File := False;
         File.all.Dummy_Mark := None;
         File.all.Mode := Mode;
      end if;
   end Reset;

   function Mode (File : Non_Controlled_File_Type) return File_Mode is
   begin
      Check_File_Open (File);
      if Streams.Stream_IO.Inside.Is_Open (File.File) then
         return File_Mode (Streams.Stream_IO.Inside.Mode (File.File));
      else
         return File.Mode;
      end if;
   end Mode;

   function Name (File : Non_Controlled_File_Type) return String is
   begin
      Check_File_Open (File);
      if Streams.Stream_IO.Inside.Is_Open (File.File) then
         return Streams.Stream_IO.Inside.Name (File.File);
      else
         return File.Name;
      end if;
   end Name;

   function Form (File : Non_Controlled_File_Type) return Packed_Form is
   begin
      Check_File_Open (File);
      declare
         Stream_Form : Streams.Stream_IO.Inside.Packed_Form;
      begin
         if Streams.Stream_IO.Inside.Is_Open (File.File) then
            Stream_Form := Streams.Stream_IO.Inside.Form (File.File);
         else
            Stream_Form := Streams.Stream_IO.Inside.Default_Form;
         end if;
         return (Stream_Form, IO_Text_Modes.UTF_8, File.New_Line, File.SUB);
      end;
   end Form;

   function External (File : Non_Controlled_File_Type)
      return IO_Text_Modes.File_External is
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
      Check_File_Mode (File, Out_File);
      if File.Last > 0 then
         Write_Buffer (File, File.Last);
         File.Col := File.Col + File.Buffer_Col;
      end if;
      if Streams.Stream_IO.Inside.Is_Open (File.File) then
         Streams.Stream_IO.Inside.Flush (File.File);
      end if;
   end Flush;

   procedure Set_Size (
      File : Non_Controlled_File_Type;
      Line_Length, Page_Length : Count) is
   begin
      Check_File_Mode (File, Out_File);
      if File.External = IO_Text_Modes.Terminal then
         declare
            Seq : String (1 .. 256);
            Last : Natural := 0;
            Error : Boolean;
         begin
            Seq (1) := Character'Val (16#1b#);
            Seq (2) := '[';
            Seq (3) := '8';
            Seq (4) := ';';
            Last := 4;
            System.Formatting.Image (
               System.Formatting.Unsigned (Page_Length),
               Seq (Last + 1 .. Seq'Last),
               Last,
               Error => Error);
            Last := Last + 1;
            Seq (Last) := ';';
            System.Formatting.Image (
               System.Formatting.Unsigned (Line_Length),
               Seq (Last + 1 .. Seq'Last),
               Last,
               Error => Error);
            Last := Last + 1;
            Seq (Last) := 't';
            declare
               Item : Streams.Stream_Element_Array (
                  1 ..
                  Streams.Stream_Element_Offset (Last));
               for Item'Address use Seq'Address;
            begin
               Streams.Write (File.Stream.all, Item);
            end;
         end;
      else
         File.Line_Length := Line_Length;
         File.Page_Length := Page_Length;
      end if;
   end Set_Size;

   procedure Set_Line_Length (File : Non_Controlled_File_Type; To : Count) is
   begin
      Set_Size (File, To, Page_Length (File));
   end Set_Line_Length;

   procedure Set_Page_Length (File : Non_Controlled_File_Type; To : Count) is
   begin
      Set_Size (File, Line_Length (File), To);
   end Set_Page_Length;

   procedure Size (
      File : Non_Controlled_File_Type;
      Line_Length, Page_Length : out Count)
   is
      Seq : constant String (1 .. 5) := (
         Character'Val (16#1b#), '[', '1', '8', 't');
      Handle : Streams.Stream_IO.Inside.Handle_Type;
      Old_Settings : aliased C.termios.struct_termios;
      Buffer : String (1 .. 256);
      Last : Natural;
      Dummy : C.signed_int;
      pragma Unreferenced (Dummy);
   begin
      Check_File_Mode (File, Out_File);
      if File.External = IO_Text_Modes.Terminal then
         --  non-canonical mode and disable echo
         Handle := Streams.Stream_IO.Inside.Handle (File.File);
         tcgetsetattr (
            Handle,
            not (C.termios.ECHO or C.termios.ICANON),
            1,
            Old_Settings'Access);
         --  output
         write (Handle, Seq);
         --  input
         read_Escape_Sequence (Handle, Buffer, Last, 't');
         --  restore terminal mode
         Dummy := C.termios.tcsetattr (
            Streams.Stream_IO.Inside.Handle (File.File),
            C.termios.TCSANOW,
            Old_Settings'Access);
         --  parse
         Parse_Escape_Sequence (
            Buffer (1 .. Last),
            Character'Val (16#1b#) & "[8;",
            't',
            System.Formatting.Unsigned (Page_Length),
            System.Formatting.Unsigned (Line_Length));
      else
         Line_Length := File.Line_Length;
         Page_Length := File.Page_Length;
      end if;
   end Size;

   function Line_Length (File : Non_Controlled_File_Type) return Count is
      Line_Length, Page_Length : Count;
   begin
      Size (File, Line_Length, Page_Length);
      return Line_Length;
   end Line_Length;

   function Page_Length (File : Non_Controlled_File_Type) return Count is
      Line_Length, Page_Length : Count;
   begin
      Size (File, Line_Length, Page_Length);
      return Page_Length;
   end Page_Length;

   procedure New_Line (
      File : Non_Controlled_File_Type;
      Spacing : Positive_Count := 1) is
   begin
      Check_File_Mode (File, Out_File);
      if File.Last > 0 then
         Write_Buffer (File, File.Last);
      end if;
      Raw_New_Line (File, Spacing);
   end New_Line;

   procedure Skip_Line (
      File : Non_Controlled_File_Type;
      Spacing : Positive_Count := 1) is
   begin
      Check_File_Mode (File, In_File);
      for I in 1 .. Spacing loop
         loop
            Read_Buffer (File);
            if End_Of_File (File) then
               if File.Dummy_Mark <= EOP then
                  File.Dummy_Mark := EOP_EOF;
                  File.Page := File.Page + 1;
                  File.Line := 1;
                  File.Col := 1;
                  exit;
               else
                  Exceptions.Raise_Exception_From_Here (End_Error'Identity);
               end if;
            elsif File.Last > 0 then
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
                        if File.SUB = IO_Text_Modes.End_Of_File then
                           File.End_Of_File := True; -- for next loop
                           File.Last := 0;
                        else
                           File.Col := File.Col + File.Buffer_Col;
                           Take_Buffer (File);
                        end if;
                     when others =>
                        File.Col := File.Col + File.Buffer_Col;
                        Take_Buffer (File);
                  end case;
               end;
            end if;
         end loop;
      end loop;
   end Skip_Line;

   function End_Of_Line (File : Non_Controlled_File_Type) return Boolean is
   begin
      if End_Of_File (File) then
         return True;
      else
         Read_Buffer (File);
         return File.Last > 0 and then (
            File.Buffer (1) = Character'Val (16#0d#)
            or else File.Buffer (1) = Character'Val (16#0a#)
            or else File.Buffer (1) = Character'Val (16#0c#)
            or else (
               File.SUB = IO_Text_Modes.End_Of_File
                  and then File.Buffer (1) = Character'Val (16#1a#)));
      end if;
   end End_Of_Line;

   procedure New_Page (File : Non_Controlled_File_Type) is
   begin
      Check_File_Mode (File, Out_File);
      if File.Last > 0 then
         Write_Buffer (File, File.Last);
      end if;
      Raw_New_Page (File);
   end New_Page;

   procedure Skip_Page (File : Non_Controlled_File_Type) is
   begin
      Check_File_Mode (File, In_File);
      while not End_Of_Page (File) loop
         Skip_Line (File);
      end loop;
      case File.Dummy_Mark is
         when EOF =>
            Exceptions.Raise_Exception_From_Here (End_Error'Identity);
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
      if End_Of_File (File)
         or else File.Dummy_Mark = EOP
         or else File.Dummy_Mark = EOP_EOF
      then
         return True;
      else
         Read_Buffer (File);
         return File.Last > 0
            and then File.Buffer (1) = Character'Val (16#0c#);
      end if;
   end End_Of_Page;

   function End_Of_File (File : Non_Controlled_File_Type) return Boolean is
   begin
      Check_File_Mode (File, In_File);
      if File.Last > 0 then
         return False;
      elsif not Streams.Stream_IO.Inside.Is_Open (File.File) then
         Read_Buffer (File);
         return File.End_Of_File;
      else
         Read_Buffer (File);
         return File.Last = 0
            and then Streams.Stream_IO.Inside.End_Of_File (File.File);
      end if;
   end End_Of_File;

   procedure Set_Position_Within_Terminal (
      File : Non_Controlled_File_Type;
      Col, Line : Positive_Count) is
   begin
      Check_File_Mode (File, Out_File);
      if File.External = IO_Text_Modes.Terminal then
         declare
            Seq : String (1 .. 256);
            Last : Natural := 0;
            Error : Boolean;
         begin
            Seq (1) := Character'Val (16#1b#);
            Seq (2) := '[';
            Last := 2;
            System.Formatting.Image (
               System.Formatting.Unsigned (Line),
               Seq (Last + 1 .. Seq'Last),
               Last,
               Error => Error);
            Last := Last + 1;
            Seq (Last) := ';';
            System.Formatting.Image (
               System.Formatting.Unsigned (Col),
               Seq (Last + 1 .. Seq'Last),
               Last,
               Error => Error);
            Last := Last + 1;
            Seq (Last) := 'H';
            declare
               Item : Streams.Stream_Element_Array (
                  1 ..
                  Streams.Stream_Element_Offset (Last));
               for Item'Address use Seq'Address;
            begin
               Streams.Write (File.Stream.all, Item);
            end;
         end;
      else
         Exceptions.Raise_Exception_From_Here (Device_Error'Identity);
      end if;
   end Set_Position_Within_Terminal;

   procedure Set_Col_Within_Terminal (
      File : Non_Controlled_File_Type;
      To : Positive_Count) is
   begin
      Check_File_Mode (File, Out_File);
      if File.External = IO_Text_Modes.Terminal then
         declare
            Seq : String (1 .. 256);
            Last : Natural := 0;
            Error : Boolean;
         begin
            Seq (1) := Character'Val (16#1b#);
            Seq (2) := '[';
            Last := 2;
            System.Formatting.Image (
               System.Formatting.Unsigned (To),
               Seq (Last + 1 .. Seq'Last),
               Last,
               Error => Error);
            Last := Last + 1;
            Seq (Last) := 'G';
            declare
               Item : Streams.Stream_Element_Array (
                  1 ..
                  Streams.Stream_Element_Offset (Last));
               for Item'Address use Seq'Address;
            begin
               Streams.Write (File.Stream.all, Item);
            end;
         end;
      else
         Exceptions.Raise_Exception_From_Here (Device_Error'Identity);
      end if;
   end Set_Col_Within_Terminal;

   procedure Set_Col (File : Non_Controlled_File_Type; To : Positive_Count) is
   begin
      if Mode (File) = In_File then
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
         if File.External = IO_Text_Modes.Terminal then
            Set_Col_Within_Terminal (File, To);
         else
            if File.Line_Length /= 0 and then To > File.Line_Length then
               Exceptions.Raise_Exception_From_Here (Layout_Error'Identity);
            end if;
            if File.Col > To then
               Raw_New_Line (File);
            end if;
            while File.Col < To loop
               Put (File, ' ');
            end loop;
         end if;
      end if;
   end Set_Col;

   procedure Set_Line (File : Non_Controlled_File_Type; To : Positive_Count) is
   begin
      if Mode (File) = In_File then
         --  In_File
         while To /= File.Line or else End_Of_Page (File) loop
            Skip_Line (File);
         end loop;
      else
         --  Out_File (or Append_File)
         if File.External = IO_Text_Modes.Terminal then
            Set_Position_Within_Terminal (File, 1, To);
         else
            if File.Page_Length /= 0 and then To > File.Page_Length then
               Exceptions.Raise_Exception_From_Here (Layout_Error'Identity);
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
      Col, Line : out Positive_Count)
   is
      Seq : constant String (1 .. 4) := (
         Character'Val (16#1b#), '[', '6', 'n');
      Handle : Streams.Stream_IO.Inside.Handle_Type;
      Old_Settings : aliased C.termios.struct_termios;
      Buffer : String (1 .. 256);
      Last : Natural;
      Dummy : C.signed_int;
      pragma Unreferenced (Dummy);
   begin
      Check_File_Open (File);
      if File.External = IO_Text_Modes.Terminal then
         --  non-canonical mode and disable echo
         Handle := Streams.Stream_IO.Inside.Handle (File.File);
         tcgetsetattr (
            Handle,
            not (C.termios.ECHO or C.termios.ICANON),
            1,
            Old_Settings'Access);
         --  output
         write (Handle, Seq);
         --  input
         read_Escape_Sequence (Handle, Buffer, Last, 'R');
         --  restore terminal mode
         Dummy := C.termios.tcsetattr (
            Streams.Stream_IO.Inside.Handle (File.File),
            C.termios.TCSANOW,
            Old_Settings'Access);
         --  parse
         Parse_Escape_Sequence (
            Buffer (1 .. Last),
            Character'Val (16#1b#) & "[",
            'R',
            System.Formatting.Unsigned (Line),
            System.Formatting.Unsigned (Col));
      else
         Col := File.Col;
         Line := File.Line;
      end if;
   end Position;

   function Col (File : Non_Controlled_File_Type) return Positive_Count is
      Col, Line : Positive_Count;
   begin
      Position (File, Col, Line);
      return Col;
   end Col;

   function Line (File : Non_Controlled_File_Type) return Positive_Count is
      Col, Line : Positive_Count;
   begin
      Position (File, Col, Line);
      return Line;
   end Line;

   function Page (File : Non_Controlled_File_Type) return Positive_Count is
   begin
      Check_File_Open (File);
      return File.Page;
   end Page;

   procedure Get (File : Non_Controlled_File_Type; Item : out Character) is
   begin
      Check_File_Mode (File, In_File);
      loop
         Read_Buffer (File);
         if End_Of_File (File) then
            Exceptions.Raise_Exception_From_Here (End_Error'Identity);
         elsif File.Last > 0 then
            declare
               C : constant Character := File.Buffer (1);
            begin
               case C is
                  when Character'Val (16#0d#) | Character'Val (16#0a#) =>
                     Take_Line (File);
                  when Character'Val (16#0c#) =>
                     Take_Page (File);
                  when Character'Val (16#1a#) =>
                     if File.SUB = IO_Text_Modes.End_Of_File then
                        File.End_Of_File := True; -- for next loop
                        File.Last := 0;
                     else
                        Item := C;
                        File.Col := File.Col + File.Buffer_Col;
                        Take_Buffer (File);
                        exit;
                     end if;
                  when others =>
                     Item := C;
                     File.Col := File.Col + File.Buffer_Col;
                     Take_Buffer (File);
                     exit;
               end case;
            end;
         end if;
      end loop;
   end Get;

   procedure Put (File : Non_Controlled_File_Type; Item : Character) is
      Sequence_Length : Natural;
      Sequence_Status : System.UTF_Conversions.Sequence_Status_Type; -- ignore
   begin
      Check_File_Open (File);
      --  if Item is not trailing byte, flush the buffer
      if File.Line_Length /= 0
         and then File.Last > 0
         and then Character'Pos (Item) not in 2#10000000# .. 2#10111111#
      then
         Write_Buffer (File, File.Last);
         File.Col := File.Col + File.Buffer_Col;
      end if;
      --  write to the buffer
      File.Last := File.Last + 1;
      File.Buffer (File.Last) := Item;
      if File.Line_Length /= 0 then
         System.UTF_Conversions.UTF_8_Sequence (
            File.Buffer (1),
            Sequence_Length,
            Sequence_Status);
         if File.Last >= Sequence_Length then
            Write_Buffer (File, Sequence_Length);
            File.Col := File.Col + File.Buffer_Col;
         end if;
      else
         Write_Buffer (File, File.Last);
         File.Col := File.Col + File.Buffer_Col;
      end if;
   end Put;

   procedure Look_Ahead (
      File : Non_Controlled_File_Type;
      Item : out Character;
      End_Of_Line : out Boolean) is
   begin
      Check_File_Open (File);
      loop
         Read_Buffer (File);
         if Inside.End_Of_Line (File) then
            End_Of_Line := True;
            Item := Character'Val (0);
            exit;
         elsif File.Last > 0 then
            End_Of_Line := False;
            Item := File.Buffer (1);
            exit;
         end if;
      end loop;
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

   procedure View (
      File : Non_Controlled_File_Type;
      Left, Top : out Positive_Count;
      Right, Bottom : out Count) is
   begin
      Size (File, Right, Bottom);
      Left := 1;
      Top := 1;
   end View;

   --  implementation of handle of stream for non-controlled

   procedure Open (
      File : in out Non_Controlled_File_Type;
      Stream : Streams.Stream_IO.Stream_Access;
      Mode : File_Mode;
      Name : String := "";
      Form : Packed_Form := Default_Form) is
   begin
      if Is_Open (File) then
         Exceptions.Raise_Exception_From_Here (Status_Error'Identity);
      end if;
      File := new Text_Type'(
         Name_Length => Name'Length + 1,
         Stream => Stream,
         Mode => Mode,
         External => IO_Text_Modes.File_External (Form.External),
         New_Line => Form.New_Line,
         SUB => Form.SUB,
         Name => '*' & Name,
         others => <>);
   end Open;

   function Stream (File : Non_Controlled_File_Type)
      return Streams.Stream_IO.Stream_Access is
   begin
      Check_Stream_IO_Open (File);
      return File.Stream;
   end Stream;

   function Stream_IO (File : Non_Controlled_File_Type)
      return not null access
         Streams.Stream_IO.Inside.Non_Controlled_File_Type is
   begin
      Check_Stream_IO_Open (File);
      return File.File'Access;
   end Stream_IO;

   --  implementation for Wide_Text_IO/Wide_Wide_Text_IO

   procedure Look_Ahead (
      File : Non_Controlled_File_Type;
      Item : out String;
      Last : out Natural;
      End_Of_Line : out Boolean)
   is
      Wanted_Length : constant Natural := Item'Length;
      Buffer_Last : Natural;
   begin
      Check_File_Open (File);
      loop
         Read_Buffer (File, Wanted_Length);
         exit when File.Last = Wanted_Length or else File.End_Of_File;
      end loop;
      if File.Last = 0 then
         Last := Item'First - 1;
         End_Of_Line := True;
      else
         Buffer_Last := File.Last;
         End_Of_Line := False;
         for I in 1 .. File.Last loop
            if File.Buffer (I) = Character'Val (16#0d#)
               or else File.Buffer (I) = Character'Val (16#0a#)
               or else File.Buffer (I) = Character'Val (16#0c#)
               or else (
                  File.SUB = IO_Text_Modes.End_Of_File
                     and then File.Buffer (I) = Character'Val (16#1a#))
            then
               Buffer_Last := I - 1;
               End_Of_Line := True;
               exit;
            end if;
         end loop;
         Last := Item'First + Last - 1;
         Item (Item'First .. Last) := File.Buffer (1 .. Buffer_Last);
      end if;
   end Look_Ahead;

   procedure Get_Immediate (
      File : Non_Controlled_File_Type;
      Item : out String; -- 1 .. 6
      Last : out Natural;
      Wait : Boolean)
   is
      Wanted : Natural := 1;
      Old_Settings : aliased C.termios.struct_termios;
      Dummy : C.signed_int;
      pragma Unreferenced (Dummy);
   begin
      Check_File_Mode (File, In_File);
      if File.External = IO_Text_Modes.Terminal then
         if not Wait then
            Wanted := 0;
         end if;
         --  non-canonical mode
         tcgetsetattr (
            Streams.Stream_IO.Inside.Handle (File.File),
            not C.termios.ICANON,
            C.termios.cc_t (Wanted),
            Old_Settings'Access);
      end if;
      Last := Item'First - 1;
      Multi_Character : for I in Item'Range loop
         Single_Character : loop
            Read_Buffer (File, Wanted => Wanted);
            if File.Last > 0 then
               Item (I) := File.Buffer (1);
               Last := I;
               Take_Buffer (File); -- not add File.Text.Col
               exit Single_Character; -- next character
            elsif File.End_Of_File then
               Exceptions.Raise_Exception_From_Here (End_Error'Identity);
            elsif not Wait then
               exit Multi_Character;
            end if;
         end loop Single_Character;
      end loop Multi_Character;
      if File.External = IO_Text_Modes.Terminal then
         --  restore terminal mode
         Dummy := C.termios.tcsetattr (
            Streams.Stream_IO.Inside.Handle (File.File),
            C.termios.TCSANOW,
            Old_Settings'Access);
      end if;
   end Get_Immediate;

   --  initialization

   procedure Init_Standard_File (File : not null Non_Controlled_File_Type);
   procedure Init_Standard_File (File : not null Non_Controlled_File_Type) is
   begin
      File.Stream := Streams.Stream_IO.Inside.Stream (File.File);
      File.External := IO_Text_Modes.File_External'Val (Boolean'Pos (
         not Streams.Stream_IO.Inside.Is_Terminal (
            Streams.Stream_IO.Inside.Handle (File.File))));
   end Init_Standard_File;

begin
   Init_Standard_File (Standard_Input_Text'Access);
   Init_Standard_File (Standard_Output_Text'Access);
   Init_Standard_File (Standard_Error_Text'Access);
end Ada.Text_IO.Inside;
