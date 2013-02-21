with Ada.Exceptions.Finally;
with Ada.Unchecked_Deallocation;
with System.IO_Options;
with C.termios;
package body Ada.Text_IO.Inside is
   use type Streams.Stream_Element_Offset;
   use type Streams.Stream_IO.Inside.Handle_Type;
   use type Streams.Stream_IO.Stream_Access;
   use type C.unsigned_int;
   use type C.unsigned_long;

   --  implementation of handle of stream

   procedure Open (
      File : in out File_Type;
      Stream : Streams.Stream_IO.Stream_Access;
      Mode : File_Mode;
      Name : String := "";
      Form : String := "") is
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
      Form : String);

   procedure Open_File (
      Open_Proc : Open_Access;
      File : in out Non_Controlled_File_Type;
      Mode : File_Mode;
      Name : String;
      Form : String);
   procedure Open_File (
      Open_Proc : Open_Access;
      File : in out Non_Controlled_File_Type;
      Mode : File_Mode;
      Name : String;
      Form : String)
   is
      New_File : aliased Non_Controlled_File_Type := new Text_Type'(
         Name_Length => 0,
         Form_Length => 0,
         Stream => <>,
         Mode => Mode,
         Encoding => Form_Encoding (Form),
         Line_Mark => Form_Line_Mark (Form),
         Name => "",
         Form => "",
         others => <>);
      package Holder is new Exceptions.Finally.Scoped_Holder (
         Non_Controlled_File_Type,
         Finally);
   begin
      Holder.Assign (New_File'Access);
      Open_Proc.all (
         File => New_File.File,
         Mode => Streams.Stream_IO.File_Mode (Mode),
         Name => Name,
         Form => Form);
      New_File.Stream := Streams.Stream_IO.Inside.Stream (New_File.File);
      Holder.Clear;
      File := New_File;
   end Open_File;

   procedure Read_Buffer (
      File : Non_Controlled_File_Type;
      Wanted : Natural := 1);
   procedure Take_Buffer (File : Non_Controlled_File_Type);
   procedure Write_Buffer (File : Non_Controlled_File_Type);

   --  Input
   --  * Read_Buffer sets (or keep) Buffer_Col.
   --  * Get adds Buffer_Col to current Col.
   --  * Take_Buffer clear Buffer_Col.
   --  Output
   --  * Write_Buffer sets (or clear) Buffer_Col to Add Col.
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

   procedure Write_Buffer (File : Non_Controlled_File_Type) is
      Length : constant Natural := File.Last;
   begin
      if Length > 0 then
         File.Last := 0; -- before New_Line to block Flush
         if File.Line_Length /= 0
            and then File.Col > File.Line_Length
         then
            New_Line (File); -- New_Line does not touch buffer
         end if;
         declare
            Buffer : Streams.Stream_Element_Array (1 .. 1);
            for Buffer'Address use File.Buffer'Address;
         begin
            Streams.Write (File.Stream.all, Buffer);
         end;
         File.Last := Length - 1;
         File.Buffer (1 .. File.Last) := File.Buffer (2 .. Length);
         File.Buffer_Col := 1;
      else
         File.Buffer_Col := 0; -- No filled
      end if;
   end Write_Buffer;

   --  implementation of non-controlled

   procedure Create (
      File : in out Non_Controlled_File_Type;
      Mode : File_Mode := Out_File;
      Name : String := "";
      Form : String := "") is
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
      Form : String := "") is
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

   function Form (File : Non_Controlled_File_Type) return String is
   begin
      Check_File_Open (File);
      if Streams.Stream_IO.Inside.Is_Open (File.File) then
         return Streams.Stream_IO.Inside.Form (File.File);
      else
         return File.Form;
      end if;
   end Form;

   function Is_Open (File : Non_Controlled_File_Type) return Boolean is
   begin
      return File /= null;
   end Is_Open;

   procedure Flush (File : Non_Controlled_File_Type) is
   begin
      Check_File_Mode (File, Out_File);
      if File.Last > 0 then
         declare
            Buffer : Streams.Stream_Element_Array (
               1 .. Streams.Stream_Element_Offset (File.Last));
            for Buffer'Address use File.Buffer'Address;
         begin
            Streams.Write (File.Stream.all, Buffer);
         end;
         File.Last := 0;
      end if;
      if Streams.Stream_IO.Inside.Is_Open (File.File) then
         Streams.Stream_IO.Inside.Flush (File.File);
      end if;
   end Flush;

   procedure Set_Line_Length (File : Non_Controlled_File_Type; To : Count) is
   begin
      Check_File_Mode (File, Out_File);
      File.Line_Length := To;
   end Set_Line_Length;

   procedure Set_Page_Length (File : Non_Controlled_File_Type; To : Count) is
   begin
      Check_File_Mode (File, Out_File);
      File.Page_Length := To;
   end Set_Page_Length;

   function Line_Length (File : Non_Controlled_File_Type) return Count is
   begin
      Check_File_Mode (File, Out_File);
      return File.Line_Length;
   end Line_Length;

   function Page_Length (File : Non_Controlled_File_Type) return Count is
   begin
      Check_File_Mode (File, Out_File);
      return File.Page_Length;
   end Page_Length;

   procedure New_Line (
      File : Non_Controlled_File_Type;
      Spacing : Positive_Count := 1)
   is
      Line_Mark : constant Streams.
         Stream_Element_Array (0 .. 1) := (16#0d#, 16#0a#);
      F, L : Streams.Stream_Element_Offset;
   begin
      Check_File_Mode (File, Out_File);
      if File.Last > 0 then
         Flush (File);
      end if;
      F := Boolean'Pos (File.Line_Mark = LF);
      L := Boolean'Pos (File.Line_Mark /= CR);
      for I in 1 .. Spacing loop
         if File.Page_Length /= 0 and then File.Line >= File.Page_Length then
            New_Page (File);
         else
            Streams.Write (File.Stream.all, Line_Mark (F .. L));
            File.Line := File.Line + 1;
            File.Col := 1;
         end if;
      end loop;
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
                        File.Line := File.Line + 1;
                        File.Col := 1;
                        Take_Buffer (File);
                        if C = Character'Val (16#0d#) then
                           Read_Buffer (File);
                           if File.Buffer (1) = Character'Val (16#0a#) then
                              Take_Buffer (File);
                           end if;
                        end if;
                        exit;
                     when Character'Val (16#0c#) =>
                        Take_Buffer (File);
                        File.Dummy_Mark := EOP;
                        File.Page := File.Page + 1;
                        File.Line := 1;
                        File.Col := 1;
                        exit;
                     when Character'Val (16#1a#) =>
                        File.End_Of_File := True; -- for next loop
                        File.Last := 0;
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
            or else File.Buffer (1) = Character'Val (16#1a#));
      end if;
   end End_Of_Line;

   procedure New_Page (File : Non_Controlled_File_Type) is
   begin
      Check_File_Mode (File, Out_File);
      if File.Last > 0 then
         Flush (File);
      end if;
      if File.Encoding = Terminal then
         --  clear screen when tty
         declare
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
         --  tty mode is unimplemented
         if File.Line_Length /= 0 and then To > File.Line_Length then
            Exceptions.Raise_Exception_From_Here (Layout_Error'Identity);
         end if;
         if File.Col > To then
            New_Line (File);
         end if;
         while File.Col < To loop
            Put (File, ' ');
         end loop;
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
         --  tty mode is unimplemented
         if File.Page_Length /= 0 and then To > File.Page_Length then
            Exceptions.Raise_Exception_From_Here (Layout_Error'Identity);
         end if;
         if File.Line > To then
            New_Page (File);
         else
            while File.Line < To loop
               New_Line (File);
            end loop;
         end if;
      end if;
   end Set_Line;

   function Col (File : Non_Controlled_File_Type) return Positive_Count is
   begin
      --  tty mode is unimplemented
      Check_File_Open (File);
      return File.Col;
   end Col;

   function Line (File : Non_Controlled_File_Type) return Positive_Count is
   begin
      --  tty mode is unimplemented
      Check_File_Open (File);
      return File.Line;
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
                     File.Line := File.Line + 1;
                     File.Col := 1;
                     Take_Buffer (File);
                     if C = Character'Val (16#0d#) then
                        Read_Buffer (File);
                        if File.Buffer (1) = Character'Val (16#0a#) then
                           Take_Buffer (File);
                        end if;
                     end if;
                  when Character'Val (16#0c#) =>
                     File.Page := File.Page + 1;
                     File.Line := 1;
                     File.Col := 1;
                     Take_Buffer (File);
                  when Character'Val (16#1a#) =>
                     File.End_Of_File := True; -- for next loop
                     File.Last := 0;
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
   begin
      Check_File_Open (File);
      File.Last := File.Last + 1;
      File.Buffer (File.Last) := Item;
      Write_Buffer (File);
      File.Col := File.Col + File.Buffer_Col;
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

   --  implementation of handle of stream for non-controlled

   procedure Open (
      File : in out Non_Controlled_File_Type;
      Stream : Streams.Stream_IO.Stream_Access;
      Mode : File_Mode;
      Name : String := "";
      Form : String := "") is
   begin
      if Is_Open (File) then
         Exceptions.Raise_Exception_From_Here (Status_Error'Identity);
      end if;
      File := new Text_Type'(
         Name_Length => Name'Length + 1,
         Form_Length => Form'Length,
         Stream => Stream,
         Mode => Mode,
         Encoding => Form_Encoding (Form),
         Line_Mark => Form_Line_Mark (Form),
         Name => '*' & Name,
         Form => Form,
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

   --  implementation of form parameter

   function Form_Encoding (Form : String) return Encoding_Type is
      pragma Unreferenced (Form);
   begin
      return Locale;
   end Form_Encoding;

   function Form_Line_Mark (Form : String) return Line_Mark_Type is
      First : Positive;
      Last : Natural;
   begin
      System.IO_Options.Form_Parameter (Form, "lm", First, Last);
      if First <= Last and then Form (First) = 'm' then
         return CRLF;
      elsif First <= Last and then Form (First) = 'c' then
         return CR;
      else
         return LF;
      end if;
   end Form_Line_Mark;

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
            case File.Buffer (I) is
               when Character'Val (16#0a#)
                  | Character'Val (16#0c#)
                  | Character'Val (16#0d#)
                  | Character'Val (16#1a#) =>
                  Buffer_Last := I - 1;
                  End_Of_Line := True;
                  exit;
               when others =>
                  null;
            end case;
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
      Settings : aliased C.termios.struct_termios;
      Dummy : C.signed_int;
      pragma Unreferenced (Dummy);
   begin
      Check_File_Mode (File, In_File);
      if File.Encoding = Terminal then
         --  get current terminal mode
         Dummy := C.termios.tcgetattr (
            Streams.Stream_IO.Inside.Handle (File.File),
            Old_Settings'Access);
         --  set non-canonical mode
         Settings := Old_Settings;
         Settings.c_lflag := Settings.c_lflag and not C.termios.ICANON;
         if Wait then
            --  wait single byte
            Settings.c_cc (C.termios.VTIME) := 0;
            Settings.c_cc (C.termios.VMIN) := 1;
         else
            --  wait 0.1 sec
            Settings.c_cc (C.termios.VTIME) := 1;
            Settings.c_cc (C.termios.VMIN) := 0;
            Wanted := 0;
         end if;
         Dummy := C.termios.tcsetattr (
            Streams.Stream_IO.Inside.Handle (File.File),
            C.termios.TCSANOW,
            Settings'Access);
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
      if File.Encoding = Terminal then
         --  restore terminal mode
         Dummy := C.termios.tcsetattr (
            Streams.Stream_IO.Inside.Handle (File.File),
            C.termios.TCSANOW,
            Old_Settings'Access);
      end if;
   end Get_Immediate;

end Ada.Text_IO.Inside;
