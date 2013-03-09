with Ada.Exceptions.Finally;
with Ada.Unchecked_Deallocation;
with System.Address_To_Named_Access_Conversions;
with System.IO_Options;
with System.UTF_Conversions;
with System.UTF_Conversions.From_8_To_16;
with System.UTF_Conversions.From_16_To_8;
with C.windef;
with C.wincon;
with C.winnls;
with C.winnt;
package body Ada.Text_IO.Inside is
   use type Streams.Stream_Element_Offset;
   use type Streams.Stream_IO.Inside.Handle_Type;
   use type Streams.Stream_IO.Stream_Access;
   use type C.size_t;
   use type C.windef.WORD;
   use type C.windef.DWORD;
   use type C.windef.WINBOOL;
   use type C.winnt.SHORT;
   use type C.winnt.WCHAR;

   pragma Compile_Time_Error (Wide_Character'Size /= C.winnt.WCHAR'Size,
      "WCHAR'Size is mismatch");

   package LPSTR_Conv is new System.Address_To_Named_Access_Conversions (
      C.winnt.CCHAR,
      C.winnt.LPSTR);

   procedure GetConsoleScreenBufferInfo (
      ConsoleOutput : C.winnt.HANDLE;
      ConsoleScreenBufferInfo : access C.wincon.CONSOLE_SCREEN_BUFFER_INFO);
   procedure GetConsoleScreenBufferInfo (
      ConsoleOutput : C.winnt.HANDLE;
      ConsoleScreenBufferInfo : access C.wincon.CONSOLE_SCREEN_BUFFER_INFO) is
   begin
      if C.wincon.GetConsoleScreenBufferInfo (
         ConsoleOutput,
         ConsoleScreenBufferInfo) = 0
      then
         Exceptions.Raise_Exception_From_Here (Device_Error'Identity);
      end if;
   end GetConsoleScreenBufferInfo;

   procedure SetConsoleScreenBufferSize_With_Adjusting (
      ConsoleOutput : C.winnt.HANDLE;
      Size : C.wincon.COORD;
      Current : C.winnt.HANDLE);
   procedure SetConsoleScreenBufferSize_With_Adjusting (
      ConsoleOutput : C.winnt.HANDLE;
      Size : C.wincon.COORD;
      Current : C.winnt.HANDLE)
   is
      Info, Old_Info : aliased C.wincon.CONSOLE_SCREEN_BUFFER_INFO;
      Old_Size : C.wincon.COORD;
      Rect : aliased C.wincon.SMALL_RECT;
   begin
      --  resize viewport to smaller than current window
      GetConsoleScreenBufferInfo (Current, Old_Info'Access);
      Old_Size.X := Old_Info.srWindow.Right - Old_Info.srWindow.Left + 1;
      Old_Size.Y := Old_Info.srWindow.Bottom - Old_Info.srWindow.Top + 1;
      if Size.X < Old_Size.X or else Size.Y < Old_Size.Y then
         Rect.Left := 0;
         Rect.Top := 0;
         Rect.Right := C.winnt.SHORT'Min (Size.X, Old_Size.X) - 1;
         Rect.Bottom := C.winnt.SHORT'Min (Size.Y, Old_Size.Y) - 1;
         if C.wincon.SetConsoleWindowInfo (
            ConsoleOutput,
            1,
            Rect'Access) = 0
         then
            Exceptions.Raise_Exception_From_Here (Device_Error'Identity);
         end if;
      end if;
      --  resize screen buffer
      if C.wincon.SetConsoleScreenBufferSize (ConsoleOutput, Size) = 0 then
         Exceptions.Raise_Exception_From_Here (Device_Error'Identity);
      end if;
      --  maximize viewport
      GetConsoleScreenBufferInfo (ConsoleOutput, Info'Access);
      Rect.Left := 0;
      Rect.Top := 0;
      Rect.Right := Info.dwMaximumWindowSize.X - 1;
      Rect.Bottom := Info.dwMaximumWindowSize.Y - 1;
      if C.wincon.SetConsoleWindowInfo (
         ConsoleOutput,
         1,
         Rect'Access) = 0
      then
         Exceptions.Raise_Exception_From_Here (Device_Error'Identity);
      end if;
   end SetConsoleScreenBufferSize_With_Adjusting;

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
         Encoding => <>,
         Line_Mark => <>,
         SUB => <>,
         Name => "",
         Form => "",
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
         Form => Form);
      New_File.Stream := Streams.Stream_IO.Inside.Stream (New_File.File);
      --  select encoding
      if Streams.Stream_IO.Inside.Is_Terminal (
         Streams.Stream_IO.Inside.Handle (New_File.File))
      then
         New_File.Encoding := Terminal;
         New_File.Line_Mark := CRLF;
         New_File.SUB := False;
      else
         New_File.Encoding := Form_Encoding (Form);
         New_File.Line_Mark := Form_Line_Mark (Form);
         New_File.SUB := Form_SUB (Form);
      end if;
      --  complete
      Holder.Clear;
      File := New_File;
   end Open_File;

   procedure Raw_New_Page (File : Non_Controlled_File_Type);
   procedure Raw_New_Page (File : Non_Controlled_File_Type) is
   begin
      if File.Encoding = Terminal then
         declare -- clear screen
            Info : aliased C.wincon.CONSOLE_SCREEN_BUFFER_INFO;
            Handle : constant Streams.Stream_IO.Inside.Handle_Type :=
               Streams.Stream_IO.Inside.Handle (File.File);
         begin
            GetConsoleScreenBufferInfo (
               Handle,
               Info'Access);
            declare
               Clear_Char_Info : constant C.wincon.CHAR_INFO := (
                  Char => (
                     Unchecked_Tag => 0,
                     UnicodeChar => C.winnt.WCHAR'Val (16#20#)),
                  Attributes => Info.wAttributes);
               Buffer : aliased constant array (
                  0 .. Info.dwSize.Y - 1,
                  0 .. Info.dwSize.X - 1) of aliased C.wincon.CHAR_INFO := (
                     others => (others => Clear_Char_Info));
               Region : aliased C.wincon.SMALL_RECT;
            begin
               Region.Left := 0;
               Region.Top := 0;
               Region.Right := Info.dwSize.X - 1;
               Region.Bottom := Info.dwSize.Y - 1;
               if C.wincon.WriteConsoleOutputW (
                  hConsoleOutput => Handle,
                  lpBuffer => Buffer (0, 0)'Access,
                  dwBufferSize => Info.dwSize,
                  dwBufferCoord => (X => 0, Y => 0),
                  lpWriteRegion => Region'Access) = 0
               then
                  Exceptions.Raise_Exception_From_Here (Device_Error'Identity);
               end if;
            end;
            if C.wincon.SetConsoleCursorPosition (
               Handle,
               (X => 0, Y => 0)) = 0
            then
               Exceptions.Raise_Exception_From_Here (Device_Error'Identity);
            end if;
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
            F := Boolean'Pos (File.Line_Mark = LF);
            L := Boolean'Pos (File.Line_Mark /= CR);
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

   procedure Read_Buffer (File : Non_Controlled_File_Type);
   procedure Read_Buffer_From_Event (File : Non_Controlled_File_Type);
   procedure Read_Buffer_Trailing_From_Terminal (
      File : Non_Controlled_File_Type;
      Leading : C.winnt.WCHAR);
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

   procedure Read_Buffer (File : Non_Controlled_File_Type) is
      Ada_Buffer : Wide_String (1 .. 2);
      W_Buffer : C.winnt.WCHAR_array (0 .. 1);
      for W_Buffer'Address use Ada_Buffer'Address;
      W_Buffer_Length : C.size_t;
      Read_Size : aliased C.windef.DWORD;
      DBCS_Seq : Natural;
   begin
      if File.End_Of_File then
         null;
      elsif File.Last = 0 or else not File.Converted then
         File.Converted := False;
         if File.Encoding = Terminal
            and then C.wincon.ReadConsole (
               hConsoleInput => Streams.Stream_IO.Inside.Handle (File.File),
               lpBuffer => C.windef.LPVOID (W_Buffer (0)'Address),
               nNumberOfCharsToRead => 1,
               lpNumberOfCharsRead => Read_Size'Access,
               lpReserved => C.windef.LPVOID (System.Null_Address)) /= 0
            and then Read_Size > 0
         then
            Read_Buffer_Trailing_From_Terminal (
               File,
               W_Buffer (0));
         else
            declare
               Buffer : Streams.Stream_Element_Array (1 .. 1);
               for Buffer'Address use File.Buffer (File.Last + 1)'Address;
               Last : Streams.Stream_Element_Offset;
            begin
               begin
                  Streams.Read (File.Stream.all, Buffer, Last);
               exception
                  when End_Error => Last := 0;
               end;
               File.Last := File.Last + Natural (Last);
               if Last = 0 then
                  File.End_Of_File := True;
               end if;
            end;
            if File.Last = 0 then
               null; -- read 0 byte
            elsif File.Encoding = Locale
               and then File.Buffer (1) >= Character'Val (16#80#)
            then
               DBCS_Seq := 1 + Boolean'Pos (
                  C.winnls.IsDBCSLeadByte (
                     C.windef.BYTE'(Character'Pos (File.Buffer (1)))) /= 0);
               if File.Last = DBCS_Seq then
                  W_Buffer_Length := C.size_t (C.winnls.MultiByteToWideChar (
                     C.winnls.CP_ACP,
                     0,
                     LPSTR_Conv.To_Pointer (File.Buffer (1)'Address),
                     C.signed_int (File.Last),
                     W_Buffer (0)'Access,
                     2));
                  System.UTF_Conversions.From_16_To_8.Convert (
                     Ada_Buffer (1 .. Natural (W_Buffer_Length)),
                     File.Buffer,
                     File.Last);
                  File.Converted := True;
                  File.Buffer_Col := 2;
               else
                  null; --  No converted
               end if;
            else
               File.Converted := True;
               File.Buffer_Col := 1;
            end if;
         end if;
      else
         null; --  No filled
      end if;
   end Read_Buffer;

   procedure Read_Buffer_From_Event (File : Non_Controlled_File_Type) is
      Handle : Streams.Stream_IO.Inside.Handle_Type;
      Event_Count : aliased C.windef.DWORD;
      Read_Size : aliased C.windef.DWORD;
      Event : aliased C.wincon.INPUT_RECORD;
   begin
      if not File.End_Of_File
         and then (File.Last = 0 or else not File.Converted)
      then
         Handle := Streams.Stream_IO.Inside.Handle (File.File);
         if C.wincon.GetNumberOfConsoleInputEvents (
            Handle,
            Event_Count'Access) /= 0
            and then Event_Count > 0
            and then C.wincon.ReadConsoleInput (
               hConsoleInput => Handle,
               lpBuffer => Event'Access,
               nLength => 1,
               lpNumberOfEventsRead => Read_Size'Access) /= 0
            and then Read_Size > 0
         then
            if Event.EventType = C.wincon.KEY_EVENT
               and then Event.Event.KeyEvent.bKeyDown /= 0
            then
               Read_Buffer_Trailing_From_Terminal (
                  File,
                  Event.Event.KeyEvent.uChar.UnicodeChar);
            end if;
         end if;
      end if;
   end Read_Buffer_From_Event;

   procedure Read_Buffer_Trailing_From_Terminal (
      File : Non_Controlled_File_Type;
      Leading : C.winnt.WCHAR)
   is
      Ada_Buffer : Wide_String (1 .. 2);
      W_Buffer : C.winnt.WCHAR_array (0 .. 1);
      for W_Buffer'Address use Ada_Buffer'Address;
      W_Buffer_Length : C.size_t;
      Read_Size : aliased C.windef.DWORD;
      UTF_16_Seq : Natural;
      Error : Boolean;
   begin
      W_Buffer (0) := Leading;
      if W_Buffer (0) /= C.winnt.WCHAR'Val (0) then
         W_Buffer_Length := 1;
         System.UTF_Conversions.UTF_16_Sequence (
            Wide_Character'Val (C.winnt.WCHAR'Pos (W_Buffer (0))),
            UTF_16_Seq,
            Error);
         if UTF_16_Seq = 2
            and then C.wincon.ReadConsoleW (
               hConsoleInput => Streams.Stream_IO.Inside.Handle (File.File),
               lpBuffer => C.windef.LPVOID (W_Buffer (1)'Address),
               nNumberOfCharsToRead => 1,
               lpNumberOfCharsRead => Read_Size'Access,
               lpReserved =>
                  C.windef.LPVOID (System.Null_Address)) /= 0
            and then Read_Size > 0
         then
            W_Buffer_Length := W_Buffer_Length + C.size_t (Read_Size);
         end if;
         System.UTF_Conversions.From_16_To_8.Convert (
            Ada_Buffer (1 .. Natural (W_Buffer_Length)),
            File.Buffer,
            File.Last);
         File.Converted := True;
         File.Buffer_Col := 0; -- unused
      end if;
   end Read_Buffer_Trailing_From_Terminal;

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
      Ada_Buffer : Wide_String (1 .. 2);
      Ada_Buffer_Last : Natural;
      W_Buffer : C.winnt.WCHAR_array (0 .. 1);
      for W_Buffer'Address use Ada_Buffer'Address;
      Written : aliased C.windef.DWORD;
      Length : constant Natural := File.Last; -- >= Sequence_Length
   begin
      System.UTF_Conversions.From_8_To_16.Convert (
         File.Buffer (1 .. Sequence_Length),
         Ada_Buffer,
         Ada_Buffer_Last);
      if File.Encoding = Terminal
         and then C.wincon.WriteConsoleW (
            hConsoleOutput => Streams.Stream_IO.Inside.Handle (File.File),
            lpBuffer => C.windef.LPVOID (Ada_Buffer (1)'Address),
            nNumberOfCharsToWrite => C.windef.DWORD (Ada_Buffer_Last),
            lpNumberOfCharsWritten => Written'Access,
            lpReserved => C.windef.LPVOID (System.Null_Address)) /= 0
         and then Written = C.windef.DWORD (Ada_Buffer_Last)
      then
         File.Buffer_Col := 0; -- unused
      elsif File.Encoding = UTF_8
         or else (
            Sequence_Length = 1
            and then File.Buffer (1) < Character'Val (16#80#))
      then
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
      else
         declare
            DBCS_Buffer : String (1 .. 2);
            DBCS_Last : Natural;
         begin
            DBCS_Last := Natural (C.winnls.WideCharToMultiByte (
               C.winnls.CP_ACP,
               0,
               W_Buffer (0)'Access,
               C.signed_int (Ada_Buffer_Last),
               LPSTR_Conv.To_Pointer (DBCS_Buffer (1)'Address),
               DBCS_Buffer'Length,
               null,
               null));
            if DBCS_Last = 0 then
               DBCS_Buffer (1) := '?';
               DBCS_Last := 1;
            end if;
            if File.Line_Length /= 0
               and then File.Col + Count (DBCS_Last - 1) > File.Line_Length
            then
               Raw_New_Line (File);
            end if;
            declare
               Buffer : Streams.Stream_Element_Array (
                  1 ..
                  Streams.Stream_Element_Offset (DBCS_Last));
               for Buffer'Address use DBCS_Buffer'Address;
            begin
               Streams.Write (File.Stream.all, Buffer);
            end;
            File.Buffer_Col := Count (DBCS_Last);
         end;
      end if;
      File.Last := Length - Sequence_Length;
      File.Buffer (1 .. File.Last) :=
         File.Buffer (Sequence_Length + 1 .. Length);
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
         File.all.Converted := False;
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

   function Encoding (File : Non_Controlled_File_Type) return Encoding_Type is
   begin
      Check_File_Open (File);
      return File.Encoding;
   end Encoding;

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
      if Streams.Stream_IO.Inside.Is_Open (File.File)
         and then File.Encoding /= Terminal -- console can not flush
      then
         Streams.Stream_IO.Inside.Flush (File.File);
      end if;
   end Flush;

   procedure Set_Size (
      File : Non_Controlled_File_Type;
      Line_Length, Page_Length : Count)
   is
      Handle : Streams.Stream_IO.Inside.Handle_Type;
   begin
      Check_File_Mode (File, Out_File);
      if File.Encoding = Terminal then
         Handle := Streams.Stream_IO.Inside.Handle (File.File);
         SetConsoleScreenBufferSize_With_Adjusting (
            Handle,
            C.wincon.COORD'(
               X => C.winnt.SHORT (Line_Length),
               Y => C.winnt.SHORT (Page_Length)),
            Handle);
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
      Info : aliased C.wincon.CONSOLE_SCREEN_BUFFER_INFO;
   begin
      Check_File_Mode (File, Out_File);
      if File.Encoding = Terminal then
         GetConsoleScreenBufferInfo (
            Streams.Stream_IO.Inside.Handle (File.File),
            Info'Access);
         Line_Length := Count (Info.dwSize.X);
         Page_Length := Count (Info.dwSize.Y);
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
            elsif File.Last > 0 then -- ASCII
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
                        if File.SUB then
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
         return File.Last > 0 and then ( -- line mark is ASCII
            File.Buffer (1) = Character'Val (16#0d#)
            or else File.Buffer (1) = Character'Val (16#0a#)
            or else File.Buffer (1) = Character'Val (16#0c#)
            or else (
               File.SUB and then File.Buffer (1) = Character'Val (16#1a#)));
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
         return File.Last > 0 -- page mark is ASCII
            and then File.Buffer (1) = Character'Val (16#0c#);
      end if;
   end End_Of_Page;

   function End_Of_File (File : Non_Controlled_File_Type) return Boolean is
   begin
      Check_File_Mode (File, In_File);
      if File.Last > 0 then
         return False;
      elsif not Streams.Stream_IO.Inside.Is_Open (File.File)
         or else File.Encoding = Terminal
      then
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
      Col, Line : Count)
   is
      Info : aliased C.wincon.CONSOLE_SCREEN_BUFFER_INFO;
      Handle : Streams.Stream_IO.Inside.Handle_Type;
   begin
      Check_File_Mode (File, Out_File);
      if File.Encoding = Terminal then
         Handle := Streams.Stream_IO.Inside.Handle (File.File);
         GetConsoleScreenBufferInfo (
            Handle,
            Info'Access);
         if C.wincon.SetConsoleCursorPosition (
            Handle,
            C.wincon.COORD'(
               X => C.winnt.SHORT (Col) - 1,
               Y => C.winnt.SHORT (Line) - 1)) = 0
         then
            Exceptions.Raise_Exception_From_Here (Layout_Error'Identity);
         end if;
      else
         Exceptions.Raise_Exception_From_Here (Device_Error'Identity);
      end if;
   end Set_Position_Within_Terminal;

   procedure Set_Col_Within_Terminal (
      File : Non_Controlled_File_Type;
      To : Count) is
   begin
      Set_Position_Within_Terminal (File, To, Line (File));
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
         if File.Encoding = Terminal then
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
         if File.Encoding = Terminal then
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
      Info : aliased C.wincon.CONSOLE_SCREEN_BUFFER_INFO;
   begin
      Check_File_Open (File);
      if File.Encoding = Terminal then
         GetConsoleScreenBufferInfo (
            Streams.Stream_IO.Inside.Handle (File.File),
            Info'Access);
         Col := Positive_Count (Info.dwCursorPosition.X + 1);
         Line := Positive_Count (Info.dwCursorPosition.Y + 1);
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
         elsif File.Last > 0 and then File.Converted then
            declare
               C : constant Character := File.Buffer (1);
            begin
               case C is
                  when Character'Val (16#0d#) | Character'Val (16#0a#) =>
                     Take_Line (File);
                  when Character'Val (16#0c#) =>
                     Take_Page (File);
                  when Character'Val (16#1a#) =>
                     if File.SUB then
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
      Error : Boolean;
   begin
      Check_File_Open (File);
      --  if Item is not trailing byte, flush the buffer
      if File.Last > 0
         and then Character'Pos (Item) not in 2#10000000# .. 2#10111111#
      then
         Write_Buffer (File, File.Last);
         File.Col := File.Col + File.Buffer_Col;
      end if;
      --  write to the buffer
      File.Last := File.Last + 1;
      File.Buffer (File.Last) := Item;
      System.UTF_Conversions.UTF_8_Sequence (
         File.Buffer (1),
         Sequence_Length,
         Error);
      if File.Last >= Sequence_Length then
         Write_Buffer (File, Sequence_Length);
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
         elsif File.Last > 0 and then File.Converted then
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
      Right, Bottom : out Count)
   is
      Info : aliased C.wincon.CONSOLE_SCREEN_BUFFER_INFO;
   begin
      Check_File_Mode (File, Out_File);
      if File.Encoding = Terminal then
         GetConsoleScreenBufferInfo (
            Streams.Stream_IO.Inside.Handle (File.File),
            Info'Access);
         Left := Positive_Count (Info.srWindow.Left + 1);
         Top := Positive_Count (Info.srWindow.Top + 1);
         Right := Count (Info.srWindow.Right + 1);
         Bottom := Count (Info.srWindow.Bottom + 1);
      else
         Exceptions.Raise_Exception_From_Here (Device_Error'Identity);
      end if;
   end View;

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
         SUB => Form_SUB (Form),
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
      First : Positive;
      Last : Natural;
   begin
      System.IO_Options.Form_Parameter (Form, "external", First, Last);
      if First > Form'First then
         if First <= Last
            and then Form (First) = 'u'
            and then Form (Last) = '8'
         then -- "utf-8"
            return UTF_8;
         else
            return Locale;
         end if;
      else
         --  compatibility with GNAT runtime
         System.IO_Options.Form_Parameter (Form, "wcem", First, Last);
         if First <= Last and then Form (First) = '8' then
            return UTF_8;
         else
            return Locale;
         end if;
      end if;
   end Form_Encoding;

   function Form_Line_Mark (Form : String) return Line_Mark_Type is
      First : Positive;
      Last : Natural;
   begin
      System.IO_Options.Form_Parameter (Form, "lm", First, Last);
      if First <= Last and then Form (First) = 'c' then -- cr
         return CR;
      elsif First <= Last and then Form (First) = 'l' then -- lf
         return LF;
      else -- ms
         return CRLF;
      end if;
   end Form_Line_Mark;

   function Form_SUB (Form : String) return Boolean is
      First : Positive;
      Last : Natural;
   begin
      System.IO_Options.Form_Parameter (Form, "sub", First, Last);
      if First <= Last and then Form (First) = 'e' then
         return True;
      else
         return False;
      end if;
   end Form_SUB;

   --  implementation for Wide_Text_IO/Wide_Wide_Text_IO

   procedure Look_Ahead (
      File : Non_Controlled_File_Type;
      Item : out String; -- 1 .. 6
      Last : out Natural;
      End_Of_Line : out Boolean)
   is
      Buffer_Last : Natural;
   begin
      Check_File_Open (File);
      loop
         Read_Buffer (File);
         exit when (File.Last > 0 and then File.Converted)
            or else File.End_Of_File;
      end loop;
      if not (File.Last > 0 and then File.Converted) then
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
                  File.SUB and then File.Buffer (I) = Character'Val (16#1a#))
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
      Console_Mode : aliased C.windef.DWORD;
      Handle : Streams.Stream_IO.Inside.Handle_Type;
   begin
      Check_File_Mode (File, In_File);
      if File.Encoding = Terminal then
         Handle := Streams.Stream_IO.Inside.Handle (File.File);
         --  get and unset line-input mode
         if C.wincon.GetConsoleMode (
            Handle,
            Console_Mode'Access) = 0
            or else C.wincon.SetConsoleMode (
               Handle,
               Console_Mode and not (
                  C.wincon.ENABLE_ECHO_INPUT
                  or C.wincon.ENABLE_LINE_INPUT)) = 0
         then
            Exceptions.Raise_Exception_From_Here (Device_Error'Identity);
         end if;
      end if;
      Last := Item'First - 1;
      Multi_Character : for I in Item'Range loop
         Single_Character : loop
            if not Wait and then File.Encoding = Terminal then
               Read_Buffer_From_Event (File);
            else
               Read_Buffer (File);
            end if;
            if File.Last > 0 and then File.Converted then
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
         if C.wincon.SetConsoleMode (
            Handle,
            Console_Mode) = 0
         then
            Exceptions.Raise_Exception_From_Here (Device_Error'Identity);
         end if;
      end if;
   end Get_Immediate;

   --  initialization

   procedure Init_Standard_File (File : not null Non_Controlled_File_Type);
   procedure Init_Standard_File (File : not null Non_Controlled_File_Type) is
   begin
      File.Stream := Streams.Stream_IO.Inside.Stream (File.File);
      File.Encoding := Encoding_Type'Val (Boolean'Pos (
         Streams.Stream_IO.Inside.Is_Terminal (
            Streams.Stream_IO.Inside.Handle (File.File))));
   end Init_Standard_File;

begin
   Init_Standard_File (Standard_Input_Text'Access);
   Init_Standard_File (Standard_Output_Text'Access);
   Init_Standard_File (Standard_Error_Text'Access);
end Ada.Text_IO.Inside;
