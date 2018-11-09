with Ada.Exception_Identification.From_Here;
with System.Address_To_Named_Access_Conversions;
with System.UTF_Conversions.From_8_To_16;
with System.UTF_Conversions.From_16_To_8;
with C.winnls;
with C.winnt;
package body System.Native_Text_IO is
   use Ada.Exception_Identification.From_Here;
   use type Ada.Streams.Stream_Element_Offset;
   use type C.signed_int; -- C.windef.WINBOOL
   use type C.windef.DWORD;
   use type C.windef.WORD;
   use type C.winnt.SHORT;
   use type C.winnt.WCHAR;

   package LPSTR_Conv is
      new Address_To_Named_Access_Conversions (C.char, C.winnt.LPSTR);

   function Processed_Input_Is_Enabled (Handle : Handle_Type) return Boolean;
   function Processed_Input_Is_Enabled (Handle : Handle_Type) return Boolean is
      Mode : aliased C.windef.DWORD;
   begin
      return C.wincon.GetConsoleMode (Handle, Mode'Access) /= C.windef.FALSE
         and then (Mode and C.wincon.ENABLE_PROCESSED_INPUT) /= 0;
   end Processed_Input_Is_Enabled;

   procedure Read_Buffer_Trailing_From_Terminal (
      Handle : Handle_Type;
      Buffer : in out Buffer_Type;
      Out_Last : out Integer; -- no error since a leading data is existing
      Leading : C.winnt.WCHAR);
   procedure Read_Buffer_Trailing_From_Terminal (
      Handle : Handle_Type;
      Buffer : in out Buffer_Type;
      Out_Last : out Integer;
      Leading : C.winnt.WCHAR)
   is
      Wide_Buffer : Wide_String (1 .. 2);
      Wide_Buffer_As_C : C.winnt.WCHAR_array (0 .. 1);
      for Wide_Buffer_As_C'Address use Wide_Buffer'Address;
      Wide_Buffer_Length : Natural;
      Read_Size : aliased C.windef.DWORD;
      UTF_16_Seq : Natural;
      Sequence_Status : UTF_Conversions.Sequence_Status_Type; -- ignore
   begin
      Wide_Buffer_As_C (0) := Leading;
      if Wide_Buffer_As_C (0) = C.winnt.WCHAR'Val (0) then
         Out_Last := 0; -- no data
      else
         Wide_Buffer_Length := 1;
         UTF_Conversions.UTF_16_Sequence (
            Wide_Character'Val (C.winnt.WCHAR'Pos (Wide_Buffer_As_C (0))),
            UTF_16_Seq,
            Sequence_Status);
         if UTF_16_Seq = 2
            and then C.wincon.ReadConsoleW (
                  hConsoleInput => Handle,
                  lpBuffer => C.windef.LPVOID (Wide_Buffer_As_C (1)'Address),
                  nNumberOfCharsToRead => 1,
                  lpNumberOfCharsRead => Read_Size'Access,
                  lpReserved => C.windef.LPVOID (Null_Address)) /=
               C.windef.FALSE
            and then Read_Size > 0
         then
            Wide_Buffer_Length := Wide_Buffer_Length + Natural (Read_Size);
         end if;
         UTF_Conversions.From_16_To_8.Convert (
            Wide_Buffer (1 .. Wide_Buffer_Length),
            Buffer,
            Out_Last);
      end if;
   end Read_Buffer_Trailing_From_Terminal;

   procedure GetConsoleScreenBufferInfo (
      ConsoleOutput : C.winnt.HANDLE;
      ConsoleScreenBufferInfo : access C.wincon.CONSOLE_SCREEN_BUFFER_INFO);
   procedure GetConsoleScreenBufferInfo (
      ConsoleOutput : C.winnt.HANDLE;
      ConsoleScreenBufferInfo : access C.wincon.CONSOLE_SCREEN_BUFFER_INFO) is
   begin
      if C.wincon.GetConsoleScreenBufferInfo (
            ConsoleOutput,
            ConsoleScreenBufferInfo) =
         C.windef.FALSE
      then
         Raise_Exception (Device_Error'Identity);
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
         if C.wincon.SetConsoleWindowInfo (ConsoleOutput, 1, Rect'Access) =
            C.windef.FALSE
         then
            Raise_Exception (Layout_Error'Identity);
         end if;
      end if;
      --  resize screen buffer
      if C.wincon.SetConsoleScreenBufferSize (ConsoleOutput, Size) =
         C.windef.FALSE
      then
         Raise_Exception (Layout_Error'Identity); -- Size is too large
      end if;
      --  maximize viewport
      GetConsoleScreenBufferInfo (ConsoleOutput, Info'Access);
      Rect.Left := 0;
      Rect.Top := 0;
      Rect.Right := Info.dwMaximumWindowSize.X - 1;
      Rect.Bottom := Info.dwMaximumWindowSize.Y - 1;
      if C.wincon.SetConsoleWindowInfo (ConsoleOutput, 1, Rect'Access) =
         C.windef.FALSE
      then
         Raise_Exception (Device_Error'Identity);
      end if;
   end SetConsoleScreenBufferSize_With_Adjusting;

   --  implementation

   procedure To_UTF_8 (
      Buffer : aliased DBCS_Buffer_Type;
      Last : Natural;
      Out_Buffer : out Buffer_Type;
      Out_Last : out Natural)
   is
      Wide_Buffer : Wide_String (1 .. 2);
      Wide_Buffer_As_C : C.winnt.WCHAR_array (0 .. 1);
      for Wide_Buffer_As_C'Address use Wide_Buffer'Address;
      Wide_Buffer_Length : C.signed_int;
      DBCS_Seq : Natural;
   begin
      DBCS_Seq :=
         1 -- leading byte
         + Boolean'Pos (
            C.winnls.IsDBCSLeadByte (
               C.windef.BYTE'(Character'Pos (Buffer (1)))) /=
            C.windef.FALSE);
      if Last = DBCS_Seq then
         Wide_Buffer_Length := C.winnls.MultiByteToWideChar (
            C.winnls.CP_ACP,
            0,
            LPSTR_Conv.To_Pointer (Buffer (1)'Address),
            C.signed_int (Last),
            Wide_Buffer_As_C (0)'Access,
            2);
         UTF_Conversions.From_16_To_8.Convert (
            Wide_Buffer (1 .. Natural (Wide_Buffer_Length)),
            Out_Buffer,
            Out_Last);
      else
         Out_Last := 0;
      end if;
   end To_UTF_8;

   procedure To_DBCS (
      Buffer : Buffer_Type;
      Last : Natural;
      Out_Buffer : aliased out DBCS_Buffer_Type;
      Out_Last : out Natural)
   is
      Wide_Buffer : Wide_String (1 .. 2);
      Wide_Buffer_Last : Natural;
      Wide_Buffer_As_C : C.winnt.WCHAR_array (0 .. 1);
      for Wide_Buffer_As_C'Address use Wide_Buffer'Address;
      Out_Length : C.signed_int;
   begin
      UTF_Conversions.From_8_To_16.Convert (
         Buffer (1 .. Last),
         Wide_Buffer,
         Wide_Buffer_Last);
      Out_Length := C.winnls.WideCharToMultiByte (
         C.winnls.CP_ACP,
         0,
         Wide_Buffer_As_C (0)'Access,
         C.signed_int (Wide_Buffer_Last),
         LPSTR_Conv.To_Pointer (Out_Buffer (1)'Address),
         Out_Buffer'Length,
         null,
         null);
      if Out_Length = 0 then
         Out_Buffer (1) := '?';
         Out_Last := 1;
      else
         Out_Last := Natural (Out_Length);
      end if;
   end To_DBCS;

   procedure Terminal_Get (
      Handle : Handle_Type;
      Item : Address;
      Length : Ada.Streams.Stream_Element_Offset;
      Out_Length : out Ada.Streams.Stream_Element_Offset)
   is
      pragma Unreferenced (Length);
      Buffer : Buffer_Type;
      for Buffer'Address use Item;
      Wide_Buffer : C.winnt.WCHAR_array (0 .. 1);
      Read_Size : aliased C.windef.DWORD;
   begin
      if C.wincon.ReadConsole (
            hConsoleInput => Handle,
            lpBuffer => C.windef.LPVOID (Wide_Buffer (0)'Address),
            nNumberOfCharsToRead => 1,
            lpNumberOfCharsRead => Read_Size'Access,
            lpReserved => C.windef.LPVOID (Null_Address)) =
         C.windef.FALSE
      then
         Out_Length := -1; -- error
      elsif Read_Size = 0
         or else (
            Wide_Buffer (0) = C.winnt.WCHAR'Val (16#1A#) -- Control+Z
            and then Processed_Input_Is_Enabled (Handle))
      then
         Out_Length := 0; -- no data
      else
         Read_Buffer_Trailing_From_Terminal (
            Handle,
            Buffer,
            Natural (Out_Length),
            Wide_Buffer (0));
      end if;
   end Terminal_Get;

   procedure Terminal_Get_Immediate (
      Handle : Handle_Type;
      Item : Address;
      Length : Ada.Streams.Stream_Element_Offset;
      Out_Length : out Ada.Streams.Stream_Element_Offset)
   is
      pragma Unreferenced (Length);
      Buffer : Buffer_Type;
      for Buffer'Address use Item;
      Event_Count : aliased C.windef.DWORD;
      Read_Size : aliased C.windef.DWORD;
      Event : aliased C.wincon.INPUT_RECORD;
   begin
      if C.wincon.GetNumberOfConsoleInputEvents (Handle, Event_Count'Access) =
         C.windef.FALSE
      then
         Out_Length := -1; -- error
      elsif Event_Count = 0 then
         Out_Length := 0; -- no data
      elsif C.wincon.ReadConsoleInput (
            hConsoleInput => Handle,
            lpBuffer => Event'Access,
            nLength => 1,
            lpNumberOfEventsRead => Read_Size'Access) =
         C.windef.FALSE
      then
         Out_Length := -1; -- error
      elsif Read_Size = 0
         or else Event.EventType /= C.wincon.KEY_EVENT
         or else Event.Event.KeyEvent.bKeyDown = C.windef.FALSE
      then
         Out_Length := 0; -- no data
      else
         Read_Buffer_Trailing_From_Terminal (
            Handle,
            Buffer,
            Natural (Out_Length),
            Event.Event.KeyEvent.uChar.UnicodeChar);
      end if;
   end Terminal_Get_Immediate;

   procedure Terminal_Put (
      Handle : Handle_Type;
      Item : Address;
      Length : Ada.Streams.Stream_Element_Offset;
      Out_Length : out Ada.Streams.Stream_Element_Offset)
   is
      Buffer : Buffer_Type;
      for Buffer'Address use Item;
      Wide_Buffer : Wide_String (1 .. 2);
      Wide_Buffer_Last : Natural;
      Written : aliased C.windef.DWORD;
   begin
      UTF_Conversions.From_8_To_16.Convert (
         Buffer (1 .. Natural (Length)),
         Wide_Buffer,
         Wide_Buffer_Last);
      if C.wincon.WriteConsoleW (
            hConsoleOutput => Handle,
            lpBuffer => C.windef.LPCVOID (Wide_Buffer (1)'Address),
            nNumberOfCharsToWrite => C.windef.DWORD (Wide_Buffer_Last),
            lpNumberOfCharsWritten => Written'Access,
            lpReserved => C.windef.LPVOID (Null_Address)) =
         C.windef.FALSE
      then
         Out_Length := -1; -- error
      else
         Out_Length := Ada.Streams.Stream_Element_Offset (Written);
      end if;
   end Terminal_Put;

   procedure Terminal_Size (
      Handle : Handle_Type;
      Line_Length, Page_Length : out Natural)
   is
      Info : aliased C.wincon.CONSOLE_SCREEN_BUFFER_INFO;
   begin
      GetConsoleScreenBufferInfo (Handle, Info'Access);
      Line_Length := Natural (Info.dwSize.X);
      Page_Length := Natural (Info.dwSize.Y);
   end Terminal_Size;

   procedure Set_Terminal_Size (
      Handle : Handle_Type;
      Line_Length, Page_Length : Natural) is
   begin
      SetConsoleScreenBufferSize_With_Adjusting (
         Handle,
         C.wincon.COORD'(
            X => C.winnt.SHORT (Line_Length),
            Y => C.winnt.SHORT (Page_Length)),
         Handle);
   end Set_Terminal_Size;

   procedure Terminal_View (
      Handle : Handle_Type;
      Left, Top : out Positive;
      Right, Bottom : out Natural)
   is
      Info : aliased C.wincon.CONSOLE_SCREEN_BUFFER_INFO;
   begin
      GetConsoleScreenBufferInfo (Handle, Info'Access);
      Left := Positive (Info.srWindow.Left + 1);
      Top := Positive (Info.srWindow.Top + 1);
      Right := Natural (Info.srWindow.Right + 1);
      Bottom := Natural (Info.srWindow.Bottom + 1);
   end Terminal_View;

   procedure Terminal_Position (
      Handle : Handle_Type;
      Col, Line : out Positive)
   is
      Info : aliased C.wincon.CONSOLE_SCREEN_BUFFER_INFO;
   begin
      GetConsoleScreenBufferInfo (Handle, Info'Access);
      Col := Positive (Info.dwCursorPosition.X + 1);
      Line := Positive (Info.dwCursorPosition.Y + 1);
   end Terminal_Position;

   procedure Set_Terminal_Position (
      Handle : Handle_Type;
      Col, Line : Positive) is
   begin
      if C.wincon.SetConsoleCursorPosition (
            Handle,
            C.wincon.COORD'(
               X => C.winnt.SHORT (Col) - 1,
               Y => C.winnt.SHORT (Line) - 1)) =
         C.windef.FALSE
      then
         Raise_Exception (Layout_Error'Identity);
      end if;
   end Set_Terminal_Position;

   procedure Set_Terminal_Col (
      Handle : Handle_Type;
      To : Positive)
   is
      Col, Line : Positive;
   begin
      Terminal_Position (Handle, Col, Line);
      Set_Terminal_Position (Handle, To, Line);
   end Set_Terminal_Col;

   procedure Terminal_Clear (
      Handle : Handle_Type)
   is
      Info : aliased C.wincon.CONSOLE_SCREEN_BUFFER_INFO;
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
         Buffer : aliased constant
               array (0 .. Info.dwSize.Y - 1, 0 .. Info.dwSize.X - 1) of
                  aliased C.wincon.CHAR_INFO :=
            (others => (others => Clear_Char_Info));
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
               lpWriteRegion => Region'Access) =
            C.windef.FALSE
         then
            Raise_Exception (Device_Error'Identity);
         end if;
      end;
      if C.wincon.SetConsoleCursorPosition (Handle, (X => 0, Y => 0)) =
         C.windef.FALSE
      then
         Raise_Exception (Device_Error'Identity);
      end if;
   end Terminal_Clear;

   procedure Set_Non_Canonical_Mode (
      Handle : Handle_Type;
      Wait : Boolean;
      Saved_Settings : aliased out Setting)
   is
      pragma Unreferenced (Wait);
   begin
      --  get and unset line-input mode
      if C.wincon.GetConsoleMode (Handle, Saved_Settings'Access) =
            C.windef.FALSE
         or else C.wincon.SetConsoleMode (
               Handle,
               Saved_Settings
                  and not (
                     C.wincon.ENABLE_ECHO_INPUT
                     or C.wincon.ENABLE_LINE_INPUT)) =
            C.windef.FALSE
      then
         Raise_Exception (Device_Error'Identity);
      end if;
   end Set_Non_Canonical_Mode;

   procedure Restore (
      Handle : Handle_Type;
      Settings : aliased Setting) is
   begin
      if C.wincon.SetConsoleMode (Handle, Settings) = C.windef.FALSE then
         Raise_Exception (Device_Error'Identity);
      end if;
   end Restore;

   procedure Set_Terminal_Attributes (
      Handle : Handle_Type;
      Attributes : C.windef.WORD) is
   begin
      if C.wincon.SetConsoleTextAttribute (Handle, Attributes) =
         C.windef.FALSE
      then
         Raise_Exception (Device_Error'Identity);
      end if;
   end Set_Terminal_Attributes;

   procedure Save_State (Handle : Handle_Type; To_State : out Output_State) is
      Info : aliased C.wincon.CONSOLE_SCREEN_BUFFER_INFO;
   begin
      if C.wincon.GetConsoleScreenBufferInfo (Handle, Info'Access) =
         C.windef.FALSE
      then
         Raise_Exception (Device_Error'Identity);
      end if;
      To_State.Position := Info.dwCursorPosition;
      To_State.Attributes := Info.wAttributes;
   end Save_State;

   procedure Reset_State (Handle : Handle_Type; From_State : Output_State) is
   begin
      Set_Terminal_Attributes (Handle, From_State.Attributes);
      Set_Terminal_Position (Handle,
         Col => Integer (From_State.Position.X + 1),
         Line => Integer (From_State.Position.Y + 1));
   end Reset_State;

end System.Native_Text_IO;
