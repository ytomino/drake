with Ada.Exception_Identification.From_Here;
with Ada.Exceptions.Finally;
with System.Formatting;
with System.Long_Long_Integer_Types;
with C.signal;
with C.sys.ioctl;
with C.unistd;
package body System.Native_Text_IO is
   use Ada.Exception_Identification.From_Here;
   use type Ada.Streams.Stream_Element_Offset;
   use type C.signed_int;
   use type C.unsigned_char; -- cc_t
   use type C.unsigned_int; -- tcflag_t in Linux or FreeBSD
   use type C.unsigned_long; -- tcflag_t in Darwin

   subtype Word_Unsigned is Long_Long_Integer_Types.Word_Unsigned;

   procedure tcgetsetattr (
      Handle : Handle_Type;
      Action : C.signed_int;
      Mask : C.termios.tcflag_t;
      Min : C.termios.cc_t;
      Saved_Settings : not null access C.termios.struct_termios);
   procedure tcgetsetattr (
      Handle : Handle_Type;
      Action : C.signed_int;
      Mask : C.termios.tcflag_t;
      Min : C.termios.cc_t;
      Saved_Settings : not null access C.termios.struct_termios)
   is
      Settings : aliased C.termios.struct_termios;
   begin
      --  get current terminal mode
      if C.termios.tcgetattr (Handle, Saved_Settings) < 0 then
         Raise_Exception (Device_Error'Identity);
      end if;
      --  set non-canonical mode
      Settings := Saved_Settings.all;
      Settings.c_lflag := Settings.c_lflag and Mask;
      Settings.c_cc (C.termios.VTIME) := 0; -- wait 0.0 sec
      Settings.c_cc (C.termios.VMIN) := Min; -- wait Min bytes
      if C.termios.tcsetattr (Handle, Action, Settings'Access) < 0 then
         Raise_Exception (Device_Error'Identity);
      end if;
   end tcgetsetattr;

   procedure Read_Escape_Sequence (
      Handle : Handle_Type;
      Item : out String;
      Last : out Natural;
      Read_Until : Character);
   procedure Read_Escape_Sequence (
      Handle : Handle_Type;
      Item : out String;
      Last : out Natural;
      Read_Until : Character)
   is
      Read_Length : Ada.Streams.Stream_Element_Offset;
   begin
      Last := Item'First - 1;
      loop
         Native_IO.Read (
            Handle,
            Item (Last + 1)'Address,
            1,
            Read_Length);
         if Read_Length < 0 then
            Raise_Exception (Device_Error'Identity);
         end if;
         exit when Read_Length = 0;
         if Last < Item'First then
            --  skip until 16#1b#
            if Item (Last + 1) = Character'Val (16#1b#) then
               Last := Last + 1;
            end if;
         else
            Last := Last + 1;
            exit when Item (Last) = Read_Until or else Last >= Item'Last;
         end if;
      end loop;
   end Read_Escape_Sequence;

   procedure Parse_Escape_Sequence (
      Item : String;
      Prefix : String;
      Postfix : Character;
      X1, X2 : out Word_Unsigned);
   procedure Parse_Escape_Sequence (
      Item : String;
      Prefix : String;
      Postfix : Character;
      X1, X2 : out Word_Unsigned)
   is
      P : Natural;
      Error : Boolean;
      L : constant Natural := Item'First + (Prefix'Length - 1);
   begin
      if L <= Item'Last and then Item (Item'First .. L) = Prefix then
         Formatting.Value (
            Item (L + 1 .. Item'Last),
            P,
            X1,
            Error => Error);
         if not Error and then P < Item'Last and then Item (P + 1) = ';' then
            Formatting.Value (
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
      Raise_Exception (Data_Error'Identity);
   end Parse_Escape_Sequence;

   State_Stack_Count : Natural := 0;

   --  implementation

   procedure Write_Just (
      Handle : Handle_Type;
      Item : String)
   is
      Written_Length : Ada.Streams.Stream_Element_Offset;
   begin
      Native_IO.Write (
         Handle,
         Item'Address,
         Item'Length,
         Written_Length);
      if Written_Length < 0 then
         Raise_Exception (Device_Error'Identity);
      end if;
   end Write_Just;

   procedure Terminal_Size (
      Handle : Handle_Type;
      Line_Length, Page_Length : out Natural)
   is
      WS : aliased C.sys.ioctl.struct_winsize;
   begin
      if C.sys.ioctl.ioctl (Handle, C.sys.ioctl.TIOCGWINSZ, WS'Access) < 0 then
         Raise_Exception (Device_Error'Identity);
      else
         Line_Length := Natural (WS.ws_col);
         Page_Length := Natural (WS.ws_row);
      end if;
   end Terminal_Size;

   procedure Set_Terminal_Size (
      Handle : Handle_Type;
      Line_Length, Page_Length : Natural)
   is
      Seq : String (1 .. 256);
      Last : Natural := 0;
      Error : Boolean;
   begin
      Seq (1) := Character'Val (16#1b#);
      Seq (2) := '[';
      Seq (3) := '8';
      Seq (4) := ';';
      Last := 4;
      Formatting.Image (
         Word_Unsigned (Page_Length),
         Seq (Last + 1 .. Seq'Last),
         Last,
         Error => Error);
      Last := Last + 1;
      Seq (Last) := ';';
      Formatting.Image (
         Word_Unsigned (Line_Length),
         Seq (Last + 1 .. Seq'Last),
         Last,
         Error => Error);
      Last := Last + 1;
      Seq (Last) := 't';
      Write_Just (Handle, Seq (1 .. Last));
   end Set_Terminal_Size;

   procedure Terminal_View (
      Handle : Handle_Type;
      Left, Top : out Positive;
      Right, Bottom : out Natural) is
   begin
      Terminal_Size (Handle, Right, Bottom);
      Left := 1;
      Top := 1;
   end Terminal_View;

   function Use_Terminal_Position (Handle : Handle_Type) return Boolean is
   begin
      --  It's a workaround for that in some kinds of combinations of
      --    commands like timeout(1), the process may be run as background,
      --    so it may receive SIGTTOU by tcsetattr and be stopped.
      return C.unistd.tcgetpgrp (Handle) = C.unistd.getpgrp;
   end Use_Terminal_Position;

   procedure Terminal_Position (
      Handle : Handle_Type;
      Col, Line : out Positive)
   is
      Seq : constant String (1 .. 4) :=
         (Character'Val (16#1b#), '[', '6', 'n');
      type Signal_Setting is record
         Old_Mask : aliased C.signal.sigset_t;
         Error : Boolean;
      end record;
      pragma Suppress_Initialization (Signal_Setting);
      SS : aliased Signal_Setting;
      type Terminal_Setting is record
         Old_Settings : aliased C.termios.struct_termios;
         Handle : Handle_Type;
         Error : Boolean;
      end record;
      pragma Suppress_Initialization (Terminal_Setting);
      TS : aliased Terminal_Setting;
      Buffer : String (1 .. 256);
      Last : Natural;
   begin
      --  block SIGINT
      declare
         Mask : aliased C.signal.sigset_t;
         Dummy_R : C.signed_int;
      begin
         Dummy_R := C.signal.sigemptyset (Mask'Access);
         Dummy_R := C.signal.sigaddset (Mask'Access, C.signal.SIGINT);
         if C.signal.sigprocmask (
            C.signal.SIG_BLOCK,
            Mask'Access,
            SS.Old_Mask'Access) < 0
         then
            raise Program_Error; -- sigprocmask failed
         end if;
      end;
      declare
         procedure Finally (X : in out Signal_Setting);
         procedure Finally (X : in out Signal_Setting) is
         begin
            --  unblock SIGINT
            X.Error := C.signal.sigprocmask (
               C.signal.SIG_SETMASK,
               X.Old_Mask'Access,
               null) < 0;
         end Finally;
         package Holder is
            new Ada.Exceptions.Finally.Scoped_Holder (
               Signal_Setting,
               Finally);
      begin
         Holder.Assign (SS);
         TS.Handle := Handle;
         --  non-canonical mode and disable echo
         tcgetsetattr (
            Handle,
            C.termios.TCSAFLUSH,
            not (C.termios.ECHO or C.termios.ICANON),
            1,
            TS.Old_Settings'Access);
         declare
            procedure Finally (X : in out Terminal_Setting);
            procedure Finally (X : in out Terminal_Setting) is
            begin
               --  restore terminal mode
               X.Error := C.termios.tcsetattr (
                  X.Handle,
                  C.termios.TCSANOW,
                  X.Old_Settings'Access) < 0;
            end Finally;
            package Holder is
               new Ada.Exceptions.Finally.Scoped_Holder (
                  Terminal_Setting,
                  Finally);
         begin
            Holder.Assign (TS);
            --  output
            Write_Just (Handle, Seq);
            --  input
            Read_Escape_Sequence (Handle, Buffer, Last, 'R');
         end;
         if TS.Error then
            Raise_Exception (Device_Error'Identity);
         end if;
      end;
      if SS.Error then
         raise Program_Error; -- sigprocmask failed
      end if;
      --  parse
      Parse_Escape_Sequence (
         Buffer (1 .. Last),
         Character'Val (16#1b#) & "[",
         'R',
         Word_Unsigned (Line),
         Word_Unsigned (Col));
   end Terminal_Position;

   procedure Set_Terminal_Position (
      Handle : Handle_Type;
      Col, Line : Positive)
   is
      Seq : String (1 .. 256);
      Last : Natural := 0;
      Error : Boolean;
   begin
      Seq (1) := Character'Val (16#1b#);
      Seq (2) := '[';
      Last := 2;
      Formatting.Image (
         Word_Unsigned (Line),
         Seq (Last + 1 .. Seq'Last),
         Last,
         Error => Error);
      Last := Last + 1;
      Seq (Last) := ';';
      Formatting.Image (
         Word_Unsigned (Col),
         Seq (Last + 1 .. Seq'Last),
         Last,
         Error => Error);
      Last := Last + 1;
      Seq (Last) := 'H';
      Write_Just (Handle, Seq (1 .. Last));
   end Set_Terminal_Position;

   procedure Set_Terminal_Col (
      Handle : Handle_Type;
      To : Positive)
   is
      Seq : String (1 .. 256);
      Last : Natural := 0;
      Error : Boolean;
   begin
      Seq (1) := Character'Val (16#1b#);
      Seq (2) := '[';
      Last := 2;
      Formatting.Image (
         Word_Unsigned (To),
         Seq (Last + 1 .. Seq'Last),
         Last,
         Error => Error);
      Last := Last + 1;
      Seq (Last) := 'G';
      Write_Just (Handle, Seq (1 .. Last));
   end Set_Terminal_Col;

   procedure Terminal_Clear (
      Handle : Handle_Type)
   is
      Code : constant String (1 .. 10) := (
         Character'Val (16#1b#),
         '[',
         '2',
         'J',
         Character'Val (16#1b#),
         '[',
         '0',
         ';',
         '0',
         'H');
   begin
      Write_Just (Handle, Code);
   end Terminal_Clear;

   procedure Set_Non_Canonical_Mode (
      Handle : Handle_Type;
      Wait : Boolean;
      Saved_Settings : aliased out Setting) is
   begin
      tcgetsetattr (
         Handle,
         C.termios.TCSADRAIN,
         not C.termios.ICANON,
         C.termios.cc_t (Boolean'Pos (Wait)), -- minimum waiting size
         Saved_Settings'Access);
   end Set_Non_Canonical_Mode;

   procedure Restore (
      Handle : Handle_Type;
      Settings : aliased Setting) is
   begin
      if C.termios.tcsetattr (
         Handle,
         C.termios.TCSANOW,
         Settings'Access) < 0
      then
         Raise_Exception (Device_Error'Identity);
      end if;
   end Restore;

   procedure Save_State (Handle : Handle_Type; To_State : out Output_State) is
      Seq : constant String (1 .. 2) := (Character'Val (16#1b#), '7');
   begin
      State_Stack_Count := State_Stack_Count + 1;
      To_State := State_Stack_Count;
      Write_Just (Handle, Seq);
   end Save_State;

   procedure Reset_State (Handle : Handle_Type; From_State : Output_State) is
      pragma Check (Pre,
         Check => From_State = State_Stack_Count or else raise Status_Error);
      Seq : constant String (1 .. 2) := (Character'Val (16#1b#), '8');
   begin
      State_Stack_Count := State_Stack_Count - 1;
      Write_Just (Handle, Seq);
   end Reset_State;

end System.Native_Text_IO;
