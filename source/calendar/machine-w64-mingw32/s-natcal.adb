--  reference:
--  https://blogs.msdn.microsoft.com/oldnewthing/20140307-00/?p=1573
with System.Native_Time;
with System.Storage_Elements;
with C.winbase;
with C.winnt;
package body System.Native_Calendar is
   use type System.Native_Time.Nanosecond_Number;
   use type Storage_Elements.Storage_Offset;
   use type C.windef.WINBOOL;
   use type C.windef.WORD;

   procedure memset (
      b : not null access C.winbase.TIME_ZONE_INFORMATION;
      c : Integer;
      n : Storage_Elements.Storage_Count)
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_memset";

   Diff : constant := 17324755200_000_000_0;
      --  100-nanoseconds from 1601-01-01 (0 of FILETIME)
      --    to 2150-01-01 (0 of Ada time)

   function "+" (Left : C.windef.FILETIME; Right : Duration)
      return C.windef.FILETIME
      with Convention => Intrinsic;
   function "-" (Left : C.windef.FILETIME; Right : Duration)
      return C.windef.FILETIME
      with Convention => Intrinsic;
   function "-" (Left, Right : C.windef.FILETIME) return Duration
      with Convention => Intrinsic;
   pragma Inline_Always ("+");
   pragma Inline_Always ("-");

   function "+" (Left : C.windef.FILETIME; Right : Duration)
      return C.windef.FILETIME
   is
      use type C.winnt.ULONGLONG;
      Left_U : constant C.winnt.ULARGE_INTEGER := (
         Unchecked_Tag => 0,
         LowPart => Left.dwLowDateTime,
         HighPart => Left.dwHighDateTime);
      Result_U : constant C.winnt.ULARGE_INTEGER := (
         Unchecked_Tag => 255, -- any value in others
         QuadPart => Left_U.QuadPart
            + C.winnt.ULONGLONG (
               System.Native_Time.Nanosecond_Number'Integer_Value (Right)
                  / 100));
   begin
      return (
         dwLowDateTime => Result_U.LowPart,
         dwHighDateTime => Result_U.HighPart);
   end "+";

   function "-" (Left : C.windef.FILETIME; Right : Duration)
      return C.windef.FILETIME is
   begin
      return Left + (-Right);
   end "-";

   function "-" (Left, Right : C.windef.FILETIME) return Duration is
      use type C.winnt.ULONGLONG;
      Left_U : constant C.winnt.ULARGE_INTEGER := (
         Unchecked_Tag => 0,
         LowPart => Left.dwLowDateTime,
         HighPart => Left.dwHighDateTime);
      Right_U : constant C.winnt.ULARGE_INTEGER := (
         Unchecked_Tag => 0,
         LowPart => Right.dwLowDateTime,
         HighPart => Right.dwHighDateTime);
   begin
      return Duration'Fixed_Value ((Left_U.QuadPart - Right_U.QuadPart) * 100);
   end "-";

   procedure Make_TZI (
      TZI : aliased out C.winbase.TIME_ZONE_INFORMATION;
      Time_Zone : Time_Offset);
   procedure Make_TZI (
      TZI : aliased out C.winbase.TIME_ZONE_INFORMATION;
      Time_Zone : Time_Offset) is
   begin
      memset (
         TZI'Access,
         0,
         C.winbase.TIME_ZONE_INFORMATION'Size / Standard'Storage_Unit);
      TZI.Bias := C.winnt.LONG (-Time_Zone);
      TZI.StandardBias := TZI.Bias;
      TZI.DaylightBias := TZI.Bias;
   end Make_TZI;

   function Is_Leap_Second (T : Duration) return Boolean;
   function Is_Leap_Second (T : Duration) return Boolean is
      Aliased_T : aliased C.windef.FILETIME := To_Native_Time (T);
      SystemTime : aliased C.winbase.SYSTEMTIME;
      Success : C.windef.WINBOOL;
   begin
      Success :=
         C.winbase.FileTimeToSystemTime (Aliased_T'Access, SystemTime'Access);
      return Success /= C.windef.FALSE
         and then Second_Number'Base (SystemTime.wSecond) = 60;
   end Is_Leap_Second;

   --  implementation

   function To_Native_Time (T : Duration) return Native_Time is
      U : constant C.winnt.ULARGE_INTEGER := (
         Unchecked_Tag => 255, -- any value in others
         QuadPart =>
            C.winnt.ULONGLONG (
               System.Native_Time.Nanosecond_Number'Integer_Value (T) / 100
                  + Diff));
   begin
      return (
         dwLowDateTime => U.LowPart,
         dwHighDateTime => U.HighPart);
   end To_Native_Time;

   function To_Time (T : Native_Time) return Duration is
      U : constant C.winnt.ULARGE_INTEGER := (
         Unchecked_Tag => 0,
         LowPart => T.dwLowDateTime,
         HighPart => T.dwHighDateTime);
   begin
      return Duration'Fixed_Value (
         (System.Native_Time.Nanosecond_Number (U.QuadPart) - Diff) * 100);
   end To_Time;

   function Clock return Native_Time is
      Result : aliased C.windef.FILETIME;
   begin
      C.winbase.GetSystemTimeAsFileTime (Result'Access);
      return Result;
   end Clock;

   procedure Split (
      Date : Time;
      Year : out Year_Number;
      Month : out Month_Number;
      Day : out Day_Number;
      Seconds : out Day_Duration;
      Leap_Second : out Boolean;
      Time_Zone : Time_Offset;
      Error : out Boolean)
   is
      Sub_Second : constant Second_Duration :=
         Duration'Fixed_Value (
            System.Native_Time.Nanosecond_Number'Integer_Value (Date)
               mod 1_000_000_000);
      FileTime : aliased C.windef.FILETIME :=
         To_Native_Time (Date - Sub_Second);
      SystemTime : aliased C.winbase.SYSTEMTIME;
      SystemTime2 : aliased C.winbase.SYSTEMTIME;
      Dest : C.winbase.PSYSTEMTIME;
   begin
      Error :=
         C.winbase.FileTimeToSystemTime (FileTime'Access, SystemTime'Access) =
         C.windef.FALSE;
      if not Error then
         --  Leap_Second is always calculated as GMT
         Leap_Second := Second_Number'Base (SystemTime.wSecond) >= 60;
         --  other units are calculated by Time_Zone
         if Time_Zone /= 0 then
            declare
               TZI : aliased C.winbase.TIME_ZONE_INFORMATION;
            begin
               Make_TZI (TZI, Time_Zone);
               Error :=
                  C.winbase.SystemTimeToTzSpecificLocalTime (
                     TZI'Access,
                     SystemTime'Access,
                     SystemTime2'Access) =
                  C.windef.FALSE;
               if not Error then
                  if Leap_Second then
                     pragma Assert (SystemTime2.wSecond = 60);
                     SystemTime2.wSecond := 59;
                  end if;
                  Seconds :=
                     Duration (
                        Integer (SystemTime2.wHour) * (60 * 60)
                        + Integer (SystemTime2.wMinute) * 60
                        + Integer (SystemTime2.wSecond));
               end if;
            end;
            Dest := SystemTime2'Unchecked_Access;
         else
            --  This assumes all leap seconds are at "23:59:60" on GMT.
            --  truncate to day
            SystemTime.wHour := 0;
            SystemTime.wMinute := 0;
            SystemTime.wSecond := 0;
            pragma Assert (SystemTime.wMilliseconds = 0);
            declare
               Truncated_Time : aliased C.windef.FILETIME;
            begin
               Error :=
                  C.winbase.SystemTimeToFileTime (
                     SystemTime'Access,
                     Truncated_Time'Access) =
                  C.windef.FALSE;
               if not Error then
                  if Leap_Second then
                     FileTime := FileTime - 1.0;
                  end if;
                  Seconds := FileTime - Truncated_Time;
               end if;
            end;
            Dest := SystemTime'Unchecked_Access;
         end if;
         if not Error then
            Year := Year_Number (Dest.wYear);
            Month := Month_Number (Dest.wMonth);
            Day := Day_Number (Dest.wDay);
            Seconds := Seconds + Sub_Second;
         end if;
      end if;
   end Split;

   procedure Split (
      Date : Time;
      Year : out Year_Number;
      Month : out Month_Number;
      Day : out Day_Number;
      Hour : out Hour_Number;
      Minute : out Minute_Number;
      Second : out Second_Number;
      Sub_Second : out Second_Duration;
      Leap_Second : out Boolean;
      Day_of_Week : out Day_Name;
      Time_Zone : Time_Offset;
      Error : out Boolean)
   is
      FileTime : aliased C.windef.FILETIME := To_Native_Time (Date);
      SystemTime : aliased C.winbase.SYSTEMTIME;
      SystemTime2 : aliased C.winbase.SYSTEMTIME;
      Dest : C.winbase.PSYSTEMTIME;
   begin
      Error :=
         C.winbase.FileTimeToSystemTime (FileTime'Access, SystemTime'Access) =
         C.windef.FALSE;
      if not Error then
         --  Second, Sub_Second and Leap_Second are always calculated as GMT
         if Second_Number'Base (SystemTime.wSecond) >= 60 then
            Second := 59;
            Leap_Second := True;
         else
            Second := Second_Number (SystemTime.wSecond);
            Leap_Second := False;
         end if;
         Sub_Second :=
            Duration'Fixed_Value (
               System.Native_Time.Nanosecond_Number'Integer_Value (Date)
                  mod 1_000_000_000);
         --  other units are calculated by Time_Zone
         if Time_Zone /= 0 then
            declare
               TZI : aliased C.winbase.TIME_ZONE_INFORMATION;
            begin
               Make_TZI (TZI, Time_Zone);
               Error :=
                  C.winbase.SystemTimeToTzSpecificLocalTime (
                     TZI'Access,
                     SystemTime'Access,
                     SystemTime2'Access) =
                  C.windef.FALSE;
            end;
            Dest := SystemTime2'Unchecked_Access;
         else
            Dest := SystemTime'Unchecked_Access;
         end if;
         if not Error then
            Year := Year_Number (Dest.wYear);
            Month := Month_Number (Dest.wMonth);
            Day := Day_Number (Dest.wDay);
            Hour := Hour_Number (Dest.wHour);
            Minute := Minute_Number (Dest.wMinute);
            Day_of_Week := (Integer (Dest.wDayOfWeek) + 6) rem 7;
               --  Day_Name starts from Monday
         end if;
      end if;
   end Split;

   procedure Time_Of (
      Year : Year_Number;
      Month : Month_Number;
      Day : Day_Number;
      Seconds : Day_Duration;
      Leap_Second : Boolean;
      Time_Zone : Time_Offset;
      Result : out Time;
      Error : out Boolean)
   is
      Actual_Seconds : Day_Duration;
      SystemTime : aliased C.winbase.SYSTEMTIME := (
         wYear => C.windef.WORD (Year),
         wMonth => C.windef.WORD (Month),
         wDayOfWeek => 16#ffff#,
         wDay => C.windef.WORD (Day),
         wHour => 0,
         wMinute => 0,
         wSecond => 0,
         wMilliseconds => 0);
      SystemTime2 : aliased C.winbase.SYSTEMTIME;
      Source : C.winbase.PSYSTEMTIME;
      FileTime : aliased C.windef.FILETIME;
   begin
      if Time_Zone /= 0 then
         Actual_Seconds :=
            Duration'Fixed_Value (
               System.Native_Time.Nanosecond_Number'Integer_Value (Seconds)
                  mod 1_000_000_000);
         declare
            S : constant Natural := Integer (Seconds - Actual_Seconds);
            TZI : aliased C.winbase.TIME_ZONE_INFORMATION;
         begin
            SystemTime.wHour := C.windef.WORD (S / (60 * 60));
            SystemTime.wMinute := C.windef.WORD (S rem (60 * 60) / 60);
            SystemTime.wSecond := C.windef.WORD (S rem 60);
            Make_TZI (TZI, Time_Zone);
            Error :=
               C.winbase.TzSpecificLocalTimeToSystemTime (
                  TZI'Access,
                  SystemTime'Access,
                  SystemTime2'Access) =
               C.windef.FALSE;
         end;
         Source := SystemTime2'Unchecked_Access;
      else
         --  This assumes all leap seconds are at "23:59:60" on GMT.
         Error := False;
         Actual_Seconds := Seconds;
         Source := SystemTime'Unchecked_Access;
      end if;
      if not Error then
         Error :=
            C.winbase.SystemTimeToFileTime (Source, FileTime'Access) =
            C.windef.FALSE;
         Result := To_Time (FileTime) + Actual_Seconds;
         if not Error then
            if Leap_Second then
               Result := Result + 1.0;
               --  checking
               Error := not Is_Leap_Second (Result);
            end if;
         end if;
      end if;
   end Time_Of;

   procedure Time_Of (
      Year : Year_Number;
      Month : Month_Number;
      Day : Day_Number;
      Hour : Hour_Number;
      Minute : Minute_Number;
      Second : Second_Number;
      Sub_Second : Second_Duration;
      Leap_Second : Boolean;
      Time_Zone : Time_Offset;
      Result : out Time;
      Error : out Boolean)
   is
      SystemTime : aliased C.winbase.SYSTEMTIME := (
         wYear => C.windef.WORD (Year),
         wMonth => C.windef.WORD (Month),
         wDayOfWeek => 16#ffff#,
         wDay => C.windef.WORD (Day),
         wHour => C.windef.WORD (Hour),
         wMinute => C.windef.WORD (Minute),
         wSecond => C.windef.WORD (Second),
         wMilliseconds => 0);
      SystemTime2 : aliased C.winbase.SYSTEMTIME;
      Source : C.winbase.PSYSTEMTIME;
      FileTime : aliased C.windef.FILETIME;
   begin
      if Time_Zone /= 0 then
         declare
            TZI : aliased C.winbase.TIME_ZONE_INFORMATION;
         begin
            Make_TZI (TZI, Time_Zone);
            Error :=
               C.winbase.TzSpecificLocalTimeToSystemTime (
                  TZI'Access,
                  SystemTime'Access,
                  SystemTime2'Access) =
               C.windef.FALSE;
         end;
         Source := SystemTime2'Unchecked_Access;
      else
         Error := False;
         Source := SystemTime'Unchecked_Access;
      end if;
      if not Error then
         Error :=
            C.winbase.SystemTimeToFileTime (Source, FileTime'Access) =
            C.windef.FALSE;
         if not Error then
            Result := To_Time (FileTime) + Sub_Second;
            if Leap_Second then
               Result := Result + 1.0;
               --  checking
               Error := not Is_Leap_Second (Result);
            end if;
         end if;
      end if;
   end Time_Of;

   procedure UTC_Time_Offset (
      Date : Time;
      Time_Zone : out Time_Offset;
      Error : out Boolean)
   is
      --  Raymond Chen explains:
      --  SystemTimeToTzSpecificLocalTime uses the time zone in effect at the
      --    time being converted, whereas the FileTimeToLocalFileTime function
      --    uses the time zone in effect right now.
      File_Time : aliased constant C.windef.FILETIME :=
         To_Native_Time (Duration (Date));
      System_Time : aliased C.winbase.SYSTEMTIME;
      Local_System_Time : aliased C.winbase.SYSTEMTIME;
      Local_File_Time : aliased C.windef.FILETIME;
      --  Use Backed_File_Time instead of Date (or File_Time) because the
      --    unit of FILETIME is 100 nano-seconds but the unit of SYSTEMTIME
      --    is one milli-second.
      Backed_File_Time : aliased C.windef.FILETIME;
   begin
      Error := not (
         C.winbase.FileTimeToSystemTime (
               File_Time'Access,
               System_Time'Access) /=
            C.windef.FALSE
         and then C.winbase.SystemTimeToTzSpecificLocalTime (
               null,
               System_Time'Access,
               Local_System_Time'Access) /=
            C.windef.FALSE
         and then C.winbase.SystemTimeToFileTime (
               Local_System_Time'Access,
               Local_File_Time'Access) /=
            C.windef.FALSE
         and then C.winbase.SystemTimeToFileTime (
               System_Time'Access,
               Backed_File_Time'Access) /=
            C.windef.FALSE);
      if not Error then
         declare
            Offset : constant System.Native_Time.Nanosecond_Number :=
               System.Native_Time.Nanosecond_Number'Integer_Value (
                  System.Native_Time.To_Duration (Local_File_Time)
                     - System.Native_Time.To_Duration (Backed_File_Time));
         begin
            Time_Zone := Time_Offset (Offset / 60_000_000_000);
         end;
      end if;
   end UTC_Time_Offset;

   procedure Delay_Until (T : Native_Time) is
      Timeout_T : constant Duration := System.Native_Time.To_Duration (T);
      Current_T : constant Duration := System.Native_Time.To_Duration (Clock);
      D : Duration;
   begin
      if Timeout_T > Current_T then
         D := Timeout_T - Current_T;
      else
         D := 0.0; -- always calling Delay_For for abort checking
      end if;
      System.Native_Time.Delay_For (D);
   end Delay_Until;

end System.Native_Calendar;
