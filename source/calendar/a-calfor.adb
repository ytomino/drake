pragma Check_Policy (Validate, Off);
with Ada.Calendar.Inside;
with System.Formatting;
with System.Native_Time;
package body Ada.Calendar.Formatting is
--  pragma Suppress (All_Checks);
   use type Time_Zones.Time_Offset;
   use type System.Formatting.Unsigned;
   use type System.Native_Time.Nanosecond_Number;

   --  for Year, Month, Day

   type Packed_Split_Time is mod 2 ** 64;
--  for Packed_Split_Time use record
--    Day at 0 range 0 .. 7; -- 2 ** 5 = 32 > 31
--    Month at 0 range 8 .. 15; -- 2 ** 4 = 16 > 12
--    Year at 0 range 16 .. 31; -- 2 ** 9 = 512 > 2399 - 1901 + 1 = 499
--    Day_of_Week at 0 range 32 .. 38; -- 2 ** 3 = 8 > 7
--    Leap_Second at 0 range 39 .. 39;
--    Second at 0 range 40 .. 47; -- 2 ** 6 = 64 > 60
--    Minute at 0 range 48 .. 55; -- 2 ** 6 = 64 > 60
--    Hour at 0 range 56 .. 63; -- 2 ** 5 = 32 > 24
--  end record;

   function Shift_Left (Value : Packed_Split_Time; Amount : Natural)
      return Packed_Split_Time;
   pragma Import (Intrinsic, Shift_Left);
   function Shift_Right (Value : Packed_Split_Time; Amount : Natural)
      return Packed_Split_Time;
   pragma Import (Intrinsic, Shift_Right);

   function Packed_Split (
      Date : Time;
      Time_Zone : Time_Zones.Time_Offset)
      return Packed_Split_Time;
   pragma Pure_Function (Packed_Split);
   pragma Machine_Attribute (Packed_Split, "pure");
   --  The callings of this function will be unified since pure attribute
   --    when Year, Month and Day are inlined

   function Packed_Split (
      Date : Time;
      Time_Zone : Time_Zones.Time_Offset)
      return Packed_Split_Time
   is
      Year : Year_Number;
      Month : Month_Number;
      Day : Day_Number;
      Hour : Hour_Number;
      Minute : Minute_Number;
      Second : Second_Number;
      Sub_Second : Second_Duration;
      Leap_Second : Boolean;
      Day_of_Week : Inside.Day_Name;
   begin
      Inside.Split (
         Date,
         Year => Year,
         Month => Month,
         Day => Day,
         Hour => Hour,
         Minute => Minute,
         Second => Second,
         Sub_Second => Sub_Second,
         Leap_Second => Leap_Second,
         Day_of_Week => Day_of_Week,
         Time_Zone => Inside.Time_Offset (Time_Zone));
      return Packed_Split_Time (Day)
         or Shift_Left (Packed_Split_Time (Month), 8)
         or Shift_Left (Packed_Split_Time (Year), 16)
         or Shift_Left (Packed_Split_Time (Day_of_Week), 32)
         or Shift_Left (Packed_Split_Time (Boolean'Pos (Leap_Second)), 39)
         or Shift_Left (Packed_Split_Time (Second), 40)
         or Shift_Left (Packed_Split_Time (Minute), 48)
         or Shift_Left (Packed_Split_Time (Hour), 56);
   end Packed_Split;

   --  99 hours

   procedure Split_Base (
      Seconds : Duration; -- Seconds >= 0.0
      Hour : out Natural;
      Minute : out Minute_Number;
      Second : out Second_Number;
      Sub_Second : out Second_Duration);
   procedure Split_Base (
      Seconds : Duration;
      Hour : out Natural;
      Minute : out Minute_Number;
      Second : out Second_Number;
      Sub_Second : out Second_Duration)
   is
      X : System.Native_Time.Nanosecond_Number
         := System.Native_Time.Nanosecond_Number'Integer_Value (Seconds);
      Sub : System.Native_Time.Nanosecond_Number;
   begin
      Sub := X rem 1000000000;
      Sub_Second := Duration'Fixed_Value (Sub);
      X := (X - Sub) / 1000000000; -- unit is 1-second
      Sub := X rem 60;
      Second := Second_Number (Sub);
      X := (X - Sub) / 60; -- unit is 1-minute
      Sub := X rem 60;
      Minute := Minute_Number (Sub);
      X := (X - Sub) / 60; -- unit is 1-hour
      Hour := Integer (X);
   end Split_Base;

   procedure Image (
      Hour : Natural;
      Minute : Minute_Number;
      Second : Second_Number;
      Sub_Second : Second_Duration;
      Include_Time_Fraction : Boolean;
      Item : out String;
      Last : out Natural);
   procedure Image (
      Hour : Natural;
      Minute : Minute_Number;
      Second : Second_Number;
      Sub_Second : Second_Duration;
      Include_Time_Fraction : Boolean;
      Item : out String;
      Last : out Natural)
   is
      Error : Boolean;
   begin
      System.Formatting.Image (
         System.Formatting.Unsigned (Hour),
         Item,
         Last,
         Width => 2,
         Error => Error);
      pragma Check (Validate, not Error);
      Last := Last + 1;
      Item (Last) := ':';
      System.Formatting.Image (
         System.Formatting.Unsigned (Minute),
         Item (Last + 1 .. Item'Last),
         Last,
         Width => 2,
         Error => Error);
      pragma Check (Validate, not Error);
      Last := Last + 1;
      Item (Last) := ':';
      System.Formatting.Image (
         System.Formatting.Unsigned (Second),
         Item (Last + 1 .. Item'Last),
         Last,
         Width => 2,
         Error => Error);
      pragma Check (Validate, not Error);
      if Include_Time_Fraction then
         Last := Last + 1;
         Item (Last) := '.';
         System.Formatting.Image (
            System.Formatting.Unsigned (Sub_Second * 100.0),
            Item (Last + 1 .. Item'Last),
            Last,
            Width => 2,
            Error => Error);
            pragma Check (Validate, not Error);
      end if;
   end Image;

   --  implementation

   function Day_Of_Week (
      Date : Time;
      Time_Zone : Time_Zones.Time_Offset := 0)
      return Day_Name is
   begin
      return Day_Name'Val (
         Shift_Right (Packed_Split (Date, Time_Zone), 32) and 16#7f#);
   end Day_Of_Week;

   function Year (Date : Time; Time_Zone : Time_Zones.Time_Offset := 0)
      return Year_Number is
   begin
      return Year_Number (
         Shift_Right (Packed_Split (Date, Time_Zone), 16) and 16#ffff#);
   end Year;

   function Month (Date : Time; Time_Zone : Time_Zones.Time_Offset := 0)
      return Month_Number is
   begin
      return Month_Number (
         Shift_Right (Packed_Split (Date, Time_Zone), 8) and 16#ff#);
   end Month;

   function Day (Date : Time; Time_Zone : Time_Zones.Time_Offset := 0)
      return Day_Number is
   begin
      return Day_Number (
         Packed_Split (Date, Time_Zone) and 16#ff#);
   end Day;

   function Hour (Date : Time; Time_Zone : Time_Zones.Time_Offset := 0)
      return Hour_Number is
   begin
      return Hour_Number (
         Shift_Right (Packed_Split (Date, Time_Zone), 56));
   end Hour;

   function Minute (Date : Time; Time_Zone : Time_Zones.Time_Offset := 0)
      return Minute_Number is
   begin
      return Minute_Number (
         Shift_Right (Packed_Split (Date, Time_Zone), 48) and 16#ff#);
   end Minute;

   function Second (Date : Time) return Second_Number is
      Time_Zone : constant Time_Zones.Time_Offset := 0;
      --  unit of Time_Zone is minute
   begin
      return Minute_Number (
         Shift_Right (Packed_Split (Date, Time_Zone), 40) and 16#ff#);
   end Second;

   function Sub_Second (Date : Time) return Second_Duration is
      Time_Zone : constant Time_Zones.Time_Offset := 0;
      --  unit of Time_Zone is minute
   begin
      return Duration'Fixed_Value (
         (System.Native_Time.Nanosecond_Number'Integer_Value (Date)
            + System.Native_Time.Nanosecond_Number (Time_Zone)
               * (60 * 1000000000))
         mod 1000000000);
   end Sub_Second;

   function Seconds (Date : Time; Time_Zone : Time_Zones.Time_Offset := 0)
      return Day_Duration is
   begin
      return Duration'Fixed_Value (
         (System.Native_Time.Nanosecond_Number'Integer_Value (Date)
            + System.Native_Time.Nanosecond_Number (Time_Zone)
               * (60 * 1000000000))
         mod (24 * 60 * 60 * 1000000000));
   end Seconds;

   function Seconds_Of (
      Hour : Hour_Number;
      Minute : Minute_Number;
      Second : Second_Number := 0;
      Sub_Second : Second_Duration := 0.0)
      return Day_Duration is
   begin
      return Duration ((Hour * 60 + Minute) * 60 + Second) + Sub_Second;
   end Seconds_Of;

   procedure Split (
      Seconds : Day_Duration;
      Hour : out Hour_Number;
      Minute : out Minute_Number;
      Second : out Second_Number;
      Sub_Second : out Second_Duration) is
   begin
      Split_Base (
         Seconds,
         Hour => Hour,
         Minute => Minute,
         Second => Second,
         Sub_Second => Sub_Second);
   end Split;

   function Time_Of (
      Year : Year_Number;
      Month : Month_Number;
      Day : Day_Number;
      Hour : Hour_Number;
      Minute : Minute_Number;
      Second : Second_Number;
      Sub_Second : Second_Duration := 0.0;
      Leap_Second : Boolean := False;
      Time_Zone : Time_Zones.Time_Offset := 0)
      return Time is
   begin
      return Time_Of (
         Year => Year,
         Month => Month,
         Day => Day,
         Seconds => Seconds_Of (Hour, Minute, Second, Sub_Second),
         Leap_Second => Leap_Second,
         Time_Zone => Time_Zone);
   end Time_Of;

   function Time_Of (
      Year : Year_Number;
      Month : Month_Number;
      Day : Day_Number;
      Seconds : Day_Duration := 0.0;
      Leap_Second : Boolean := False;
      Time_Zone : Time_Zones.Time_Offset := 0)
      return Time is
   begin
      return Inside.Time_Of (
         Year => Year,
         Month => Month,
         Day => Day,
         Seconds => Seconds,
         Leap_Second => Leap_Second,
         Time_Zone => Inside.Time_Offset (Time_Zone));
   end Time_Of;

   procedure Split (
      Date : Time;
      Year : out Year_Number;
      Month : out Month_Number;
      Day : out Day_Number;
      Hour : out Hour_Number;
      Minute : out Minute_Number;
      Second : out Second_Number;
      Sub_Second : out Second_Duration;
      Time_Zone : Time_Zones.Time_Offset := 0)
   is
      Leap_Second : Boolean;
   begin
      Split (
         Date,
         Year => Year,
         Month => Month,
         Day => Day,
         Hour => Hour,
         Minute => Minute,
         Second => Second,
         Sub_Second => Sub_Second,
         Leap_Second => Leap_Second,
         Time_Zone => Time_Zone);
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
      Time_Zone : Time_Zones.Time_Offset := 0)
   is
      Day_of_Week : Inside.Day_Name;
   begin
      Inside.Split (
         Date,
         Year => Year,
         Month => Month,
         Day => Day,
         Hour => Hour,
         Minute => Minute,
         Second => Second,
         Sub_Second => Sub_Second,
         Leap_Second => Leap_Second,
         Day_of_Week => Day_of_Week,
         Time_Zone => Inside.Time_Offset (Time_Zone));
   end Split;

   procedure Split (
      Date : Time;
      Year : out Year_Number;
      Month : out Month_Number;
      Day : out Day_Number;
      Seconds : out Day_Duration;
      Leap_Second : out Boolean;
      Time_Zone : Time_Zones.Time_Offset := 0)
   is
      Hour : Hour_Number;
      Minute : Minute_Number;
      Second : Second_Number;
      Sub_Second : Second_Duration;
   begin
      Split (
         Date,
         Year => Year,
         Month => Month,
         Day => Day,
         Hour => Hour,
         Minute => Minute,
         Second => Second,
         Sub_Second => Sub_Second,
         Leap_Second => Leap_Second,
         Time_Zone => Time_Zone);
      Seconds := Seconds_Of (Hour, Minute, Second, Sub_Second);
   end Split;

   function Image (
      Date : Time;
      Include_Time_Fraction : Boolean := False;
      Time_Zone : Time_Zones.Time_Offset := 0)
      return String
   is
      Year : Year_Number;
      Month : Month_Number;
      Day : Day_Number;
      Hour : Hour_Number;
      Minute : Minute_Number;
      Second : Second_Number;
      Sub_Second : Second_Duration;
      Leap_Second : Boolean;
      Result : String (1 .. 22 + Integer'Width); -- yyyy-mm-dd hh:mm:ss.ss
      Last : Natural;
      Error : Boolean;
   begin
      Split (
         Date,
         Year => Year,
         Month => Month,
         Day => Day,
         Hour => Hour,
         Minute => Minute,
         Second => Second,
         Sub_Second => Sub_Second,
         Leap_Second => Leap_Second,
         Time_Zone => Time_Zone);
      System.Formatting.Image (
         System.Formatting.Unsigned (Year),
         Result,
         Last,
         Width => 4,
         Error => Error);
      pragma Check (Validate, not Error);
      Last := Last + 1;
      Result (Last) := '-';
      System.Formatting.Image (
         System.Formatting.Unsigned (Month),
         Result (Last + 1 .. Result'Last),
         Last,
         Width => 2,
         Error => Error);
      pragma Check (Validate, not Error);
      Last := Last + 1;
      Result (Last) := '-';
      System.Formatting.Image (
         System.Formatting.Unsigned (Day),
         Result (Last + 1 .. Result'Last),
         Last,
         Width => 2,
         Error => Error);
      pragma Check (Validate, not Error);
      Last := Last + 1;
      Result (Last) := ' ';
      Image (
         Hour,
         Minute,
         Second,
         Sub_Second,
         Include_Time_Fraction,
         Result (Last + 1 .. Result'Last),
         Last);
      return Result (1 .. Last);
   end Image;

   function Value (
      Date : String;
      Time_Zone : Time_Zones.Time_Offset := 0)
      return Time
   is
      Last : Natural;
      Year : System.Formatting.Unsigned;
      Month : System.Formatting.Unsigned;
      Day : System.Formatting.Unsigned;
      Seconds : Duration;
      Error : Boolean;
   begin
      System.Formatting.Value (
         Date,
         Last,
         Year,
         Error => Error);
      if Error
         or else Year not in
            System.Formatting.Unsigned (Year_Number'First) ..
            System.Formatting.Unsigned (Year_Number'Last)
         or else Last >= Date'Last
         or else Date (Last + 1) /= '-'
      then
         raise Constraint_Error;
      end if;
      Last := Last + 1;
      System.Formatting.Value (
         Date (Last + 1 .. Date'Last),
         Last,
         Month,
         Error => Error);
      if Error
         or else Month not in
            System.Formatting.Unsigned (Month_Number'First) ..
            System.Formatting.Unsigned (Month_Number'Last)
         or else Last >= Date'Last
         or else Date (Last + 1) /= '-'
      then
         raise Constraint_Error;
      end if;
      Last := Last + 1;
      System.Formatting.Value (
         Date (Last + 1 .. Date'Last),
         Last,
         Day,
         Error => Error);
      if Error
         or else Day not in
            System.Formatting.Unsigned (Day_Number'First) ..
            System.Formatting.Unsigned (Day_Number'Last)
         or else Last >= Date'Last
         or else Date (Last + 1) /= ' '
      then
         raise Constraint_Error;
      end if;
      Last := Last + 1;
      Seconds := Value (Date (Last + 1 .. Date'Last));
      return Time_Of (
         Year => Year_Number (Year),
         Month => Month_Number (Month),
         Day => Day_Number (Day),
         Seconds => Seconds,
         Leap_Second => False,
         Time_Zone => Time_Zone);
   end Value;

   function Image (
      Elapsed_Time : Duration;
      Include_Time_Fraction : Boolean := False)
      return String
   is
      Abs_Elapsed_Time : Duration := Elapsed_Time;
      Hour : Natural;
      Minute : Minute_Number;
      Second : Second_Number;
      Sub_Second : Second_Duration;
      Result : String (1 .. 12 + Integer'Width); -- [-]hh:mm:ss.ss
      Last : Natural := 0;
   begin
      if Abs_Elapsed_Time < 0.0 then
         Result (1) := '-';
         Last := 1;
         Abs_Elapsed_Time := -Abs_Elapsed_Time;
      end if;
      Split_Base (
         Abs_Elapsed_Time,
         Hour => Hour,
         Minute => Minute,
         Second => Second,
         Sub_Second => Sub_Second);
      Image (
         Hour rem 100,
         Minute,
         Second,
         Sub_Second,
         Include_Time_Fraction,
         Result (Last + 1 .. Result'Last),
         Last);
      return Result (1 .. Last);
   end Image;

   function Value (Elapsed_Time : String) return Duration is
      Last : Natural := Elapsed_Time'First - 1;
      P : Natural;
      Minus : Boolean := False;
      Hour : System.Formatting.Unsigned;
      Minute : System.Formatting.Unsigned;
      Second : System.Formatting.Unsigned;
      Sub_Second_I : System.Formatting.Unsigned;
      Sub_Second : Second_Duration;
      Error : Boolean;
      Result : Duration;
   begin
      if Elapsed_Time'First <= Elapsed_Time'Last
         and then Elapsed_Time (Elapsed_Time'First) = '-'
      then
         Minus := True;
         Last := Elapsed_Time'First;
      end if;
      System.Formatting.Value (
         Elapsed_Time (Last + 1 .. Elapsed_Time'Last),
         Last,
         Hour,
         Error => Error);
      if Error
         or else Hour > System.Formatting.Unsigned (Hour_Number'Last)
         or else Last >= Elapsed_Time'Last
         or else Elapsed_Time (Last + 1) /= ':'
      then
         raise Constraint_Error;
      end if;
      Last := Last + 1;
      System.Formatting.Value (
         Elapsed_Time (Last + 1 .. Elapsed_Time'Last),
         Last,
         Minute,
         Error => Error);
      if Error
         or else Minute > System.Formatting.Unsigned (Minute_Number'Last)
         or else Last >= Elapsed_Time'Last
         or else Elapsed_Time (Last + 1) /= ':'
      then
         raise Constraint_Error;
      end if;
      Last := Last + 1;
      System.Formatting.Value (
         Elapsed_Time (Last + 1 .. Elapsed_Time'Last),
         Last,
         Second,
         Error => Error);
      if Error
         or else Second > System.Formatting.Unsigned (Second_Number'Last)
      then
         raise Constraint_Error;
      end if;
      if Last < Elapsed_Time'Last and then Elapsed_Time (Last + 1) = '.' then
         Last := Last + 1;
         P := Last + 1;
         System.Formatting.Value (
            Elapsed_Time (P .. Elapsed_Time'Last),
            Last,
            Sub_Second_I,
            Error => Error);
         if Error then
            raise Constraint_Error;
         end if;
         declare
            pragma Suppress (Range_Check); -- [gcc-4.6] suppress a buggy check
         begin
            Sub_Second := Duration (Sub_Second_I);
         end;
         for I in P .. Last loop
            declare
               pragma Suppress (Range_Check); -- [gcc-4.8] a buggy check
            begin
               Sub_Second := Sub_Second / 10;
            end;
         end loop;
      else
         Sub_Second := 0.0;
      end if;
      if Last /= Elapsed_Time'Last then
         raise Constraint_Error;
      end if;
      Result := Seconds_Of (
         Hour_Number (Hour),
         Minute_Number (Minute),
         Second_Number (Second),
         Sub_Second);
      if Minus then
         Result := -Result;
      end if;
      return Result;
   end Value;

   function Image (Time_Zone : Time_Zones.Time_Offset) return String is
      U_Time_Zone : constant Natural := Natural (abs Time_Zone);
      Hour : constant Hour_Number := U_Time_Zone / 60;
      Minute : constant Minute_Number := U_Time_Zone mod 60;
      Last : Natural;
      Error : Boolean;
   begin
      return Result : String (1 .. 6) do
         if Time_Zone < 0 then
            Result (1) := '-';
         else
            Result (1) := '+';
         end if;
         System.Formatting.Image (
            System.Formatting.Unsigned (Hour),
            Result (2 .. 3),
            Last,
            Width => 2,
            Error => Error);
         pragma Check (Validate, not Error and then Last = 3);
         Result (4) := ':';
         System.Formatting.Image (
            System.Formatting.Unsigned (Minute),
            Result (5 .. 6),
            Last,
            Width => 2,
            Error => Error);
         pragma Check (Validate, not Error and then Last = 6);
      end return;
   end Image;

   function Value (Time_Zone : String) return Time_Zones.Time_Offset is
      Minus : Boolean;
      Hour : System.Formatting.Unsigned;
      Minute : System.Formatting.Unsigned;
      Last : Natural;
      Error : Boolean;
      Result : Time_Zones.Time_Offset;
   begin
      Last := Time_Zone'First - 1;
      if Last < Time_Zone'Last and then Time_Zone (Last + 1) = '-' then
         Minus := True;
         Last := Last + 1;
      else
         Minus := False;
         if Last < Time_Zone'Last and then Time_Zone (Last + 1) = '+' then
            Last := Last + 1;
         end if;
      end if;
      System.Formatting.Value (
         Time_Zone (Last + 1 .. Time_Zone'Last),
         Last,
         Hour,
         Error => Error);
      if Error
         or else Hour > System.Formatting.Unsigned (Hour_Number'Last)
         or else Last >= Time_Zone'Last
         or else Time_Zone (Last + 1) /= ':'
      then
         raise Constraint_Error;
      end if;
      Last := Last + 1;
      System.Formatting.Value (
         Time_Zone (Last + 1 .. Time_Zone'Last),
         Last,
         Minute,
         Error => Error);
      if Error
         or else Minute > System.Formatting.Unsigned (Minute_Number'Last)
         or else Last /= Time_Zone'Last
      then
         raise Constraint_Error;
      end if;
      Result := Time_Zones.Time_Offset'Base (Hour) * 60
         + Time_Zones.Time_Offset'Base (Minute);
      if Minus then
         Result := -Result;
      end if;
      return Result;
   end Value;

end Ada.Calendar.Formatting;
