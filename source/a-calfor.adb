with Ada.Calendar.Inside;
with System.Formatting;
package body Ada.Calendar.Formatting is
   pragma Suppress (All_Checks);
   use type Time_Zones.Time_Offset;

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
      pragma Assert (not Error);
      Last := Last + 1;
      Item (Last) := ':';
      System.Formatting.Image (
         System.Formatting.Unsigned (Minute),
         Item (Last + 1 .. Item'Last),
         Last,
         Width => 2,
         Error => Error);
      pragma Assert (not Error);
      Last := Last + 1;
      Item (Last) := ':';
      System.Formatting.Image (
         System.Formatting.Unsigned (Second),
         Item (Last + 1 .. Item'Last),
         Last,
         Width => 2,
         Error => Error);
      pragma Assert (not Error);
      if Include_Time_Fraction then
         Last := Last + 1;
         Item (Last) := '.';
         System.Formatting.Image (
            System.Formatting.Unsigned (Sub_Second * 100.0),
            Item (Last + 1 .. Item'Last),
            Last,
            Width => 2,
            Error => Error);
            pragma Assert (not Error);
      end if;
   end Image;

   --  implementation

   function Day_Of_Week (
      Date : Time;
      Time_Zone : Time_Zones.Time_Offset := 0)
      return Day_Name
   is
      Year : Year_Number;
      Month : Month_Number;
      Day : Day_Number;
      Hour : Hour_Number;
      Minute : Minute_Number;
      Second : Second_Number;
      Sub_Second : Second_Duration;
      Leap_Second : Boolean;
      Result : Inside.Day_Name;
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
         Day_of_Week => Result,
         Time_Zone => Inside.Time_Offset (Time_Zone));
      return Day_Name'Val (Result);
   end Day_Of_Week;

   function Year (Date : Time; Time_Zone : Time_Zones.Time_Offset := 0)
      return Year_Number
   is
      Year : Year_Number;
      Month : Month_Number;
      Day : Day_Number;
      Hour : Hour_Number;
      Minute : Minute_Number;
      Second : Second_Number;
      Sub_Second : Second_Duration;
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
      return Year;
   end Year;

   function Month (Date : Time; Time_Zone : Time_Zones.Time_Offset := 0)
      return Month_Number
   is
      Year : Year_Number;
      Month : Month_Number;
      Day : Day_Number;
      Hour : Hour_Number;
      Minute : Minute_Number;
      Second : Second_Number;
      Sub_Second : Second_Duration;
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
      return Month;
   end Month;

   function Day (Date : Time; Time_Zone : Time_Zones.Time_Offset := 0)
      return Day_Number
   is
      Year : Year_Number;
      Month : Month_Number;
      Day : Day_Number;
      Hour : Hour_Number;
      Minute : Minute_Number;
      Second : Second_Number;
      Sub_Second : Second_Duration;
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
      return Day;
   end Day;

   function Hour (Date : Time; Time_Zone : Time_Zones.Time_Offset := 0)
      return Hour_Number
   is
      Hour : Hour_Number;
      Minute : Minute_Number;
      Second : Second_Number;
      Sub_Second : Second_Duration;
   begin
      Split (
         Seconds (Date, Time_Zone),
         Hour,
         Minute,
         Second,
         Sub_Second);
      return Hour;
   end Hour;

   function Minute (Date : Time; Time_Zone : Time_Zones.Time_Offset := 0)
      return Minute_Number
   is
      Hour : Hour_Number;
      Minute : Minute_Number;
      Second : Second_Number;
      Sub_Second : Second_Duration;
   begin
      Split (
         Seconds (Date, Time_Zone),
         Hour,
         Minute,
         Second,
         Sub_Second);
      return Minute;
   end Minute;

   function Second (Date : Time) return Second_Number is
      Hour : Hour_Number;
      Minute : Minute_Number;
      Second : Second_Number;
      Sub_Second : Second_Duration;
   begin
      Split (
         Seconds (Date, Time_Zone => 0), --  unit of Time_Zone is minute
         Hour,
         Minute,
         Second,
         Sub_Second);
      return Second;
   end Second;

   function Sub_Second (Date : Time) return Second_Duration is
      Hour : Hour_Number;
      Minute : Minute_Number;
      Second : Second_Number;
      Sub_Second : Second_Duration;
   begin
      Split (
         Seconds (Date, Time_Zone => 0), --  unit of Time_Zone is minute
         Hour,
         Minute,
         Second,
         Sub_Second);
      return Sub_Second;
   end Sub_Second;

   function Seconds (Date : Time; Time_Zone : Time_Zones.Time_Offset := 0)
      return Day_Duration is
   begin
      return Inside.Seconds (Date, Inside.Time_Offset (Time_Zone));
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
      Inside.Split (
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
      Result : String (1 .. 22 + Integer'Width); --  yyyy-mm-dd hh:mm:ss.ss
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
      pragma Assert (not Error);
      Last := Last + 1;
      Result (Last) := '-';
      System.Formatting.Image (
         System.Formatting.Unsigned (Month),
         Result (Last + 1 .. Result'Last),
         Last,
         Width => 2,
         Error => Error);
      pragma Assert (not Error);
      Last := Last + 1;
      Result (Last) := '-';
      System.Formatting.Image (
         System.Formatting.Unsigned (Day),
         Result (Last + 1 .. Result'Last),
         Last,
         Width => 2,
         Error => Error);
      pragma Assert (not Error);
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
      if Error or else Last >= Date'Last or else Date (Last + 1) /= '-' then
         raise Constraint_Error;
      end if;
      Last := Last + 1;
      System.Formatting.Value (
         Date (Last + 1 .. Date'Last),
         Last,
         Month,
         Error => Error);
      if Error or else Last >= Date'Last or else Date (Last + 1) /= '-' then
         raise Constraint_Error;
      end if;
      Last := Last + 1;
      System.Formatting.Value (
         Date (Last + 1 .. Date'Last),
         Last,
         Day,
         Error => Error);
      if Error or else Last >= Date'Last or else Date (Last + 1) /= ' ' then
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
      Hour : Natural;
      Minute : Minute_Number;
      Second : Second_Number;
      Sub_Second : Second_Duration;
      Result : String (1 .. 11 + Integer'Width); --  hh:mm:ss.ss
      Last : Natural;
   begin
      Inside.Split (
         Elapsed_Time,
         Hour => Hour,
         Minute => Minute,
         Second => Second,
         Sub_Second => Sub_Second);
      Image (
         Hour,
         Minute,
         Second,
         Sub_Second,
         Include_Time_Fraction,
         Result,
         Last);
      return Result (1 .. Last);
   end Image;

   function Value (Elapsed_Time : String) return Duration is
      Last : Natural;
      P : Natural;
      Hour : System.Formatting.Unsigned;
      Minute : System.Formatting.Unsigned;
      Second : System.Formatting.Unsigned;
      Sub_Second_I : System.Formatting.Unsigned;
      Sub_Second : Second_Duration;
      Error : Boolean;
   begin
      System.Formatting.Value (
         Elapsed_Time,
         Last,
         Hour,
         Error => Error);
      if Error
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
      if Error then
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
         Sub_Second := Duration (Sub_Second_I);
         for I in P .. Last loop
            Sub_Second := Sub_Second / 10;
         end loop;
      else
         Sub_Second := 0.0;
      end if;
      if Last /= Elapsed_Time'Last then
         raise Constraint_Error;
      end if;
      return Seconds_Of (
         Hour_Number (Hour),
         Minute_Number (Minute),
         Second_Number (Second),
         Sub_Second);
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
         pragma Assert (not Error and then Last = 3);
         Result (4) := ':';
         System.Formatting.Image (
            System.Formatting.Unsigned (Minute),
            Result (5 .. 6),
            Last,
            Width => 2,
            Error => Error);
         pragma Assert (not Error and then Last = 6);
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
      if Error or else Last /= Time_Zone'Last then
         raise Constraint_Error;
      end if;
      Result := Time_Zones.Time_Offset'Base (Hour) * 60 +
         Time_Zones.Time_Offset'Base (Minute);
      if Minus then
         Result := -Result;
      end if;
      return Result;
   end Value;

end Ada.Calendar.Formatting;
