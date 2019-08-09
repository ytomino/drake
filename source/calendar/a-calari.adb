with Ada.Calendar.Formatting;
with System.Long_Long_Integer_Types;
package body Ada.Calendar.Arithmetic is
   use type System.Long_Long_Integer_Types.Long_Long_Unsigned;

   subtype Long_Long_Unsigned is
      System.Long_Long_Integer_Types.Long_Long_Unsigned;

   procedure Addition (Left : Time; Right : Day_Count; Result : out Time);
   procedure Addition (Left : Time; Right : Day_Count; Result : out Time) is
      Left_Seconds : Day_Duration;
      Left_Leap_Second : Boolean;
   begin
      Result := Left + Duration'(Duration (Right) * (24 * 60 * 60.0));
      declare -- split Left
         Year : Year_Number;
         Month : Month_Number;
         Day : Day_Number;
      begin
         Formatting.Split (Left,
            Year => Year,
            Month => Month,
            Day => Day,
            Seconds => Left_Seconds,
            Leap_Second => Left_Leap_Second,
            Time_Zone => 0);
      end;
      declare -- split Result
         Year : Year_Number;
         Month : Month_Number;
         Day : Day_Number;
         Seconds : Duration; -- may include a leap second
         Leap_Second : Boolean;
      begin
         Formatting.Split (Result,
            Year => Year,
            Month => Month,
            Day => Day,
            Seconds => Seconds,
            Leap_Second => Leap_Second,
            Time_Zone => 0);
         if Leap_Second then
            Seconds := Seconds + 1.0;
         end if;
         if Left_Seconds - Seconds >= 12 * 60 * 60.0 then
            Result := Result - Seconds - Duration'(1.0);
            Formatting.Split (Result,
               Year => Year,
               Month => Month,
               Day => Day,
               Seconds => Seconds,
               Leap_Second => Leap_Second,
               Time_Zone => 0);
            pragma Assert (Seconds = Duration'(24 * 60 * 60.0) - 1.0);
            if Leap_Second then
               Seconds := Seconds + 1.0;
            end if;
         elsif Seconds - Left_Seconds >= 12 * 60 * 60.0 then
            Result := Result + (Duration'(24 * 60 * 60.0 + 1.0) - Seconds);
            Formatting.Split (Result,
               Year => Year,
               Month => Month,
               Day => Day,
               Seconds => Seconds,
               Leap_Second => Leap_Second,
               Time_Zone => 0);
            pragma Assert (Seconds = 0.0);
            pragma Assert (not Leap_Second);
         end if;
         Result := Result + (Left_Seconds - Seconds);
      end;
      if Left_Leap_Second then
         declare -- set if a leap second is existing in the result day
            New_Result : constant Time := Result + Duration'(1.0);
            Year : Year_Number;
            Month : Month_Number;
            Day : Day_Number;
            Seconds : Day_Duration;
            Leap_Second : Boolean;
         begin
            Formatting.Split (New_Result,
               Year => Year,
               Month => Month,
               Day => Day,
               Seconds => Seconds,
               Leap_Second => Leap_Second,
               Time_Zone => 0);
            if Leap_Second then
               pragma Assert (Seconds = Left_Seconds);
               Result := New_Result;
            end if;
         end;
      end if;
   end Addition;

   --  implementation

   procedure Difference (
      Left, Right : Time;
      Days : out Day_Count;
      Seconds : out Duration;
      Leap_Seconds : out Leap_Seconds_Count)
   is
      Minus : Boolean;
      L, H : Time;
      L_Seconds : Duration; -- may include a leap second
      H_Seconds : Duration; -- may over 24H
      Truncated_L, Truncated_H : Time;
   begin
      if Left < Right then
         Minus := True;
         L := Left;
         H := Right;
      else
         Minus := False;
         L := Right;
         H := Left;
      end if;
      declare -- split L
         Year : Year_Number;
         Month : Month_Number;
         Day : Day_Number;
         Leap_Second : Boolean;
      begin
         Formatting.Split (L,
            Year => Year,
            Month => Month,
            Day => Day,
            Seconds => L_Seconds,
            Leap_Second => Leap_Second,
            Time_Zone => 0);
         Truncated_L :=
            Formatting.Time_Of (
               Year => Year,
               Month => Month,
               Day => Day,
               Seconds => 0.0,
               Leap_Second => False,
               Time_Zone => 0);
         if Leap_Second then
            L_Seconds := L_Seconds + 1.0;
         end if;
      end;
      declare -- split H
         Year : Year_Number;
         Month : Month_Number;
         Day : Day_Number;
         Seconds : Day_Duration;
         Leap_Second : Boolean;
         T : Time;
      begin
         Formatting.Split (H,
            Year => Year,
            Month => Month,
            Day => Day,
            Seconds => H_Seconds,
            Leap_Second => Leap_Second,
            Time_Zone => 0);
         Truncated_H :=
            Formatting.Time_Of (
               Year => Year,
               Month => Month,
               Day => Day,
               Seconds => 0.0,
               Leap_Second => False,
               Time_Zone => 0);
         if Leap_Second then
            H_Seconds := H_Seconds + 1.0;
         end if;
         if H_Seconds < L_Seconds then
            T := Truncated_H;
            Formatting.Split (T - Duration'(1.0),
               Year => Year,
               Month => Month,
               Day => Day,
               Seconds => Seconds,
               Leap_Second => Leap_Second, -- ignored
               Time_Zone => 0);
            Truncated_H :=
               Formatting.Time_Of (
                  Year => Year,
                  Month => Month,
                  Day => Day,
                  Seconds => 0.0,
                  Leap_Second => False,
                  Time_Zone => 0);
            H_Seconds := H_Seconds + (T - Truncated_H);
         end if;
      end;
      declare
         Truncated_D : constant Duration := Truncated_H - Truncated_L;
         Quotient, Remainder : Long_Long_Unsigned;
      begin
         System.Long_Long_Integer_Types.Divide (
            Long_Long_Unsigned'Integer_Value (Truncated_D),
            Long_Long_Unsigned'Integer_Value (Duration'(24 * 60 * 60.0)),
            Quotient => Quotient,
            Remainder => Remainder);
         Days := Day_Count (Quotient);
         Seconds := H_Seconds - L_Seconds;
         Leap_Seconds := Integer (Remainder / 1_000_000_000);
         pragma Assert (Remainder rem 1_000_000_000 = 0);
      end;
      if Minus then
         Days := -Days;
         Seconds := -Seconds;
         Leap_Seconds := -Leap_Seconds;
      end if;
   end Difference;

   function "+" (Left : Time; Right : Day_Count) return Time is
      Result : Time;
   begin
      Addition (Left, Right, Result => Result);
      return Result;
   end "+";

   function "+" (Left : Day_Count; Right : Time) return Time is
   begin
      return Right + Left;
   end "+";

   function "-" (Left : Time; Right : Day_Count) return Time is
   begin
      return Left + (-Right);
   end "-";

   function "-" (Left, Right : Time) return Day_Count is
      Days : Day_Count;
      Seconds : Duration;
      Leap_Seconds : Leap_Seconds_Count;
   begin
      Difference (Left, Right,
         Days => Days, Seconds => Seconds, Leap_Seconds => Leap_Seconds);
      return Days;
   end "-";

end Ada.Calendar.Arithmetic;
