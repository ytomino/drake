package body System.Val_Uns is
   pragma Suppress (All_Checks);
   use type Unsigned_Types.Unsigned;

   function Value_Unsigned (Str : String) return Unsigned_Types.Unsigned is
      Index : Positive := Str'First;
      Result : Unsigned_Types.Unsigned;
   begin
      Skip_Spaces (Str, Index);
      if Index < Str'Last and then Str (Index) = '+' then
         Index := Index + 1;
      end if;
      Get_Unsigned_Literal_Without_Sign (Str, Index, Result);
      Check_Last (Str, Index);
      return Result;
   end Value_Unsigned;

   procedure Skip_Spaces (S : String; Index : in out Positive) is
   begin
      while Index <= S'Last and then S (Index) = ' ' loop
         Index := Index + 1;
      end loop;
   end Skip_Spaces;

   procedure Check_Last (S : String; Index : Positive) is
      I : Positive := Index;
   begin
      Skip_Spaces (S, I);
      if I <= S'Last then
         raise Constraint_Error;
      end if;
   end Check_Last;

   procedure Get_Unsigned (
      S : String;
      Index : in out Positive;
      Result : out Formatting.Unsigned;
      Base : Formatting.Base_Type)
   is
      Last : Natural;
      Error : Boolean;
   begin
      Formatting.Value (
         S (Index .. S'Last),
         Last,
         Result,
         Base => Base,
         Skip_Underscore => True,
         Error => Error);
      if Error then
         raise Constraint_Error;
      end if;
      Index := Last + 1;
   end Get_Unsigned;

   procedure Get_Exponent (
      S : String;
      Index : in out Positive;
      Result : out Integer;
      Positive_Only : Boolean) is
   begin
      if Index <= S'Last
         and then (S (Index) = 'E' or else S (Index) = 'e')
      then
         Index := Index + 1;
         if Index <= S'Last and then S (Index) = '-' then
            if Positive_Only then
               raise Constraint_Error;
            end if;
            Index := Index + 1;
            Get_Unsigned (S, Index, Formatting.Unsigned (Result), Base => 10);
            Result := -Result;
         else
            if Index <= S'Last and then S (Index) = '+' then
               Index := Index + 1;
            end if;
            Get_Unsigned (S, Index, Formatting.Unsigned (Result), Base => 10);
         end if;
      else
         Result := 0;
      end if;
   end Get_Exponent;

   procedure Get_Unsigned_Literal_Without_Sign (
      S : String;
      Index : in out Positive;
      Result : out Formatting.Unsigned)
   is
      Base : Formatting.Base_Type := 10;
      Exponent : Integer;
   begin
      Get_Unsigned (S, Index, Result, Base => Base);
      if Index <= S'Last and then S (Index) = '#' then
         Index := Index + 1;
         if Result not in Formatting.Base_Type then
            raise Constraint_Error;
         end if;
         Base := Result;
         Get_Unsigned (S, Index, Result, Base => Base);
         if Index > S'Last or else S (Index) /= '#' then
            raise Constraint_Error;
         end if;
         Index := Index + 1;
      end if;
      Get_Exponent (S, Index, Exponent, Positive_Only => True);
      if Exponent /= 0 then
         Result := Result * Base ** Exponent;
      end if;
   end Get_Unsigned_Literal_Without_Sign;

end System.Val_Uns;
