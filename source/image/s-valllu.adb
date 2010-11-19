with System.Val_Uns;
package body System.Val_LLU is
   pragma Suppress (All_Checks);
   use type Unsigned_Types.Long_Long_Unsigned;

   function Value_Long_Long_Unsigned (Str : String)
      return Unsigned_Types.Long_Long_Unsigned
   is
      Index : Positive := Str'First;
      Result : Unsigned_Types.Long_Long_Unsigned;
   begin
      Val_Uns.Skip_Spaces (Str, Index);
      if Index < Str'Last and then Str (Index) = '+' then
         Index := Index + 1;
      end if;
      Get_Longest_Unsigned_Literal_Without_Sign (Str, Index, Result);
      Val_Uns.Check_Last (Str, Index);
      return Result;
   end Value_Long_Long_Unsigned;

   procedure Get_Longest_Unsigned (
      S : String;
      Index : in out Positive;
      Result : out Formatting.Longest_Unsigned;
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
   end Get_Longest_Unsigned;

   procedure Get_Longest_Unsigned_Literal_Without_Sign (
      S : String;
      Index : in out Positive;
      Result : out Formatting.Longest_Unsigned)
   is
      Base : Formatting.Base_Type := 10;
      Exponent : Integer;
   begin
      Get_Longest_Unsigned (S, Index, Result, Base => Base);
      if Index <= S'Last and then S (Index) = '#' then
         Index := Index + 1;
         if Result <
            Unsigned_Types.Long_Long_Unsigned (Formatting.Base_Type'First)
            or else Result >
               Unsigned_Types.Long_Long_Unsigned (Formatting.Base_Type'Last)
         then
            raise Constraint_Error;
         end if;
         Base := Formatting.Base_Type (Result);
         Get_Longest_Unsigned (S, Index, Result, Base => Base);
         if Index > S'Last or else S (Index) /= '#' then
            raise Constraint_Error;
         end if;
         Index := Index + 1;
      end if;
      Val_Uns.Get_Exponent (S, Index, Exponent, Positive_Only => True);
      if Exponent < 0 then
         raise Constraint_Error;
      elsif Exponent > 0 then
         Result := Result *
            Unsigned_Types.Long_Long_Unsigned (Base) ** Exponent;
      end if;
   end Get_Longest_Unsigned_Literal_Without_Sign;

end System.Val_LLU;
