with System.Formatting;
package body System.Val_Uns is
   pragma Suppress (All_Checks);
   use type Unsigned_Types.Unsigned;

   function Value_Unsigned (Str : String) return Unsigned_Types.Unsigned is
      Position : Positive := Str'First;
      Last : Natural;
      Result : Unsigned_Types.Unsigned;
      Base : Formatting.Base_Type;
      Error : Boolean;
   begin
      Skip_Spaces (Str, Position);
      Formatting.Value (
         Str (Position .. Str'Last),
         Last,
         Result,
         Error => Error);
      if Error then
         raise Constraint_Error;
      end if;
      Position := Last + 1;
      if Position <= Str'Last and then Str (Position) = '#' then
         Position := Position + 1;
         if Result not in Formatting.Base_Type then
            raise Constraint_Error;
         end if;
         Base := Result;
         Formatting.Value (
            Str (Position .. Str'Last),
            Last,
            Result,
            Base => Base,
            Error => Error);
         if Error then
            raise Constraint_Error;
         end if;
         Position := Last + 1;
         if Position > Str'Last or else Str (Position) /= '#' then
            raise Constraint_Error;
         end if;
         Last := Position;
      end if;
      if Last /= Str'Last then
         raise Constraint_Error;
      end if;
      return Result;
   end Value_Unsigned;

   procedure Skip_Spaces (S : String; Index : in out Positive) is
   begin
      while Index <= S'Last and then S (Index) = ' ' loop
         Index := Index + 1;
      end loop;
   end Skip_Spaces;

end System.Val_Uns;
