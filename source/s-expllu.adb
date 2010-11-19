with System.Unwind.Raising;
pragma Warnings (Off, System.Unwind.Raising);
package body System.Exp_LLU is
   use type Unsigned_Types.Long_Long_Unsigned;

   function Exp_Long_Long_Unsigned (
      Left : Unsigned_Types.Long_Long_Unsigned;
      Right : Natural)
      return Unsigned_Types.Long_Long_Unsigned is
   begin
      if Left = 2 then
         if Right >= Unsigned_Types.Unsigned'Size then
            Unwind.Raising.Overflow;
         end if;
         return Unsigned_Types.Shift_Left (1, Right);
      else
         declare
            Result : Unsigned_Types.Long_Long_Unsigned := 1;
            Factor : Unsigned_Types.Long_Long_Unsigned := Left;
            Exponent : Natural := Right;
         begin
            loop
               if Exponent rem 2 /= 0 then
                  if Unsigned_Types.Long_Long_Unsigned'Last / Factor <
                     Result
                  then
                     Unwind.Raising.Overflow;
                  end if;
                  Result := Result * Factor;
               end if;
               Exponent := Exponent / 2;
               exit when Exponent = 0;
               if Unsigned_Types.Long_Long_Unsigned'Last / Factor < Factor then
                  Unwind.Raising.Overflow;
               end if;
               Factor := Factor * Factor;
            end loop;
            return Result;
         end;
      end if;
   end Exp_Long_Long_Unsigned;

end System.Exp_LLU;
