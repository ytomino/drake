--  with System.Unwind.Raising;
--  pragma Warnings (Off, System.Unwind.Raising); -- break "pure" rule
package body System.Exp_Uns is
   pragma Suppress (All_Checks);
   use type Unsigned_Types.Unsigned;

   function Exp_Unsigned (
      Left : Unsigned_Types.Unsigned;
      Right : Natural)
      return Unsigned_Types.Unsigned is
   begin
      if Left = 2 then
--       if Right >= Unsigned_Types.Unsigned'Size then
--          Unwind.Raising.Overflow;
--       end if;
         return Unsigned_Types.Shift_Left (1, Right);
      else
         declare
            Result : Unsigned_Types.Unsigned := 1;
            Factor : Unsigned_Types.Unsigned := Left;
            Exponent : Natural := Right;
         begin
            loop
               if Exponent rem 2 /= 0 then
--                if Unsigned_Types.Unsigned'Last / Factor < Result then
--                   Unwind.Raising.Overflow;
--                end if;
                  Result := Result * Factor;
               end if;
               Exponent := Exponent / 2;
               exit when Exponent = 0;
--             if Unsigned_Types.Unsigned'Last / Factor < Factor then
--                Unwind.Raising.Overflow;
--             end if;
               Factor := Factor * Factor;
            end loop;
            return Result;
         end;
      end if;
   end Exp_Unsigned;

end System.Exp_Uns;
