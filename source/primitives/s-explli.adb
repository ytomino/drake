with System.Unwind.Raising;
pragma Warnings (Off, System.Unwind.Raising); -- break "pure" rule
package body System.Exp_LLI is
   pragma Suppress (All_Checks);

   function Exp_Long_Long_Integer (Left : Long_Long_Integer; Right : Natural)
      return Long_Long_Integer
   is
      function Shift_Left (Left : Long_Long_Integer; Right : Natural)
         return Long_Long_Integer;
      pragma Import (Intrinsic, Shift_Left);
   begin
      if Left = 2 then
         if Right >= Long_Long_Integer'Size then
            Unwind.Raising.Overflow;
         end if;
         return Shift_Left (1, Right);
      else
         declare
            Result : Long_Long_Integer := 1;
            Factor : Long_Long_Integer := Left;
            Exponent : Natural := Right;
         begin
            if Factor < 0 then
               Factor := -Factor;
               if Exponent rem 2 /= 0 then
                  Result := -1;
               end if;
            end if;
            loop
               if Exponent rem 2 /= 0 then
                  if Result < 0 then
                     if Long_Long_Integer'First / Factor > Result then
                        Unwind.Raising.Overflow;
                     end if;
                  else
                     if Long_Long_Integer'Last / Factor < Result then
                        Unwind.Raising.Overflow;
                     end if;
                  end if;
                  Result := Result * Factor;
               end if;
               Exponent := Exponent / 2;
               exit when Exponent = 0;
               if Long_Long_Integer'Last / Factor < Factor then
                  Unwind.Raising.Overflow;
               end if;
               Factor := Factor * Factor;
            end loop;
            return Result;
         end;
      end if;
   end Exp_Long_Long_Integer;

end System.Exp_LLI;
