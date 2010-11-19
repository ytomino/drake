with System.Unwind.Raising;
pragma Warnings (Off, System.Unwind.Raising);
package body System.Exp_Int is
   pragma Suppress (All_Checks);

   function Exp_Integer (Left : Integer; Right : Natural) return Integer is
      function Shift_Left (Left : Integer; Right : Natural) return Integer;
      pragma Import (Intrinsic, Shift_Left);
   begin
      if Left = 2 then
         if Right >= Integer'Size then
            Unwind.Raising.Overflow;
         end if;
         return Shift_Left (1, Right);
      else
         declare
            Result : Integer := 1;
            Factor : Integer := Left;
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
                     if Integer'First / Factor > Result then
                        Unwind.Raising.Overflow;
                     end if;
                  else
                     if Integer'Last / Factor < Result then
                        Unwind.Raising.Overflow;
                     end if;
                  end if;
                  Result := Result * Factor;
               end if;
               Exponent := Exponent / 2;
               exit when Exponent = 0;
               if Integer'Last / Factor < Factor then
                  Unwind.Raising.Overflow;
               end if;
               Factor := Factor * Factor;
            end loop;
            return Result;
         end;
      end if;
   end Exp_Integer;

end System.Exp_Int;
