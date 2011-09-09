package body System.Exn_Int is
   pragma Suppress (All_Checks);

   function Exn_Integer (Left : Integer; Right : Natural) return Integer is
      function Shift_Left (Left : Integer; Right : Natural) return Integer;
      pragma Import (Intrinsic, Shift_Left);
   begin
      if Left = 2 then
         return Shift_Left (1, Right);
      else
         declare
            Result : Integer := 1;
            Factor : Integer := Left;
            Exponent : Natural := Right;
         begin
            loop
               if Exponent rem 2 /= 0 then
                  Result := Result * Factor;
               end if;
               Exponent := Exponent / 2;
               exit when Exponent = 0;
               Factor := Factor * Factor;
            end loop;
            return Result;
         end;
      end if;
   end Exn_Integer;

end System.Exn_Int;
