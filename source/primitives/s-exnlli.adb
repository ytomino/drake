package body System.Exn_LLI is
   pragma Suppress (All_Checks);

   function Exn_Long_Long_Integer (Left : Long_Long_Integer; Right : Natural)
      return Long_Long_Integer
   is
      function Shift_Left (Left : Long_Long_Integer; Right : Natural)
         return Long_Long_Integer;
      pragma Import (Intrinsic, Shift_Left);
   begin
      if Left = 2 then
         return Shift_Left (1, Right);
      else
         declare
            Result : Long_Long_Integer := 1;
            Factor : Long_Long_Integer := Left;
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
   end Exn_Long_Long_Integer;

end System.Exn_LLI;
