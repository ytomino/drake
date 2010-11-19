package body System.Fore is
   pragma Suppress (All_Checks);

   function Fore (Lo, Hi : Long_Long_Float) return Natural is
      Max_Abs : Long_Long_Float;
   begin
      if Lo > Hi then
         return 0;
      elsif Hi <= 0.0 then
         Max_Abs := -Lo;
      elsif Lo >= 0.0 then
         Max_Abs := Hi;
      else --  Lo < 0 and then Hi > 0
         Max_Abs := Long_Long_Float'Max (-Lo, Hi);
      end if;
      declare
         Result : Natural := 2;
      begin
         while Max_Abs >= 10.0 loop
            Max_Abs := Max_Abs / 10.0;
            Result := Result + 1;
         end loop;
         return Result;
      end;
   end Fore;

end System.Fore;
