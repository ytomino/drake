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
      return Fore_Width (Max_Abs) + 1;
   end Fore;

   function Fore_Width (Value : Long_Long_Float; Base : Base_Type := 10)
      return Positive
   is
      V : Long_Long_Float := Value;
      Result : Positive := 1;
   begin
      while V >= Long_Long_Float (Base) loop
         V := V / Long_Long_Float (Base);
         Result := Result + 1;
      end loop;
      return Result;
   end Fore_Width;

end System.Fore;
