with System.Formatting.Float;
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
      else -- Lo < 0 and then Hi > 0
         Max_Abs := Long_Long_Float'Max (-Lo, Hi);
      end if;
      return Formatting.Float.Fore_Width (Max_Abs) + 1; -- including sign
   end Fore;

end System.Fore;
