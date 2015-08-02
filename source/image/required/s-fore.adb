with System.Formatting.Float;
package body System.Fore is

   function Fore (Lo, Hi : Long_Long_Float) return Natural is
   begin
      return Formatting.Float.Fore_Width (Lo, Hi) + 1; -- including sign
   end Fore;

end System.Fore;
