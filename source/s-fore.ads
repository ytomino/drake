pragma License (Unrestricted);
--  implementation unit required by compiler
package System.Fore is
   pragma Pure;

   --  required for Fixed'Fore by compiler (s-fore.ads)
   function Fore (Lo, Hi : Long_Long_Float) return Natural;

   --  helper for System.Img_Real
   subtype Number_Base is Integer range 2 .. 16; -- same as Text_IO.Number_Base
   function Fore_Width (Value : Long_Long_Float; Base : Number_Base := 10)
      return Positive;

end System.Fore;
