pragma License (Unrestricted);
--  implementation package required by compiler
with System.Unsigned_Types;
package System.Fore is
   pragma Pure;

   --  required for Fixed'Fore by compiler (s-fore.ads)
   function Fore (Lo, Hi : Long_Long_Float) return Natural;

   --  helper for System.Img_Real
   subtype Base_Type is Unsigned_Types.Unsigned range 2 .. 16;
   function Fore_Width (Value : Long_Long_Float; Base : Base_Type := 10)
      return Positive;

end System.Fore;
