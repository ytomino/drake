pragma License (Unrestricted);
--  implementation package required by compiler
package System.Fore is
   pragma Pure;

   --  required for Fixed'Fore by compiler (s-fore.ads)
   function Fore (Lo, Hi : Long_Long_Float) return Natural;

end System.Fore;
