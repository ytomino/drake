pragma License (Unrestricted);
--  implementation package required by compiler
package System.Bit_Ops is
   pragma Pure;

   --  required for "=" packed boolean array by compiler (s-bitop.ads)
   function Bit_Eq (
      Left : Address;
      Llen : Natural;
      Right : Address;
      Rlen : Natural)
      return Boolean;

end System.Bit_Ops;
