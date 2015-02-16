pragma License (Unrestricted);
--  implementation unit required by compiler
package System.Mantissa is
   pragma Pure;

   --  required for Fixed'Mantissa, if its range is dynamic (s-mantis.ads)
   function Mantissa_Value (First, Last : Integer) return Natural;

end System.Mantissa;
