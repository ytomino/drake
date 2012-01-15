pragma License (Unrestricted);
--  implementation unit
generic
package Ada.Numerics.Generic_Complex_Types.Inside is
   pragma Pure;

   function Argument (X : Complex) return Real'Base;
   pragma Inline (Argument);

   function Modulus (X : Complex) return Real'Base;
   pragma Inline (Modulus);

end Ada.Numerics.Generic_Complex_Types.Inside;
