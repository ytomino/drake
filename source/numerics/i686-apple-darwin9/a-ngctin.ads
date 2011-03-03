pragma License (Unrestricted);
--  implementation unit
generic
package Ada.Numerics.Generic_Complex_Types.Inside is
   pragma Pure;

   function Argument (X : Complex) return Real'Base;
   pragma Inline (Argument);

end Ada.Numerics.Generic_Complex_Types.Inside;
