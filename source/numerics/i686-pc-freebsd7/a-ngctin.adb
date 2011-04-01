with Ada.Numerics.Generic_Elementary_Arctan;
package body Ada.Numerics.Generic_Complex_Types.Inside is

   --  FreeBSD does not have carg

   function Arctan is new Generic_Elementary_Arctan (Real'Base);

   function Argument (X : Complex) return Real'Base is
   begin
      return Arctan (X.Im, X.Re);
   end Argument;

end Ada.Numerics.Generic_Complex_Types.Inside;
