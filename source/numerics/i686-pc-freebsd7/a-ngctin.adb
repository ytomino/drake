with Ada.Float.Elementary_Functions;
package body Ada.Numerics.Generic_Complex_Types.Inside is
   pragma Suppress (All_Checks);

   function Arctan is new Float.Elementary_Functions.Arctan (Real'Base);

   --  implementation

   function Argument (X : Complex) return Real'Base is
   begin
      --  FreeBSD does not have carg
      return Arctan (X.Im, X.Re);
   end Argument;

end Ada.Numerics.Generic_Complex_Types.Inside;
