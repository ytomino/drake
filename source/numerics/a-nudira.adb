with Ada.Numerics.Distributions;
package body Ada.Numerics.Discrete_Random is
   pragma Suppress (All_Checks);

   function Random (Gen : Generator) return Result_Subtype is
      function To is
         new Distributions.Linear_Discrete (MT19937.Cardinal, Result_Subtype);
   begin
      return To (Random_32 (Gen'Unrestricted_Access));
   end Random;

end Ada.Numerics.Discrete_Random;
