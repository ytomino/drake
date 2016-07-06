with Ada.Numerics.Distributions;
package body Ada.Numerics.Discrete_Random is

   function Random (Gen : Generator) return Result_Subtype is
      function Do_Random is
         new Distributions.Linear_Discrete_Random (
            MT19937.Cardinal,
            Result_Subtype,
            Generator,
            Random_32);
   begin
      return Do_Random (Gen'Unrestricted_Access.all);
   end Random;

   function Random (Gen : Generator; First, Last : Result_Subtype)
      return Result_Subtype
   is
      subtype R is Result_Subtype range First .. Last;
      function Do_Random is
         new Distributions.Linear_Discrete_Random (
            MT19937.Cardinal,
            R,
            Generator,
            Random_32);
   begin
      return Do_Random (Gen'Unrestricted_Access.all);
   end Random;

end Ada.Numerics.Discrete_Random;
