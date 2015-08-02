package body Ada.Numerics.Float_Random is

   function Random (Gen : Generator) return Uniformly_Distributed is
   begin
      return Float (MT19937.Random_0_To_1 (Gen'Unrestricted_Access.all));
   end Random;

end Ada.Numerics.Float_Random;
