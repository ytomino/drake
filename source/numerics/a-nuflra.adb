package body Ada.Numerics.Float_Random is

   function Random (Gen : Generator) return Uniformly_Distributed is
   begin
      return Float (MT19937.Random_0_To_1 (Gen'Unrestricted_Access));
   end Random;

   procedure Reset (Gen : in out Generator; Initiator : Integer) is
   begin
      MT19937.Reset (Gen, MT19937.Cardinal'Mod (Initiator));
   end Reset;

end Ada.Numerics.Float_Random;
