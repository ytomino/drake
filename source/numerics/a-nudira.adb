package body Ada.Numerics.Discrete_Random is

   function Random (Gen : Generator) return Result_Subtype is
      subtype Real is Long_Long_Float;
      Random : constant Real :=
         MT19937.Random_0_To_Less_1 (Gen'Unrestricted_Access);
      Width : constant Integer :=
         Result_Subtype'Pos (Result_Subtype'Last) -
         Result_Subtype'Pos (Result_Subtype'First) + 1;
      Position : constant Integer :=
         Result_Subtype'Pos (Result_Subtype'First) +
         Integer (Real'Floor (Random * Real (Width)));
   begin
      return Result_Subtype'Val (Position);
   end Random;

   procedure Reset (Gen : in out Generator; Initiator : Integer) is
   begin
      MT19937.Reset (Gen, MT19937.Cardinal'Mod (Initiator));
   end Reset;

end Ada.Numerics.Discrete_Random;
