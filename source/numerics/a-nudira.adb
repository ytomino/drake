package body Ada.Numerics.Discrete_Random is
   pragma Suppress (All_Checks);

   function Random (Gen : Generator) return Result_Subtype is
      subtype Real is Long_Long_Float;
      Random : constant Real := Random_0_To_Less_1 (Gen'Unrestricted_Access);
      Position : constant Integer := Result_Subtype'Pos (Result_Subtype'First)
         + Integer (Real'Floor (Random * Real (Result_Subtype'Range_Length)));
   begin
      return Result_Subtype'Val (Position);
   end Random;

end Ada.Numerics.Discrete_Random;
