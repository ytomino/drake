package body System.Exp_Mod is

   function Exp_Modular (Left : Integer; Modulus : Integer; Right : Natural)
      return Integer
   is
      M : constant Long_Long_Integer := Long_Long_Integer (Modulus);
      Result : Integer := 1;
      Factor : Integer := Left;
      Exponent : Natural := Right;
   begin
      loop
         if Exponent rem 2 /= 0 then
            Result := Integer (
               Long_Long_Integer (Result) * Long_Long_Integer (Factor) mod M);
         end if;
         Exponent := Exponent / 2;
         exit when Exponent = 0;
         Factor := Integer (
            Long_Long_Integer (Factor) * Long_Long_Integer (Factor) mod M);
      end loop;
      return Result;
   end Exp_Modular;

end System.Exp_Mod;
