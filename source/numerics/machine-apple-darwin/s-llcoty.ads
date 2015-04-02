pragma License (Unrestricted);
--  implementation unit
package System.Long_Long_Complex_Types is
   pragma Pure;

   --  Complex

   type Imaginary is new Float;

   type Complex is record
      Re, Im : Float;
   end record;
   pragma Complex_Representation (Complex);

   function cargf (x : Complex) return Float;
   pragma Import (Intrinsic, cargf, "__builtin_cargf");
   function Fast_Argument (X : Complex) return Float
      renames cargf;

   --  Long_Complex

   type Long_Imaginary is new Long_Float;

   type Long_Complex is record
      Re, Im : Long_Float;
   end record;
   pragma Complex_Representation (Long_Complex);

   function carg (x : Long_Complex) return Long_Float;
   pragma Import (Intrinsic, carg, "__builtin_carg");
   function Fast_Argument (X : Long_Complex) return Long_Float
      renames carg;

   --  Long_Long_Complex

   type Long_Long_Imaginary is new Long_Long_Float;

   type Long_Long_Complex is record
      Re, Im : Long_Long_Float;
   end record;
   pragma Complex_Representation (Long_Long_Complex);

   function cargl (x : Long_Long_Complex) return Long_Long_Float;
   pragma Import (Intrinsic, cargl, "__builtin_cargl");
   function Fast_Argument (X : Long_Long_Complex) return Long_Long_Float
      renames cargl;

   function cabsl (x : Long_Long_Complex) return Long_Long_Float;
   pragma Import (Intrinsic, cabsl, "__builtin_cabsl");
   function Fast_Modulus (X : Long_Long_Complex) return Long_Long_Float
      renames cabsl;

end System.Long_Long_Complex_Types;
