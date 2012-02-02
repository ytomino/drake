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

   function Fast_Argument (X : Complex) return Float;
   pragma Inline (Fast_Argument);

   function cabsf (x : Complex) return Float;
   pragma Import (Intrinsic, cabsf, "__builtin_cabsf");
   function Fast_Modulus (X : Complex) return Float
      renames cabsf;

   --  Long_Complex

   type Long_Imaginary is new Long_Float;

   type Long_Complex is record
      Re, Im : Long_Float;
   end record;
   pragma Complex_Representation (Long_Complex);

   function Fast_Argument (X : Long_Complex) return Long_Float;
   pragma Inline (Fast_Argument);

   function cabs (x : Long_Complex) return Long_Float;
   pragma Import (Intrinsic, cabs, "__builtin_cabs");
   function Fast_Modulus (X : Long_Complex) return Long_Float
      renames cabs;

   --  Long_Long_Complex

   type Long_Long_Imaginary is new Long_Long_Float;

   type Long_Long_Complex is record
      Re, Im : Long_Long_Float;
   end record;
   pragma Complex_Representation (Long_Long_Complex);

   function Fast_Argument (X : Long_Long_Complex) return Long_Long_Float;
   pragma Inline (Fast_Argument);

   function Fast_Modulus (X : Long_Long_Complex) return Long_Long_Float;
   pragma Inline (Fast_Modulus);

end System.Long_Long_Complex_Types;
