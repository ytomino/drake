pragma License (Unrestricted);
--  implementation unit specialized for Darwin (or Linux, or Windows)
package System.Long_Long_Complex_Types is
   pragma Pure;

   --  Complex

   type Imaginary is new Float;

   type Complex is record
      Re, Im : Float;
   end record;
   pragma Complex_Representation (Complex);

   --  Long_Complex

   type Long_Imaginary is new Long_Float;

   type Long_Complex is record
      Re, Im : Long_Float;
   end record;
   pragma Complex_Representation (Long_Complex);

   --  Long_Long_Complex

   type Long_Long_Imaginary is new Long_Long_Float;

   type Long_Long_Complex is record
      Re, Im : Long_Long_Float;
   end record;
   pragma Complex_Representation (Long_Long_Complex);

end System.Long_Long_Complex_Types;
