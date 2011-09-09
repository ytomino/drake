pragma License (Unrestricted);
with Ada.Numerics.Generic_Complex_Types; -- see G.1.1
pragma Elaborate_All (Ada.Numerics.Generic_Complex_Types);
package Interfaces.Fortran is
   pragma Pure;

   type Fortran_Integer is new Integer; -- implementation-defined

   type Real is new Float; -- implementation-defined
   type Double_Precision is new Long_Float; -- implementation-defined

   type Logical is new Boolean;

   package Single_Precision_Complex_Types is
      new Ada.Numerics.Generic_Complex_Types (Real);

   type Complex is new Single_Precision_Complex_Types.Complex;

   subtype Imaginary is Single_Precision_Complex_Types.Imaginary;
   --  modified
--  i : Imaginary renames Single_Precision_Complex_Types.i;
--  j : Imaginary renames Single_Precision_Complex_Types.j;
   function i return Imaginary renames Single_Precision_Complex_Types.i;
   function j return Imaginary renames Single_Precision_Complex_Types.j;

   type Character_Set is
      new Character; -- implementation-defined character type

   type Fortran_Character is array (Positive range <>) of Character_Set;
   pragma Pack (Fortran_Character);

--  function To_Fortran (Item : Character) return Character_Set;
--  function To_Ada (Item : Character_Set) return Character;

--  function To_Fortran (Item : String) return Fortran_Character;
--  function To_Ada (Item : Fortran_Character) return String;

--  procedure To_Fortran (
--    Item : String;
--    Target : out Fortran_Character;
--    Last : out Natural);

--  procedure To_Ada (
--    Item : Fortran_Character;
--    Target : out String;
--    Last : out Natural);

end Interfaces.Fortran;
