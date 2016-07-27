pragma License (Unrestricted);
with Ada.Numerics.Generic_Complex_Types; -- see G.1.1
pragma Elaborate_All (Ada.Numerics.Generic_Complex_Types);
package Interfaces.Fortran is
   pragma Pure;

   type Fortran_Integer is new Integer; -- implementation-defined

   type Real is new Float; -- implementation-defined
   type Double_Precision is new Long_Float; -- implementation-defined

   type Logical is new Boolean
      with Convention => Fortran;
   for Logical'Size use Integer'Size;

   package Single_Precision_Complex_Types is
      new Ada.Numerics.Generic_Complex_Types (Real);

   type Complex is new Single_Precision_Complex_Types.Complex;

   subtype Imaginary is Single_Precision_Complex_Types.Imaginary;
   --  modified
--  i : Imaginary renames Single_Precision_Complex_Types.i;
--  j : Imaginary renames Single_Precision_Complex_Types.j;
   function i return Imaginary renames Single_Precision_Complex_Types.i;
   function j return Imaginary renames Single_Precision_Complex_Types.j;

   --  Note: character(kind=ascii) is treated as Latin-1
   --    and character(kind=ucs4) is treated as UTF-32 in gfortran.

   type Character_Set is
      new Character; -- implementation-defined character type

   type Fortran_Character is array (Positive range <>) of Character_Set;
   pragma Pack (Fortran_Character);

   --  modified
   function To_Fortran (
      Item : Character;
      Substitute : Character_Set := '?') -- additional
      return Character_Set;
   function To_Ada (
      Item : Character_Set;
      Substitute : Character := '?') -- additional
      return Character;

   pragma Inline (To_Fortran);
   pragma Inline (To_Ada);

   --  modified
   function To_Fortran (
      Item : String;
      Substitute : Fortran_Character := "?") -- additional
      return Fortran_Character;
   function To_Ada (
      Item : Fortran_Character;
      Substitute : String := "?") -- additional, and unreferenced
      return String;

   --  modified
   procedure To_Fortran (
      Item : String;
      Target : out Fortran_Character;
      Last : out Natural;
      Substitute : Fortran_Character := "?"); -- additional

   --  modified
   procedure To_Ada (
      Item : Fortran_Character;
      Target : out String;
      Last : out Natural;
      Substitute : String := "?"); -- additional, and unreferenced

   --  Note: In RM B.5(21), an implementation is permitted to add
   --    Integer_Star_n, Real_Star_n, Logical_Star_n, Complex_Star_n,
   --    Integer_Kind_n, Real_Kind_n, Logical_Kind_n, Complex_Kind_n,
   --    and Character_Kind_n.

   --  Integer_Star_n and Integer_Kind_n

   subtype Integer_Kind_1 is Integer_8;
   subtype Integer_Kind_2 is Integer_16;
   subtype Integer_Kind_4 is Integer_32;
   subtype Integer_Kind_8 is Integer_64;
--  type Integer_Kind_16 is ...; -- supported in gfortran, but not in GNAT

   subtype Integer_Star_1 is Integer_Kind_1;
   subtype Integer_Star_2 is Integer_Kind_2;
   subtype Integer_Star_4 is Integer_Kind_4;
   subtype Integer_Star_8 is Integer_Kind_8;
--  subtype Integer_Star_16 is Integer_Kind_16;

   --  Real_Star_n and Real_Kind_n

   pragma Compile_Time_Error (
      Long_Long_Float'Model_Mantissa /= 64
         and then Long_Long_Float'Size /= 80
         and then Long_Long_Float'Size /= 96,
      "Long_Long_Float is not Real_Kind_10.");

   subtype Real_Kind_4 is Real;
   subtype Real_Kind_8 is Double_Precision;
   type Real_Kind_10 is new Long_Long_Float; -- i686/x86_64
--  type Real_Kind_16 is ...; --  libquadmath

   subtype Real_Star_4 is Real_Kind_4;
   subtype Real_Star_8 is Real_Kind_8;
   subtype Real_Star_10 is Real_Kind_10;
--  subtype Real_Star_16 is Real_Kind_16;

   --  Logical_Star_n and Logical_Kind_n

   type Logical_Kind_1 is new Boolean
      with Convention => Fortran;
   for Logical_Kind_1'Size use 8;
   for Logical_Kind_1'Alignment use 1;
   type Logical_Kind_2 is new Boolean
      with Convention => Fortran;
   for Logical_Kind_2'Size use 16;
   for Logical_Kind_2'Alignment use 2;
   type Logical_Kind_4 is new Boolean
      with Convention => Fortran;
   for Logical_Kind_4'Size use 32;
   for Logical_Kind_4'Alignment use 4;
   type Logical_Kind_8 is new Boolean
      with Convention => Fortran;
   for Logical_Kind_8'Size use 64;
   for Logical_Kind_8'Alignment use 8;
--  type Logical_Kind_16 is ...; -- supported in gfortran, but not in GNAT

   subtype Logical_Star_1 is Logical_Kind_1;
   subtype Logical_Star_2 is Logical_Kind_2;
   subtype Logical_Star_4 is Logical_Kind_4;
   subtype Logical_Star_8 is Logical_Kind_8;
--  subtype Logical_Star_16 is Logical_Kind_16;

   --  Complex_Star_n and Complex_Kind_n

   package Double_Precision_Complex_Types is
      new Ada.Numerics.Generic_Complex_Types (Double_Precision);
      --  AARM B.5(21.a)

   package Complex_Types_Kind_4 renames Single_Precision_Complex_Types;
   subtype Complex_Kind_4 is Complex_Types_Kind_4.Complex;
   package Complex_Types_Kind_8 renames Double_Precision_Complex_Types;
   subtype Complex_Kind_8 is Complex_Types_Kind_8.Complex;
   package Complex_Types_Kind_10 is
      new Ada.Numerics.Generic_Complex_Types (Real_Kind_10);
   subtype Complex_Kind_10 is Complex_Types_Kind_10.Complex;
--  type Complex_Kind_16 is ...; -- libquadmath

   subtype Complex_Star_8 is Complex_Kind_4;
   subtype Complex_Star_16 is Complex_Kind_8;
   subtype Complex_Star_20 is Complex_Kind_10;
--  subtype Complex_Star_32 is Complex_Kind_16;

   --  Character_Kind_n

   subtype Character_Set_Kind_1 is Character_Set;
   subtype Character_Kind_1 is Fortran_Character;
   type Character_Set_Kind_4 is new Wide_Wide_Character;
   type Character_Kind_4 is array (Positive range <>) of Character_Set_Kind_4;

end Interfaces.Fortran;
