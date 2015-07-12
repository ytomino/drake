pragma License (Unrestricted);
package Interfaces.COBOL is
   pragma Preelaborate;

   --  Types and operations for internal data representations

   type Floating is new Float; -- implementation-defined
   type Long_Floating is new Long_Float; -- implementation-defined

   type Binary is new Integer; -- implementation-defined
   type Long_Binary is new Long_Long_Integer; -- implementation-defined

   Max_Digits_Binary : constant := 9; -- implementation-defined
   Max_Digits_Long_Binary : constant := 18; -- implementation-defined

   type Decimal_Element is mod 2 ** 4; -- implementation-defined
   type Packed_Decimal is array (Positive range <>) of Decimal_Element;
   pragma Pack (Packed_Decimal);

   type COBOL_Character is
      new Character; -- implementation-defined character type

   --  modified
--  Ada_To_COBOL : array (Character) of COBOL_Character :=
--    implementation-defined;
   function Ada_To_COBOL (
      Item : Character;
      Substitute : COBOL_Character := '?')
      return COBOL_Character;

   --  modified
--  COBOL_To_Ada : array (COBOL_Character) of Character :=
--    implementation-defined;
   function COBOL_To_Ada (
      Item : COBOL_Character;
      Substitute : Character := '?')
      return Character;

   type Alphanumeric is array (Positive range <>) of COBOL_Character;
   pragma Pack (Alphanumeric);

   --  modified
   function To_COBOL (
      Item : String;
      Substitute : Alphanumeric := "?") -- additional
      return Alphanumeric;
   --  modified
   function To_Ada (
      Item : Alphanumeric;
      Substitute : String := "?") -- additional
      return String;

   --  modified
   procedure To_COBOL (
      Item : String;
      Target : out Alphanumeric;
      Last : out Natural;
      Substitute : Alphanumeric := "?"); -- additional

   --  modified
   procedure To_Ada (
      Item : Alphanumeric;
      Target : out String;
      Last : out Natural;
      Substitute : String := "?"); -- additional

   type Numeric is array (Positive range <>) of COBOL_Character;
   pragma Pack (Numeric);

   --  Formats for COBOL data representations

   type Display_Format is private;

   Unsigned : constant Display_Format;
   Leading_Separate : constant Display_Format;
   Trailing_Separate : constant Display_Format;
   Leading_Nonseparate : constant Display_Format;
   Trailing_Nonseparate : constant Display_Format;

   type Binary_Format is private;

   High_Order_First : constant Binary_Format;
   Low_Order_First : constant Binary_Format;
   Native_Binary : constant Binary_Format;

   type Packed_Format is private;

   Packed_Unsigned : constant Packed_Format;
   Packed_Signed : constant Packed_Format;

   --  Types for external representation of COBOL binary data

   type Byte is mod 2 ** COBOL_Character'Size;
   type Byte_Array is array (Positive range <>) of Byte;
   pragma Pack (Byte_Array);

   Conversion_Error : exception;

   generic
      type Num is delta <> digits <>;
   package Decimal_Conversions is

      --  Display Formats: data values are represented as Numeric

--    function Valid (Item : Numeric; Format : Display_Format) return Boolean;

--    function Length (Format : Display_Format) return Natural;

--    function To_Decimal (Item : Numeric; Format : Display_Format) return Num;

--    function To_Display (Item : Num; Format : Display_Format) return Numeric;

      --  Packed Formats: data values are represented as Packed_Decimal

--    function Valid (Item : Packed_Decimal; Format : Packed_Format)
--       return Boolean;

--    function Length (Format : Packed_Format) return Natural;

--    function To_Decimal (Item : Packed_Decimal; Format : Packed_Format)
--       return Num;

--    function To_Packed (Item : Num; Format : Packed_Format)
--       return Packed_Decimal;

      --  Binary Formats: external data values are represented as Byte_Array

--    function Valid (Item : Byte_Array; Format : Binary_Format)
--       return Boolean;

--    function Length (Format : Binary_Format) return Natural;
--    function To_Decimal (Item : Byte_Array; Format : Binary_Format)
--       return Num;

--    function To_Binary (Item : Num; Format : Binary_Format)
--       return Byte_Array;

      --  Internal Binary formats:
      --  data values are of type Binary or Long_Binary

--    function To_Decimal (Item : Binary) return Num;
--    function To_Decimal (Item : Long_Binary) return Num;

--    function To_Binary (Item : Num) return Binary;
--    function To_Long_Binary (Item : Num) return Long_Binary;

   end Decimal_Conversions;

private

   type Display_Format is (U, LS, TS, LN, TN);
   pragma Discard_Names (Display_Format);

   Unsigned : constant Display_Format := U;
   Leading_Separate : constant Display_Format := LS;
   Trailing_Separate : constant Display_Format := TS;
   Leading_Nonseparate : constant Display_Format := LN;
   Trailing_Nonseparate : constant Display_Format := TN;

   type Binary_Format is (H, L, N);
   pragma Discard_Names (Binary_Format);

   High_Order_First : constant Binary_Format := H;
   Low_Order_First : constant Binary_Format := L;
   Native_Binary : constant Binary_Format := N;

   type Packed_Format is (U, S);
   pragma Discard_Names (Packed_Format);

   Packed_Unsigned : constant Packed_Format := U;
   Packed_Signed : constant Packed_Format := S;

end Interfaces.COBOL;
