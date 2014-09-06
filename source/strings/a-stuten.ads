pragma License (Unrestricted);
--  Ada 2012
private with System;
package Ada.Strings.UTF_Encoding is
   pragma Pure;

   --  Declarations common to the string encoding packages
   --  modified
   --  UTF-32 support is added.
   type Encoding_Scheme is (UTF_8, UTF_16BE, UTF_16LE,
      UTF_32BE, UTF_32LE); -- additional

   subtype UTF_String is String;

   subtype UTF_8_String is String;

   subtype UTF_16_Wide_String is Wide_String;

   --  extended
   subtype UTF_32_Wide_Wide_String is Wide_Wide_String;

   Encoding_Error : exception;

   BOM_8 : aliased constant String := -- "aliased" is extended
      Character'Val (16#EF#)
      & Character'Val (16#BB#)
      & Character'Val (16#BF#);

   BOM_16BE : aliased constant String := -- "aliased" is extended
      Character'Val (16#FE#)
      & Character'Val (16#FF#);

   BOM_16LE : aliased constant String := -- "aliased" is extended
      Character'Val (16#FF#)
      & Character'Val (16#FE#);

   BOM_16 : constant Wide_String :=
      (1 => Wide_Character'Val (16#FEFF#));

   --  extended
   BOM_32BE : aliased constant String :=
      Character'Val (16#00#)
      & Character'Val (16#00#)
      & Character'Val (16#FE#)
      & Character'Val (16#FF#);

   --  extended
   BOM_32LE : aliased constant String :=
      Character'Val (16#FF#)
      & Character'Val (16#FE#)
      & Character'Val (16#00#)
      & Character'Val (16#00#);

   --  extended
   BOM_32 : constant Wide_Wide_String :=
      (1 => Wide_Wide_Character'Val (16#0000FEFF#));

   function Encoding (
      Item : UTF_String;
      Default : Encoding_Scheme := UTF_8)
      return Encoding_Scheme;

   --  extended
   UTF_16_Wide_String_Scheme : constant
      Encoding_Scheme range UTF_16BE .. UTF_16LE;
   UTF_32_Wide_Wide_String_Scheme : constant
      Encoding_Scheme range UTF_32BE .. UTF_32LE;

private

   use type System.Bit_Order;

   UTF_16_Wide_String_Scheme : constant Encoding_Scheme :=
      Encoding_Scheme'Val (
         Encoding_Scheme'Pos (UTF_16BE) *
            Boolean'Pos (System.Default_Bit_Order = System.High_Order_First)
         + Encoding_Scheme'Pos (UTF_16LE) *
            Boolean'Pos (System.Default_Bit_Order = System.Low_Order_First));

   UTF_32_Wide_Wide_String_Scheme : constant Encoding_Scheme :=
      Encoding_Scheme'Val (
         Encoding_Scheme'Pos (UTF_32BE) *
            Boolean'Pos (System.Default_Bit_Order = System.High_Order_First)
         + Encoding_Scheme'Pos (UTF_32LE) *
            Boolean'Pos (System.Default_Bit_Order = System.Low_Order_First));

   BOM_Table : constant array (Encoding_Scheme) of
      not null access constant UTF_String := (
         UTF_8 => BOM_8'Access,
         UTF_16BE => BOM_16BE'Access,
         UTF_16LE => BOM_16LE'Access,
         UTF_32BE => BOM_32BE'Access,
         UTF_32LE => BOM_32LE'Access);

end Ada.Strings.UTF_Encoding;
