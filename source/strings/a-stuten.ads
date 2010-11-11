pragma License (Unrestricted);
--  Ada 2012
package Ada.Strings.UTF_Encoding is
   pragma Pure;

   --  Declarations common to the string encoding packages
   type Encoding_Scheme is (UTF_8, UTF_16BE, UTF_16LE,
      UTF_32BE, UTF_32LE); --  extended

   subtype UTF_String is String;

   subtype UTF_8_String is String;

   subtype UTF_16_Wide_String is Wide_String;

   --  extended
   subtype UTF_32_Wide_Wide_String is Wide_Wide_String;

   Encoding_Error : exception;

   BOM_8 : constant String :=
      Character'Val (16#EF#) &
      Character'Val (16#BB#) &
      Character'Val (16#BF#);

   BOM_16BE : constant String :=
      Character'Val (16#FE#) &
      Character'Val (16#FF#);

   BOM_16LE : constant String :=
      Character'Val (16#FF#) &
      Character'Val (16#FE#);

   BOM_16 : constant Wide_String :=
       (1 => Wide_Character'Val (16#FEFF#));

   --  extended
   BOM_32BE : constant String :=
      Character'Val (16#00#) &
      Character'Val (16#00#) &
      Character'Val (16#FE#) &
      Character'Val (16#FF#);

   --  extended
   BOM_32LE : constant String :=
      Character'Val (16#FF#) &
      Character'Val (16#FE#) &
      Character'Val (16#00#) &
      Character'Val (16#00#);

   --  extended
   BOM_32 : constant Wide_Wide_String :=
       (1 => Wide_Wide_Character'Val (16#0000FEFF#));

   function Encoding (
      Item : UTF_String;
      Default : Encoding_Scheme := UTF_8)
      return Encoding_Scheme;

end Ada.Strings.UTF_Encoding;
