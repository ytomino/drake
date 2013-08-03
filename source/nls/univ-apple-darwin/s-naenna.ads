pragma License (Unrestricted);
--  extended unit
package System.Native_Encoding.Names is
   --  Constants for schemes of platform-depended text encoding.
   pragma Preelaborate;

   UTF_8 : Encoding_Id
      renames Native_Encoding.UTF_8;
   UTF_16 : Encoding_Id
      renames Native_Encoding.UTF_16;
   UTF_32 : Encoding_Id
      renames Native_Encoding.UTF_32;

   UTF_16BE : constant Encoding_Id;
   UTF_16LE : constant Encoding_Id;

   UTF_32BE : constant Encoding_Id;
   UTF_32LE : constant Encoding_Id;

   Latin_1 : constant Encoding_Id;

   Windows_31J : constant Encoding_Id;

   EUC_JP : constant Encoding_Id;

private

   UTF_16BE : constant Encoding_Id :=
      UTF_16_Names (High_Order_First)(0)'Access;
   UTF_16LE : constant Encoding_Id :=
      UTF_16_Names (Low_Order_First)(0)'Access;

   UTF_32BE : constant Encoding_Id :=
      UTF_32_Names (High_Order_First)(0)'Access;
   UTF_32LE : constant Encoding_Id :=
      UTF_32_Names (Low_Order_First)(0)'Access;

   Latin_1_Name : aliased constant C.char_array (0 .. 10) :=
      "ISO-8859-1" & C.char'Val (0);
   Latin_1 : constant Encoding_Id := Latin_1_Name (0)'Access;

   Windows_31J_Name : aliased constant C.char_array (0 .. 11) :=
      "windows-31j" & C.char'Val (0); -- ibm-943_P15A-2003
   Windows_31J : constant Encoding_Id := Windows_31J_Name (0)'Access;

   EUC_JP_Name : aliased constant C.char_array (0 .. 6) :=
      "EUC-JP" & C.char'Val (0); -- ibm-33722_P12A_P12A-2004_U2
   EUC_JP : constant Encoding_Id := EUC_JP_Name (0)'Access;

end System.Native_Encoding.Names;
