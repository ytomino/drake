pragma License (Unrestricted);
--  extended unit specialized for FreeBSD (or Linux)
private with C;
package Ada.Environment_Encoding.Names is
   --  Constants for schemes of platform-depended text encoding.
   pragma Preelaborate;

   UTF_8 : Encoding_Id
      renames Environment_Encoding.UTF_8;
   UTF_16 : Encoding_Id
      renames Environment_Encoding.UTF_16;
   UTF_32 : Encoding_Id
      renames Environment_Encoding.UTF_32;

   UTF_16BE : constant Encoding_Id;
   UTF_16LE : constant Encoding_Id;

   UTF_32BE : constant Encoding_Id;
   UTF_32LE : constant Encoding_Id;

   Latin_1 : constant Encoding_Id;

   Windows_31J : constant Encoding_Id;

   EUC_JP : constant Encoding_Id;

private
   use type C.char_array;

   UTF_16BE : constant Encoding_Id :=
      System.Native_Encoding.UTF_16_Names (System.High_Order_First)(0)'Access;
   UTF_16LE : constant Encoding_Id :=
      System.Native_Encoding.UTF_16_Names (System.Low_Order_First)(0)'Access;

   UTF_32BE : constant Encoding_Id :=
      System.Native_Encoding.UTF_32_Names (System.High_Order_First)(0)'Access;
   UTF_32LE : constant Encoding_Id :=
      System.Native_Encoding.UTF_32_Names (System.Low_Order_First)(0)'Access;

   Latin_1_Name : aliased constant C.char_array (0 .. 5) :=
      "CP819" & C.char'Val (0);
   Latin_1 : constant Encoding_Id := Latin_1_Name (0)'Access;

   Windows_31J_Name : aliased constant C.char_array (0 .. 5) :=
      "CP932" & C.char'Val (0);
   Windows_31J : constant Encoding_Id := Windows_31J_Name (0)'Access;

   EUC_JP_Name : aliased constant C.char_array (0 .. 6) :=
      "EUC-JP" & C.char'Val (0);
   EUC_JP : constant Encoding_Id := EUC_JP_Name (0)'Access;

end Ada.Environment_Encoding.Names;
