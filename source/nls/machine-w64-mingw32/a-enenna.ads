pragma License (Unrestricted);
--  extended unit specialized for Windows
package Ada.Environment_Encoding.Names is
   --  Constants for schemes of system-specific text encoding.
   pragma Preelaborate;

   UTF_8 : Encoding_Id
      renames Environment_Encoding.UTF_8;
   UTF_16 : Encoding_Id
      renames Environment_Encoding.UTF_16;
   UTF_32 : Encoding_Id
      renames Environment_Encoding.UTF_32;

   Latin_1 : constant Encoding_Id;

   Windows_31J : constant Encoding_Id;

   EUC_JP : constant Encoding_Id;

private

   Latin_1 : constant Encoding_Id := 1252; -- 819 ?

   Windows_31J : constant Encoding_Id := 932;

   EUC_JP : constant Encoding_Id := 20932; -- 54932 ?

end Ada.Environment_Encoding.Names;
