package body Ada.Strings.UTF_Encoding is

   function Encoding (
      Item : UTF_String;
      Default : Encoding_Scheme := UTF_8)
      return Encoding_Scheme is
   begin
      if Item'Length >= 4
         and then Item (Item'First) = BOM_32BE (1)
         and then Item (Item'First + 1) = BOM_32BE (2)
         and then Item (Item'First + 2) = BOM_32BE (3)
         and then Item (Item'First + 3) = BOM_32BE (4)
      then
         return UTF_32BE;
      elsif Item'Length >= 4
         and then Item (Item'First) = BOM_32LE (1)
         and then Item (Item'First + 1) = BOM_32LE (2)
         and then Item (Item'First + 2) = BOM_32LE (3)
         and then Item (Item'First + 3) = BOM_32LE (4)
      then
         return UTF_32LE;
      elsif Item'Length >= 3
         and then Item (Item'First) = BOM_8 (1)
         and then Item (Item'First + 1) = BOM_8 (2)
         and then Item (Item'First + 2) = BOM_8 (3)
      then
         return UTF_8;
      elsif Item'Length >= 2
         and then Item (Item'First) = BOM_16BE (1)
         and then Item (Item'First + 1) = BOM_16BE (2)
      then
         return UTF_16BE;
      elsif Item'Length >= 2
         and then Item (Item'First) = BOM_16LE (1)
         and then Item (Item'First + 1) = BOM_16LE (2)
      then
         return UTF_16LE;
      else
         return Default;
      end if;
   end Encoding;

end Ada.Strings.UTF_Encoding;
