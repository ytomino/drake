package body Ada.Strings.UTF_Encoding is

   function Contains_BOM_8 (Item : UTF_String) return Boolean is
   begin
      return Item'Length >= 3
         and then Item (Item'First) = BOM_8 (1)
         and then Item (Item'First + 1) = BOM_8 (2)
         and then Item (Item'First + 2) = BOM_8 (3);
   end Contains_BOM_8;

   function Contains_BOM_16BE (Item : UTF_String) return Boolean is
   begin
      return Item'Length >= 2
         and then Item (Item'First) = BOM_16BE (1)
         and then Item (Item'First + 1) = BOM_16BE (2);
   end Contains_BOM_16BE;

   function Contains_BOM_16LE (Item : UTF_String) return Boolean is
   begin
      return Item'Length >= 2
         and then Item (Item'First) = BOM_16LE (1)
         and then Item (Item'First + 1) = BOM_16LE (2);
   end Contains_BOM_16LE;

   function Contains_BOM_32BE (Item : UTF_String) return Boolean is
   begin
      return Item'Length >= 4
         and then Item (Item'First) = BOM_32BE (1)
         and then Item (Item'First + 1) = BOM_32BE (2)
         and then Item (Item'First + 2) = BOM_32BE (3)
         and then Item (Item'First + 3) = BOM_32BE (4);
   end Contains_BOM_32BE;

   function Contains_BOM_32LE (Item : UTF_String) return Boolean is
   begin
      return Item'Length >= 4
         and then Item (Item'First) = BOM_32LE (1)
         and then Item (Item'First + 1) = BOM_32LE (2)
         and then Item (Item'First + 2) = BOM_32LE (3)
         and then Item (Item'First + 3) = BOM_32LE (4);
   end Contains_BOM_32LE;

   function Encoding (
      Item : UTF_String;
      Default : Encoding_Scheme := UTF_8)
      return Encoding_Scheme is
   begin
      if Contains_BOM_32BE (Item) then -- 4 elements
         return UTF_32BE;
      elsif Contains_BOM_32LE (Item) then
         return UTF_32LE;
      elsif Contains_BOM_8 (Item) then -- 3 elements
         return UTF_8;
      elsif Contains_BOM_16BE (Item) then -- 2 elements
         return UTF_16BE;
      elsif Contains_BOM_16LE (Item) then
         return UTF_16LE;
      else
         return Default;
      end if;
   end Encoding;

   function Generic_Add_Or_Remove_BOM (
      Item : String_Type;
      Output_BOM : Boolean := False)
      return String_Type
   is
      Has_BOM : constant Boolean := Item'Length >= BOM'Length
         and then Item (Item'First .. Item'First + (BOM'Length - 1)) = BOM;
   begin
      if Output_BOM then
         if Has_BOM then
            return Item;
         else
            return BOM & Item;
         end if;
      else
         if Has_BOM then
            return Item (Item'First + BOM'Length .. Item'Last);
         else
            return Item;
         end if;
      end if;
   end Generic_Add_Or_Remove_BOM;

end Ada.Strings.UTF_Encoding;
