with System.Wid_Char;
package body System.Wid_WChar is
   pragma Suppress (All_Checks);

   function Width_Wide_Character (Lo, Hi : Wide_Character) return Natural is
   begin
      if Lo > Hi then
         return 0;
      elsif Hi <= Wide_Character'Val (16#7f#) then
         return Wid_Char.Width_Character (
            Character'Val (Wide_Character'Pos (Lo)),
            Character'Val (Wide_Character'Pos (Hi)));
      else
         return 8; -- "Hex_XXXX"
      end if;
   end Width_Wide_Character;

   function Width_Wide_Wide_Character (Lo, Hi : Wide_Wide_Character)
      return Natural is
   begin
      if Lo > Hi then
         return 0;
      elsif Hi <= Wide_Wide_Character'Val (16#7f#) then
         return Wid_Char.Width_Character (
            Character'Val (Wide_Wide_Character'Pos (Lo)),
            Character'Val (Wide_Wide_Character'Pos (Hi)));
      else
         return 12; -- "Hex_XXXXXXXX"
      end if;
   end Width_Wide_Wide_Character;

end System.Wid_WChar;
