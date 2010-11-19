with System.Img_Char;
package body System.Wid_Char is
   pragma Suppress (All_Checks);

   function Width_Character (Lo, Hi : Character) return Natural is
   begin
      if Lo > Hi then
         return 0;
      elsif Hi >= Character'Val (16#7f#) then
         return 6; -- "Hex_XX"
      elsif Hi >= ' ' then
         return 3; -- "'X'" or "DEL"
      else
         declare
            Result : Natural := 0;
         begin
            for I in Lo .. Hi loop
               Result := Natural'Max (
                  Img_Char.Images_1f (I).all'Length,
                  Result);
            end loop;
            return Result; --  2 or 3
         end;
      end if;
   end Width_Character;

end System.Wid_Char;
