with System.Img_Char;
package body System.Wid_Char is

   function Width_Character (Lo, Hi : Character) return Natural is
   begin
      if Lo > Hi then
         return 0;
      elsif Hi >= Character'Val (16#80#) then
         return 6; -- "Hex_XX"
      elsif Hi >= ' ' then -- including 7F
         return 3; -- "'X'" or "DEL"
      else -- 2 or 3
         for I in Lo .. Hi loop
            if Img_Char.Length (Img_Char.Image_00_1F (I)) = 3 then
               return 3;
            end if;
         end loop;
         return 2;
      end if;
   end Width_Character;

end System.Wid_Char;
