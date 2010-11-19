package body Ada.Characters.ASCII.Handling is

   function To_Upper (Item : Character) return Character is
   begin
      if Item in 'a' .. 'z' then
         return Character'Val (Character'Pos (Item) - 16#20#);
      else
         return Item;
      end if;
   end To_Upper;

end Ada.Characters.ASCII.Handling;
