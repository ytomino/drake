package body Ada.Characters.Inside is

   function To_Character (Item : Wide_Wide_Character)
      return Character is
   begin
      if Wide_Wide_Character'Pos (Item) >= 16#80# then
         raise Constraint_Error;
      else
         return Character'Val (Wide_Wide_Character'Pos (Item));
      end if;
   end To_Character;

   function To_Wide_Wide_Character (Item : Character)
      return Wide_Wide_Character is
   begin
      if Character'Pos (Item) >= 16#80# then
         raise Constraint_Error;
      else
         return Wide_Wide_Character'Val (Character'Pos (Item));
      end if;
   end To_Wide_Wide_Character;

   function To_Wide_Character (Item : Wide_Wide_Character)
      return Wide_Character is
   begin
      if Wide_Wide_Character'Pos (Item) >= 16#ffff# then
         raise Constraint_Error;
      else
         return Wide_Character'Val (Wide_Wide_Character'Pos (Item));
      end if;
   end To_Wide_Character;

   function To_Wide_Wide_Character (Item : Wide_Character)
      return Wide_Wide_Character is
   begin
      case Wide_Character'Pos (Item) is
         when 16#d800# .. 16#dfff# =>
            raise Constraint_Error;
         when others =>
            return Wide_Wide_Character'Val (Wide_Character'Pos (Item));
      end case;
   end To_Wide_Wide_Character;

end Ada.Characters.Inside;
