with System.UTF_Conversions;
package body Ada.Characters.Conversions is

   function Is_Character (Item : Wide_Character) return Boolean is
   begin
      return Wide_Character'Pos (Item) <= 16#7f#;
   end Is_Character;

   function Is_Character (Item : Wide_Wide_Character) return Boolean is
   begin
      return Wide_Wide_Character'Pos (Item) <= 16#7f#;
   end Is_Character;

   function Is_String (Item : Wide_String) return Boolean is
      pragma Unreferenced (Item);
   begin
      return True;
   end Is_String;

   function Is_String (Item : Wide_Wide_String) return Boolean is
      pragma Unreferenced (Item);
   begin
      return True;
   end Is_String;

   function Is_Wide_Character (Item : Wide_Wide_Character) return Boolean is
      subtype T is Wide_Wide_Character;
   begin
      case Item is
         when T'Val (16#0000#) .. T'Val (16#d7ff#)
            | T'Val (16#e000#) .. T'Val (16#ffff#) =>
            return True;
         when T'Val (16#d800#) .. T'Val (16#dfff#)
            | T'Val (16#10000#) .. T'Last =>
            return False;
      end case;
   end Is_Wide_Character;

   function Is_Wide_String (Item : Wide_Wide_String) return Boolean is
      pragma Unreferenced (Item);
   begin
      return True;
   end Is_Wide_String;

   function To_Character (
      Item : Wide_Character;
      Substitute : Character := ' ')
      return Character is
   begin
      if Is_Character (Item) then
         return Character'Val (Wide_Character'Pos (Item));
      else
         return Substitute;
      end if;
   end To_Character;

   function To_Character (
      Item : Wide_Wide_Character;
      Substitute : Character := ' ')
      return Character is
   begin
      if Is_Character (Item) then
         return Character'Val (Wide_Wide_Character'Pos (Item));
      else
         return Substitute;
      end if;
   end To_Character;

   function To_String (
      Item : Wide_String;
      Substitute : Character := ' ')
      return String
   is
      pragma Unreferenced (Substitute);
      Result : String (
         1 ..
         Item'Length * System.UTF_Conversions.UTF_8_Max_Length);
      Last : Natural;
      Error : Boolean;
   begin
      System.UTF_Conversions.UTF_16_To_UTF_8 (Item, Result, Last, Error);
      return Result (1 .. Last);
   end To_String;

   function To_String (
      Item : Wide_Wide_String;
      Substitute : Character := ' ')
      return String
   is
      pragma Unreferenced (Substitute);
   begin
      return Inside.To_String (Item);
   end To_String;

   function To_Wide_Character (Item : Character) return Wide_Character is
   begin
      return Wide_Character'Val (Character'Pos (Item));
   end To_Wide_Character;

   function To_Wide_Character (
      Item : Wide_Wide_Character;
      Substitute : Wide_Character := ' ')
      return Wide_Character is
   begin
      if Is_Wide_Character (Item) then
         return Wide_Character'Val (Wide_Wide_Character'Pos (Item));
      else
         return Substitute;
      end if;
   end To_Wide_Character;

   function To_Wide_String (Item : String) return Wide_String is
      Result : Wide_String (1 .. Item'Length);
      Last : Natural;
      Error : Boolean;
   begin
      System.UTF_Conversions.UTF_8_To_UTF_16 (Item, Result, Last, Error);
      return Result (1 .. Last);
   end To_Wide_String;

   function To_Wide_String (
      Item : Wide_Wide_String;
      Substitute : Wide_Character := ' ')
      return Wide_String
   is
      pragma Unreferenced (Substitute);
   begin
      return Inside.To_Wide_String (Item);
   end To_Wide_String;

   function To_Wide_Wide_Character (Item : Character)
      return Wide_Wide_Character is
   begin
      return Wide_Wide_Character'Val (Character'Pos (Item));
   end To_Wide_Wide_Character;

   function To_Wide_Wide_Character (Item : Wide_Character)
      return Wide_Wide_Character is
   begin
      return Wide_Wide_Character'Val (Wide_Character'Pos (Item));
   end To_Wide_Wide_Character;

end Ada.Characters.Conversions;
