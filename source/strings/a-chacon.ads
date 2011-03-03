pragma License (Unrestricted);
private with Ada.Characters.Inside;
package Ada.Characters.Conversions is
   pragma Pure;

   function Is_Character (Item : Wide_Character) return Boolean;
   function Is_String (Item : Wide_String) return Boolean;
   pragma Inline (Is_String);
   function Is_Character (Item : Wide_Wide_Character) return Boolean;
   function Is_String (Item : Wide_Wide_String) return Boolean;
   pragma Inline (Is_String);
   function Is_Wide_Character (Item : Wide_Wide_Character) return Boolean;
   function Is_Wide_String (Item : Wide_Wide_String) return Boolean;
   pragma Inline (Is_Wide_String);

   --  extended
   --  Is_Character return False when 16#7F# .. 16#FF#
   --  Is_Wide_Character return False when surrogate pair

   function To_Wide_Character (Item : Character) return Wide_Character;
   function To_Wide_String (Item : String) return Wide_String;
   function To_Wide_Wide_Character (Item : Character)
      return Wide_Wide_Character;
   function To_Wide_Wide_String (Item : String) return Wide_Wide_String;
   pragma Inline_Always (To_Wide_Wide_String);
   function To_Wide_Wide_Character (Item : Wide_Character)
      return Wide_Wide_Character;
   function To_Wide_Wide_String (Item : Wide_String) return Wide_Wide_String;
   pragma Inline_Always (To_Wide_Wide_String);

   function To_Character (
      Item : Wide_Character;
      Substitute : Character := ' ')
      return Character;
   function To_String (
      Item : Wide_String;
      Substitute : Character := ' ')
      return String;
   pragma Inline (To_String);
   function To_Character (
      Item : Wide_Wide_Character;
      Substitute : Character := ' ')
      return Character;
   function To_String (
      Item : Wide_Wide_String;
      Substitute : Character := ' ')
      return String;
   pragma Inline (To_String);
   function To_Wide_Character (
      Item : Wide_Wide_Character;
      Substitute : Wide_Character := ' ')
      return Wide_Character;
   function To_Wide_String (
      Item : Wide_Wide_String;
      Substitute : Wide_Character := ' ')
      return Wide_String;
   pragma Inline (To_Wide_String);

private

   function To_Wide_Wide_String (Item : String) return Wide_Wide_String
      renames Inside.To_Wide_Wide_String;
   function To_Wide_Wide_String (Item : Wide_String) return Wide_Wide_String
      renames Inside.To_Wide_Wide_String;

end Ada.Characters.Conversions;
