pragma License (Unrestricted);
package Ada.Characters.Conversions is
   pragma Pure;

   function Is_Character (Item : Wide_Character) return Boolean;
   function Is_String (Item : Wide_String) return Boolean;
   function Is_Character (Item : Wide_Wide_Character) return Boolean;
   function Is_String (Item : Wide_Wide_String) return Boolean;
   function Is_Wide_Character (Item : Wide_Wide_Character) return Boolean;
   function Is_Wide_String (Item : Wide_Wide_String) return Boolean;

   function To_Wide_Character (Item : Character) return Wide_Character;
   function To_Wide_String (Item : String) return Wide_String;
   function To_Wide_Wide_Character (Item : Character)
      return Wide_Wide_Character;
   function To_Wide_Wide_String (Item : String) return Wide_Wide_String;
   function To_Wide_Wide_Character (Item : Wide_Character)
      return Wide_Wide_Character;
   function To_Wide_Wide_String (Item : Wide_String) return Wide_Wide_String;

   function To_Character (
      Item : Wide_Character;
      Substitute : Character := ' ')
      return Character;
   function To_String (
      Item : Wide_String;
      Substitute : Character := ' ')
      return String;
   function To_Character (
      Item : Wide_Wide_Character;
      Substitute : Character := ' ')
      return Character;
   function To_String (
      Item : Wide_Wide_String;
      Substitute : Character := ' ')
      return String;
   function To_Wide_Character (
      Item : Wide_Wide_Character;
      Substitute : Wide_Character := ' ')
      return Wide_Character;
   function To_Wide_String (
      Item : Wide_Wide_String;
      Substitute : Wide_Character := ' ')
      return Wide_String;

end Ada.Characters.Conversions;
