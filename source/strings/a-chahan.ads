pragma License (Unrestricted);
with Ada.Characters.Conversions;
package Ada.Characters.Handling is
--  pragma Pure;
   pragma Preelaborate; --  use mapping

   --  Character classification functions

--  function Is_Control (Item : Character) return Boolean;
--  function Is_Graphic (Item : Character) return Boolean;
--  function Is_Letter (Item : Character) return Boolean;
--  function Is_Lower (Item : Character) return Boolean;
--  function Is_Upper (Item : Character) return Boolean;
--  function Is_Basic (Item : Character) return Boolean;
--  function Is_Digit (Item : Character) return Boolean;
--  function Is_Decimal_Digit (Item : Character) return Boolean
--    renames Is_Digit;
--  function Is_Hexadecimal_Digit (Item : Character) return Boolean;
--  function Is_Alphanumeric (Item : Character) return Boolean;
--  function Is_Special (Item : Character) return Boolean;

   --  Conversion functions for Character and String

   function To_Lower (Item : Character) return Character;
   function To_Upper (Item : Character) return Character;
--  function To_Basic (Item : Character) return Character;

   function To_Lower (Item : String) return String;
   function To_Upper (Item : String) return String;
--  function To_Basic (Item : String) return String;

   --  Classifications of and conversions between Character and ISO 646

   subtype ISO_646 is Character range Character'Val (0) .. Character'Val (127);

--  function Is_ISO_646 (Item : Character) return Boolean;
--  function Is_ISO_646 (Item : String) return Boolean;

--  function To_ISO_646 (Item : Character; Substitute : ISO_646 := ' ')
--    return ISO_646;

--  function To_ISO_646 (Item : String; Substitute : ISO_646 := ' ')
--    return String;

   --  The functions Is_Character, Is_String, To_Character, To_String,
   --  To_Wide_Character, and To_Wide_String are obsolescent; see J.14.

   function Is_Character (Item : Wide_Character) return Boolean
      renames Conversions.Is_Character;
   function Is_String (Item : Wide_String) return Boolean
      renames Conversions.Is_String;

   function To_Character (
      Item : Wide_Character;
      Substitute : Character := ' ')
      return Character
      renames Conversions.To_Character;

   function To_String (
      Item : Wide_String;
      Substitute : Character := ' ')
      return String
      renames Conversions.To_String;

   function To_Wide_Character (Item : Character) return Wide_Character
      renames Conversions.To_Wide_Character;

   function To_Wide_String (Item : String) return Wide_String
      renames Conversions.To_Wide_String;

end Ada.Characters.Handling;
