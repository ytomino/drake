pragma License (Unrestricted);
--  Ada 2012
with Ada.Characters.Handling;
private with Ada.UCD;
package Ada.Wide_Characters.Handling is
--  pragma Pure;
   pragma Preelaborate;

--  function Character_Set_Version return String;
   Character_Set_Version : constant String;

   function Is_Control (Item : Wide_Character) return Boolean
      renames Characters.Handling.Overloaded_Is_Control;

   function Is_Letter (Item : Wide_Character) return Boolean
      renames Characters.Handling.Overloaded_Is_Letter;

   function Is_Lower (Item : Wide_Character) return Boolean
      renames Characters.Handling.Overloaded_Is_Lower;

   function Is_Upper (Item : Wide_Character) return Boolean
      renames Characters.Handling.Overloaded_Is_Upper;

   function Is_Basic (Item : Wide_Character) return Boolean
      renames Characters.Handling.Overloaded_Is_Basic;

   --  Note: Wide_Characters.Handling.Is_Basic is incompatible with
   --    Characters.Handling.Is_Basic. See AI12-0260-1.

   function Is_Digit (Item : Wide_Character) return Boolean
      renames Characters.Handling.Overloaded_Is_Digit;

   function Is_Decimal_Digit (Item : Wide_Character) return Boolean
      renames Is_Digit;

   function Is_Hexadecimal_Digit (Item : Wide_Character) return Boolean
      renames Characters.Handling.Overloaded_Is_Hexadecimal_Digit;

   function Is_Alphanumeric (Item : Wide_Character) return Boolean
      renames Characters.Handling.Overloaded_Is_Alphanumeric;

   function Is_Special (Item : Wide_Character) return Boolean
      renames Characters.Handling.Overloaded_Is_Special;

--  function Is_Line_Terminator (Item : Wide_Character) return Boolean;

--  function Is_Mark (Item : Wide_Character) return Boolean;

--  function Is_Other_Format (Item : Wide_Character) return Boolean;

--  function Is_Punctuation_Connector (Item : Wide_Character) return Boolean;

--  function Is_Space (Item : Wide_Character) return Boolean;

   function Is_Graphic (Item : Wide_Character) return Boolean
      renames Characters.Handling.Overloaded_Is_Graphic;

   function To_Lower (Item : Wide_Character) return Wide_Character
      renames Characters.Handling.Overloaded_To_Lower;
   function To_Upper (Item : Wide_Character) return Wide_Character
      renames Characters.Handling.Overloaded_To_Upper;

   function To_Basic (Item : Wide_Character) return Wide_Character
      renames Characters.Handling.Overloaded_To_Basic;

   function To_Lower (Item : Wide_String) return Wide_String
      renames Characters.Handling.Overloaded_To_Lower;
   function To_Upper (Item : Wide_String) return Wide_String
      renames Characters.Handling.Overloaded_To_Upper;

   function To_Basic (Item : Wide_String) return Wide_String
      renames Characters.Handling.Overloaded_To_Basic;

private

   Character_Set_Version : constant String := UCD.Version;

end Ada.Wide_Characters.Handling;
