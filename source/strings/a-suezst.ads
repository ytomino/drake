pragma License (Unrestricted);
--  Ada 2012
with Ada.Characters.Conversions;
with Ada.Strings.UTF_Encoding.Generic_Strings;
package Ada.Strings.UTF_Encoding.Wide_Wide_Strings is
   new Ada.Strings.UTF_Encoding.Generic_Strings (
      Wide_Wide_Character,
      Wide_Wide_String,
      Expanding_From_8 => 1, -- Expanding_From_8_To_32
      Expanding_From_16 => 1, -- Expanding_From_16_To_32
      Expanding_From_32 => 1,
      Expanding_To_8 => 6, -- Expanding_From_32_To_8
      Expanding_To_16 => 2, -- Expanding_From_32_To_16
      Expanding_To_32 => 1,
      Get => Ada.Characters.Conversions.Get,
      Put => Ada.Characters.Conversions.Put);
pragma Pure (Ada.Strings.UTF_Encoding.Wide_Wide_Strings);
