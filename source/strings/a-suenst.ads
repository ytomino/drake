pragma License (Unrestricted);
--  Ada 2012
with Ada.Characters.Conversions;
with Ada.Strings.UTF_Encoding.Generic_Strings;
package Ada.Strings.UTF_Encoding.Strings is
   new Ada.Strings.UTF_Encoding.Generic_Strings (
      Character,
      String,
      Expanding_From_8 =>
         Characters.Conversions.Expanding_From_UTF_8_To_String,
      Expanding_From_16 =>
         Characters.Conversions.Expanding_From_UTF_16_To_String,
      Expanding_From_32 =>
         Characters.Conversions.Expanding_From_UTF_32_To_String,
      Expanding_To_8 =>
         Characters.Conversions.Expanding_From_String_To_UTF_8,
      Expanding_To_16 =>
         Characters.Conversions.Expanding_From_String_To_UTF_16,
      Expanding_To_32 =>
         Characters.Conversions.Expanding_From_String_To_UTF_32,
      Get => Ada.Characters.Conversions.Get,
      Put => Ada.Characters.Conversions.Put);
pragma Pure (Ada.Strings.UTF_Encoding.Strings);
