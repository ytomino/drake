pragma License (Unrestricted);
--  implementation unit specialized for Windows
with C;
package System.C_Encoding is
   pragma Preelaborate;

   --  Character (UTF-8) from/to char (MBCS)

   function To_char (
      Item : Character;
      Substitute : C.char)
      return C.char;

   function To_Character (
      Item : C.char;
      Substitute : Character)
      return Character;

   procedure To_Non_Nul_Terminated (
      Item : String;
      Target : out C.char_array;
      Count : out C.size_t;
      Substitute : C.char_array);

   procedure From_Non_Nul_Terminated (
      Item : C.char_array;
      Target : out String;
      Count : out Natural;
      Substitute : String); -- unreferenced

   Expanding_To_char : constant := 1;
   Expanding_To_Character : constant := 3; -- halfwidth kana

   --  Wide_Character (UTF-16) from/to wchar_t (UTF-16)

   function To_wchar_t (
      Item : Wide_Character;
      Substitute : C.wchar_t) -- unreferenced
      return C.wchar_t;

   function To_Wide_Character (
      Item : C.wchar_t;
      Substitute : Wide_Character) -- unreferenced
      return Wide_Character;

   procedure To_Non_Nul_Terminated (
      Item : Wide_String;
      Target : out C.wchar_t_array;
      Count : out C.size_t;
      Substitute : C.wchar_t_array); -- unreferenced

   procedure From_Non_Nul_Terminated (
      Item : C.wchar_t_array;
      Target : out Wide_String;
      Count : out Natural;
      Substitute : Wide_String); -- unreferenced

   Expanding_From_Wide_To_wchar_t : constant := 1;
   Expanding_From_wchar_t_To_Wide : constant := 1;

   --  Wide_Wide_Character (UTF-32) from/to wchar_t (UTF-16)

   function To_wchar_t (
      Item : Wide_Wide_Character;
      Substitute : C.wchar_t)
      return C.wchar_t;

   function To_Wide_Wide_Character (
      Item : C.wchar_t;
      Substitute : Wide_Wide_Character)
      return Wide_Wide_Character;

   procedure To_Non_Nul_Terminated (
      Item : Wide_Wide_String;
      Target : out C.wchar_t_array;
      Count : out C.size_t;
      Substitute : C.wchar_t_array);

   procedure From_Non_Nul_Terminated (
      Item : C.wchar_t_array;
      Target : out Wide_Wide_String;
      Count : out Natural;
      Substitute : Wide_Wide_String);

   Expanding_From_Wide_Wide_To_wchar_t : constant :=
      2; -- Expanding_From_32_To_16
   Expanding_From_wchar_t_To_Wide_Wide : constant :=
      1; -- Expanding_From_16_To_32

end System.C_Encoding;
