pragma License (Unrestricted);
package Interfaces.C is
   pragma Pure;

   --  Declarations based on C's <limits.h>

   CHAR_BIT : constant := 8; -- typically 8
   SCHAR_MIN : constant := -128; -- typically -128
   SCHAR_MAX : constant := 127; -- typically 127
   UCHAR_MAX : constant := 255; -- typically 255

   --  Signed and Unsigned Integers

   type int is new Integer; -- implementation-defined
   type short is new Short_Integer; -- implementation-defined
   type long is new Long_Integer; -- implementation-defined

   type signed_char is range SCHAR_MIN .. SCHAR_MAX;
   for signed_char'Size use CHAR_BIT;

   type unsigned is mod 2 ** int'Size; -- implementation-defined
   type unsigned_short is mod 2 ** short'Size; -- implementation-defined
   type unsigned_long is mod 2 ** long'Size; -- implementation-defined

   type unsigned_char is mod UCHAR_MAX + 1;
   for unsigned_char'Size use CHAR_BIT;

   subtype plain_char is unsigned_char; -- implementation-defined

   type ptrdiff_t is range
      -(2 ** (Standard'Address_Size - 1)) ..
      +(2 ** (Standard'Address_Size - 1) - 1); -- implementation-defined

   type size_t is mod 2 ** Standard'Address_Size; -- implementation-defined

   --  Floating Point

   type C_float is new Standard.Float; -- implementation-defined
   type double is new Standard.Long_Float; -- implementation-defined
   type long_double is new Standard.Long_Long_Float; -- implementation-defined

   --  Characters and Strings

   type char is new Character; -- implementation-defined character type

   nul : constant char := char'Val (0); -- implementation-defined

   function To_C (Item : Character) return char;
   function To_Ada (Item : char) return Character;

   type char_array is array (size_t range <>) of aliased char;
   for char_array'Component_Size use CHAR_BIT;

   function Is_Nul_Terminated (Item : char_array) return Boolean;

   --  extended
   function Length (Item : char_array) return size_t;

   function To_C (Item : String; Append_Nul : Boolean := True)
      return char_array;
   function To_Ada (Item : char_array; Trim_Nul : Boolean := True)
      return String;

   procedure To_C (
      Item : String;
      Target : out char_array;
      Count : out size_t;
      Append_Nul : Boolean := True);
   procedure To_Ada (
      Item : char_array;
      Target : out String;
      Count : out Natural;
      Trim_Nul : Boolean := True);

   --  Wide Character and Wide String

   type wchar_t is
      new Wide_Wide_Character; -- implementation-defined character type
   pragma Compile_Time_Error (
      wchar_t'Size /= Standard'Wchar_T_Size,
      "bad size of wchar_t");

   wide_nul : constant wchar_t := wchar_t'Val (0); -- implementation-defined

   function To_C (Item : Wide_Character) return wchar_t;
   function To_Ada (Item : wchar_t) return Wide_Character;

   type wchar_array is array (size_t range <>) of aliased wchar_t;
   pragma Pack (wchar_array);

   function Is_Nul_Terminated (Item : wchar_array) return Boolean;

   --  extended
   function Length (Item : wchar_array) return size_t;

   --  extended
   function To_C (
      Item : Wide_String;
      Append_Nul : Boolean;
      Substitute : wchar_t)
      return wchar_array;

   function To_C (Item : Wide_String; Append_Nul : Boolean := True)
      return wchar_array;

   --  extended
   function To_Ada (
      Item : wchar_array;
      Trim_Nul : Boolean;
      Substitute : Wide_Character)
      return Wide_String;

   function To_Ada (Item : wchar_array; Trim_Nul : Boolean := True)
      return Wide_String;

   --  modified
--  procedure To_C (
--    Item : Wide_String;
--    Target : out wchar_array;
--    Count : out size_t;
--    Append_Nul : Boolean := True);
   procedure To_C (
      Item : Wide_String;
      Target : out wchar_array;
      Count : out size_t;
      Append_Nul : Boolean := True;
      Substitute : wchar_t := '?');
--  procedure To_Ada (
--    Item : wchar_array;
--    Target : out Wide_String;
--    Count : out Natural;
--    Trim_Nul : Boolean := True);
   procedure To_Ada (
      Item : wchar_array;
      Target : out Wide_String;
      Count : out Natural;
      Trim_Nul : Boolean := True;
      Substitute : Wide_Character := '?');

   --  extended
   function To_C (Item : Wide_Wide_String; Append_Nul : Boolean := True)
      return wchar_array;
   function To_Ada (Item : wchar_array; Trim_Nul : Boolean := True)
      return Wide_Wide_String;

   --  ISO/IEC 10646:2003 compatible types defined by ISO/IEC TR 19769:2004.

   type char16_t is
      new Wide_Character; -- implementation-defined character type

   char16_nul : constant char16_t :=
      char16_t'Val (0); -- implementation-defined

   function To_C (Item : Wide_Character) return char16_t;
   function To_Ada (Item : char16_t) return Wide_Character;

   type char16_array is array (size_t range <>) of aliased char16_t;
   pragma Pack (char16_array);

   function Is_Nul_Terminated (Item : char16_array) return Boolean;

   function To_C (Item : Wide_String; Append_Nul : Boolean := True)
      return char16_array;
   function To_Ada (Item : char16_array; Trim_Nul : Boolean := True)
      return Wide_String;

   procedure To_C (
      Item : Wide_String;
      Target : out char16_array;
      Count : out size_t;
      Append_Nul : Boolean := True);
   procedure To_Ada (
      Item : char16_array;
      Target : out Wide_String;
      Count : out Natural;
      Trim_Nul : Boolean := True);

   type char32_t is
      new Wide_Wide_Character; -- implementation-defined character type

   char32_nul : constant char32_t :=
      char32_t'Val (0); -- implementation-defined

   function To_C (Item : Wide_Wide_Character) return char32_t;
   function To_Ada (Item : char32_t) return Wide_Wide_Character;

   type char32_array is array (size_t range <>) of aliased char32_t;
   pragma Pack (char32_array);

   function Is_Nul_Terminated (Item : char32_array) return Boolean;

   function To_C (Item : Wide_Wide_String; Append_Nul : Boolean := True)
      return char32_array;
   function To_Ada (Item : char32_array; Trim_Nul : Boolean := True)
      return Wide_Wide_String;

   procedure To_C (
      Item : Wide_Wide_String;
      Target : out char32_array;
      Count : out size_t;
      Append_Nul : Boolean := True);
   procedure To_Ada (
      Item : char32_array;
      Target : out Wide_Wide_String;
      Count : out Natural;
      Trim_Nul : Boolean := True);

   pragma Inline (To_C);
   pragma Inline (To_Ada);
   pragma Inline (Is_Nul_Terminated);

   Terminator_Error : exception;

   --  extended
   --  Common to instances of Interfaces.C.Pointers.
   Pointer_Error : exception;

   --  extended
   --  Common to instances of Interfaces.C.Generic_Strings.
   Dereference_Error : exception;
   Update_Error : exception;

end Interfaces.C;
