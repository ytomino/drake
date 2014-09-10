pragma License (Unrestricted);
private with System.UTF_Conversions.From_8_To_16;
private with System.UTF_Conversions.From_8_To_32;
private with System.UTF_Conversions.From_16_To_32;
private with System.UTF_Conversions.From_16_To_8;
private with System.UTF_Conversions.From_32_To_8;
private with System.UTF_Conversions.From_32_To_16;
package Ada.Characters.Conversions is
   pragma Pure;

   --  extended
   --  This function returns False if Item is in UTF-8 multibyte sequence,
   --    otherwise True.
   function Is_Wide_Character (Item : Character) return Boolean;

   --  extended
   --  This function returns False if Item is not in BMP or surrogate pair,
   --    otherwise True.
   --  These functions Is_XXX_String assume Item contains a legal sequence.
   function Is_Wide_String (Item : String) return Boolean;

   --  extended
   --  This function returns False if Item is in UTF-8 multibyte sequence,
   --    otherwise True.
   function Is_Wide_Wide_Character (Item : Character) return Boolean
      renames Is_Wide_Character;

--  function Is_Wide_Wide_String (Item : String) return Boolean; -- True

   function Is_Character (Item : Wide_Character) return Boolean;
   function Is_String (Item : Wide_String) return Boolean; -- True

   --  extended
   --  This function returns False if Item is in UTF-16 surrogate pair,
   --    otherwise True.
   function Is_Wide_Wide_Character (Item : Wide_Character) return Boolean;

--  function Is_Wide_Wide_String (Item : Wide_String) return Boolean; -- True

   function Is_Character (Item : Wide_Wide_Character) return Boolean;
   function Is_String (Item : Wide_Wide_String) return Boolean; -- True

   function Is_Wide_Character (Item : Wide_Wide_Character) return Boolean;
   function Is_Wide_String (Item : Wide_Wide_String) return Boolean;

   pragma Inline (Is_Character);
   pragma Inline (Is_Wide_Character);
   pragma Inline (Is_Wide_Wide_Character);
   pragma Inline (Is_String);

   --  Is_Character return False when 16#7F# .. 16#FF# and greater
   --  Is_Wide_Character return False when surrogate pair and greater
   --  Is_String, Is_Wide_String return True always

   --  modified
   --  These functions use Substitute if Item contains illegal byte sequence.
   function To_Wide_Character (
      Item : Character;
      Substitute : Wide_Character := ' ') -- additional
      return Wide_Character;
   function To_Wide_String (
      Item : String;
      Substitute : Wide_Character := ' ') -- additional
      return Wide_String;

   --  modified
   function To_Wide_Wide_Character (
      Item : Character;
      Substitute : Wide_Wide_Character := ' ') -- additional
      return Wide_Wide_Character;
   function To_Wide_Wide_String (
      Item : String;
      Substitute : Wide_Wide_Character := ' ') -- additional
      return Wide_Wide_String;

   --  modified
   function To_Wide_Wide_Character (
      Item : Wide_Character;
      Substitute : Wide_Wide_Character := ' ') -- additional
      return Wide_Wide_Character;
   function To_Wide_Wide_String (
      Item : Wide_String;
      Substitute : Wide_Wide_Character := ' ') -- additional
      return Wide_Wide_String;

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

   pragma Inline (To_String); -- renamed
   pragma Inline (To_Wide_String); -- renamed
   pragma Inline (To_Wide_Wide_String); -- renamed

   --  extended
   --  There are subprograms for code-point based decoding iteration.
   procedure Get (
      Item : String;
      Last : out Natural;
      Value : out Wide_Wide_Character;
      Substitute : Wide_Wide_Character := ' ');
   procedure Get (
      Item : String;
      Last : out Natural;
      Value : out Wide_Wide_Character;
      Is_Illegal_Sequence : out Boolean);
   procedure Get_Reverse (
      Item : String;
      First : out Positive;
      Value : out Wide_Wide_Character;
      Substitute : Wide_Wide_Character := ' ');
   procedure Get_Reverse (
      Item : String;
      First : out Positive;
      Value : out Wide_Wide_Character;
      Is_Illegal_Sequence : out Boolean);
   procedure Get (
      Item : Wide_String;
      Last : out Natural;
      Value : out Wide_Wide_Character;
      Substitute : Wide_Wide_Character := ' ');
   procedure Get (
      Item : Wide_String;
      Last : out Natural;
      Value : out Wide_Wide_Character;
      Is_Illegal_Sequence : out Boolean);
   procedure Get_Reverse (
      Item : Wide_String;
      First : out Positive;
      Value : out Wide_Wide_Character;
      Substitute : Wide_Wide_Character := ' ');
   procedure Get_Reverse (
      Item : Wide_String;
      First : out Positive;
      Value : out Wide_Wide_Character;
      Is_Illegal_Sequence : out Boolean);
   procedure Get (
      Item : Wide_Wide_String;
      Last : out Natural;
      Value : out Wide_Wide_Character;
      Substitute : Wide_Wide_Character := ' ');
   procedure Get (
      Item : Wide_Wide_String;
      Last : out Natural;
      Value : out Wide_Wide_Character;
      Is_Illegal_Sequence : out Boolean);
   procedure Get_Reverse (
      Item : Wide_Wide_String;
      First : out Positive;
      Value : out Wide_Wide_Character;
      Substitute : Wide_Wide_Character := ' ');
   procedure Get_Reverse (
      Item : Wide_Wide_String;
      First : out Positive;
      Value : out Wide_Wide_Character;
      Is_Illegal_Sequence : out Boolean);

   --  extended
   --  There are encoding subprograms.
   procedure Put (
      Value : Wide_Wide_Character;
      Item : out String;
      Last : out Natural);
   procedure Put (
      Value : Wide_Wide_Character;
      Item : out Wide_String;
      Last : out Natural);
   procedure Put (
      Value : Wide_Wide_Character;
      Item : out Wide_Wide_String;
      Last : out Natural);

   --  extended
   --  Max lengthes of each one multi-byte character,
   --    and the rates of expansion.
   Max_Length_In_String : constant := 6;
   Max_Length_In_Wide_String : constant := 2;
   Max_Length_In_Wide_Wide_String : constant := 1;
   Expanding_From_String_To_Wide_String : constant := 1;
   Expanding_From_String_To_Wide_Wide_String : constant := 1;
   Expanding_From_Wide_String_To_String : constant := 3;
   Expanding_From_Wide_String_To_Wide_Wide_String : constant := 1;
   Expanding_From_Wide_Wide_String_To_String : constant := 6;
   Expanding_From_Wide_Wide_String_To_Wide_String : constant := 2;
   Expanding_From_String_To_UTF_8 : constant := 1;
   Expanding_From_String_To_UTF_16 : constant := 1;
   Expanding_From_String_To_UTF_32 : constant := 1;
   Expanding_From_Wide_String_To_UTF_8 : constant := 3;
   Expanding_From_Wide_String_To_UTF_16 : constant := 1;
   Expanding_From_Wide_String_To_UTF_32 : constant := 1;
   Expanding_From_Wide_Wide_String_To_UTF_8 : constant := 6;
   Expanding_From_Wide_Wide_String_To_UTF_16 : constant := 2;
   Expanding_From_Wide_Wide_String_To_UTF_32 : constant := 1;
   Expanding_From_UTF_8_To_String : constant := 1;
   Expanding_From_UTF_8_To_Wide_String : constant := 1;
   Expanding_From_UTF_8_To_Wide_Wide_String : constant := 1;
   Expanding_From_UTF_16_To_String : constant := 3;
   Expanding_From_UTF_16_To_Wide_String : constant := 1;
   Expanding_From_UTF_16_To_Wide_Wide_String : constant := 1;
   Expanding_From_UTF_32_To_String : constant := 6;
   Expanding_From_UTF_32_To_Wide_String : constant := 2;
   Expanding_From_UTF_32_To_Wide_Wide_String : constant := 1;

private

   function To_Wide_String (
      Item : String;
      Substitute : Wide_Character := ' ')
      return Wide_String
      renames System.UTF_Conversions.From_8_To_16.Convert;
   function To_Wide_Wide_String (
      Item : String;
      Substitute : Wide_Wide_Character := ' ')
      return Wide_Wide_String
      renames System.UTF_Conversions.From_8_To_32.Convert;
   function To_Wide_Wide_String (
      Item : Wide_String;
      Substitute : Wide_Wide_Character := ' ')
      return Wide_Wide_String
      renames System.UTF_Conversions.From_16_To_32.Convert;
   function To_String (
      Item : Wide_String;
      Substitute : Character := ' ')
      return String
      renames System.UTF_Conversions.From_16_To_8.Convert;
   function To_String (
      Item : Wide_Wide_String;
      Substitute : Character := ' ')
      return String
      renames System.UTF_Conversions.From_32_To_8.Convert;
   function To_Wide_String (
      Item : Wide_Wide_String;
      Substitute : Wide_Character := ' ')
      return Wide_String
      renames System.UTF_Conversions.From_32_To_16.Convert;

end Ada.Characters.Conversions;
