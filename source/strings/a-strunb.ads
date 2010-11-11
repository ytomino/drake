pragma License (Unrestricted);
with Ada.Strings.Generic_Unbounded;
--  with Ada.Strings.Maps;
package Ada.Strings.Unbounded is
   pragma Preelaborate;

   --  if this package instantiate Generic_Unounded directly,
   --  Unounded.Hash can not have body...
   package Instance is new Generic_Unbounded (Character, String);

--  type Unbounded_String is private;
--  pragma Preelaborable_Initialization (Unbounded_String);
   subtype Unbounded_String is Instance.Unbounded_String;

--  Null_Unbounded_String : constant Unbounded_String;
   function Null_Unbounded_String return Unbounded_String
      renames Instance.Null_Unbounded_String;

   function Length (Source : Unbounded_String) return Natural
      renames Instance.Length;

--  type String_Access is access all String;
   subtype String_Access is Instance.String_Access;
   procedure Free (X : in out String_Access)
      renames Instance.Free;

   --  Conversion, Concatenation, and Selection functions

   function To_Unbounded_String (Source : String)
      return Unbounded_String
      renames Instance.To_Unbounded_String;

--  function To_Unbounded_String (Length : Natural)
--    return Unbounded_String;

   function To_String (Source : Unbounded_String) return String
      renames Instance.To_String;

--  procedure Set_Unbounded_String (
--    Target : out Unbounded_String;
--    Source : String);

   procedure Append (
      Source : in out Unbounded_String;
      New_Item : Unbounded_String)
      renames Instance.Append;

   procedure Append (
      Source : in out Unbounded_String;
      New_Item : String)
      renames Instance.Append;

   procedure Append (
      Source : in out Unbounded_String;
      New_Item : Character)
      renames Instance.Append;

   function "&" (Left, Right : Unbounded_String) return Unbounded_String
      renames Instance."&";

   function "&" (Left : Unbounded_String; Right : String)
      return Unbounded_String
      renames Instance."&";

   function "&" (Left : String; Right : Unbounded_String)
      return Unbounded_String
      renames Instance."&";

   function "&" (Left : Unbounded_String; Right : Character)
      return Unbounded_String
      renames Instance."&";

   function "&" (Left : Character; Right : Unbounded_String)
      return Unbounded_String
      renames Instance."&";

--  function Element (Source : Unbounded_String; Index : Positive)
--    return Character;

--  procedure Replace_Element (
--    Source : in out Unbounded_String;
--    Index : Positive;
--    By : Character);

   function Slice (
      Source : Unbounded_String;
      Low : Positive;
      High : Natural)
      return String
      renames Instance.Slice;

--  function Unbounded_Slice (
--    Source : Unbounded_String;
--    Low : Positive;
--    High : Natural)
--    return Unbounded_String;

--  procedure Unbounded_Slice (
--    Source : Unbounded_String;
--    Target : out Unbounded_String;
--    Low : Positive;
--    High : Natural);

   function "=" (Left, Right : Unbounded_String) return Boolean
      renames Instance."=";

   function "=" (Left : Unbounded_String; Right : String) return Boolean
      renames Instance."=";

   function "=" (Left : String; Right : Unbounded_String) return Boolean
      renames Instance."=";

   function "<" (Left, Right : Unbounded_String) return Boolean
      renames Instance."<";

--  function "<" (Left : Unbounded_String; Right : String) return Boolean;

--  function "<" (Left : String; Right : Unbounded_String) return Boolean;

--  function "<=" (Left, Right : Unbounded_String) return Boolean;

--  function "<=" (Left : Unbounded_String; Right : String) return Boolean;

--  function "<=" (Left : String; Right : Unbounded_String) return Boolean;

   function ">" (Left, Right : Unbounded_String) return Boolean
      renames Instance.">";

--  function ">" (Left : Unbounded_String; Right : String) return Boolean;

--  function ">" (Left : String; Right : Unbounded_String) return Boolean;

--  function ">=" (Left, Right : Unbounded_String) return Boolean;

--  function ">=" (Left : Unbounded_String; Right : String) return Boolean;

--  function ">=" (Left : String; Right : Unbounded_String) return Boolean;

   --  Search subprograms

--  function Index (
--    Source : Unbounded_String;
--    Pattern : String;
--    From : Positive;
--    Going : Direction := Forward;
--    Mapping : Maps.Character_Mapping := Maps.Identity)
--    return Natural;

--  function Index (
--    Source : Unbounded_String;
--    Pattern : String;
--    From : Positive;
--    Going : Direction := Forward;
--    Mapping : Maps.Character_Mapping_Function)
--    return Natural;

--  function Index (
--    Source : Unbounded_String;
--    Pattern : String;
--    Going : Direction := Forward;
--    Mapping : Maps.Character_Mapping := Maps.Identity)
--    return Natural;

--  function Index (
--    Source : Unbounded_String;
--    Pattern : String;
--    Going : Direction := Forward;
--    Mapping : Maps.Character_Mapping_Function)
--    return Natural;

--  function Index (
--    Source : Unbounded_String;
--    Set : Maps.Character_Set;
--    From : Positive;
--    Test : Membership := Inside;
--    Going : Direction := Forward)
--    return Natural;

--  function Index (
--    Source : Unbounded_String;
--    Set : Maps.Character_Set;
--    Test : Membership := Inside;
--    Going : Direction  := Forward)
--    return Natural;

--  function Index_Non_Blank (
--    Source : Unbounded_String;
--    From : Positive;
--    Going : Direction := Forward)
--    return Natural;

--  function Index_Non_Blank (
--    Source : Unbounded_String;
--    Going : Direction := Forward)
--    return Natural;

--  function Count (
--    Source : Unbounded_String;
--    Pattern : String;
--    Mapping : Maps.Character_Mapping := Maps.Identity)
--    return Natural;

--  function Count (
--    Source : Unbounded_String;
--    Pattern : String;
--    Mapping : Maps.Character_Mapping_Function)
--    return Natural;

--  function Count (
--    Source : Unbounded_String;
--    Set : Maps.Character_Set)
--    return Natural;

--  procedure Find_Token (
--    Source : Unbounded_String;
--    Set : Maps.Character_Set;
--    From : Positive;
--    Test : Membership;
--    First : out Positive;
--    Last : out Natural);

--  procedure Find_Token (
--    Source : Unbounded_String;
--    Set : Maps.Character_Set;
--    Test : Membership;
--    First : out Positive;
--    Last : out Natural);

   --  String translation subprograms

--  function Translate (
--    Source : Unbounded_String;
--    Mapping : Maps.Character_Mapping)
--    return Unbounded_String;

--  procedure Translate (
--    Source : in out Unbounded_String;
--    Mapping : Maps.Character_Mapping);

--  function Translate (
--    Source : Unbounded_String;
--    Mapping : Maps.Character_Mapping_Function)
--    return Unbounded_String;

--  procedure Translate (
--    Source : in out Unbounded_String;
--    Mapping : Maps.Character_Mapping_Function);

   --  String transformation subprograms

--  function Replace_Slice (
--    Source : Unbounded_String;
--    Low : Positive;
--    High : Natural;
--    By : String)
--    return Unbounded_String;

--  procedure Replace_Slice (
--    Source : in out Unbounded_String;
--    Low : Positive;
--    High : Natural;
--    By : String);

--  function Insert (
--    Source : Unbounded_String;
--    Before : Positive;
--    New_Item : String)
--    return Unbounded_String;

--  procedure Insert (
--    Source : in out Unbounded_String;
--    Before : Positive;
--    New_Item : String);

--  function Overwrite (
--    Source : Unbounded_String;
--    Position : Positive;
--    New_Item : String)
--    return Unbounded_String;

--  procedure Overwrite (
--    Source : in out Unbounded_String;
--    Position : Positive;
--    New_Item : String);

--  function Delete (
--    Source : Unbounded_String;
--    From : Positive;
--    Through : Natural)
--    return Unbounded_String;

--  procedure Delete (
--    Source : in out Unbounded_String;
--    From : Positive;
--    Through : Natural);

--  function Trim (
--    Source : Unbounded_String;
--    Side : Trim_End)
--    return Unbounded_String;

--  procedure Trim (
--    Source : in out Unbounded_String;
--    Side : Trim_End);

--  function Trim (
--    Source : Unbounded_String;
--    Left : Maps.Character_Set;
--    Right : Maps.Character_Set)
--    return Unbounded_String;

--  procedure Trim (
--    Source : in out Unbounded_String;
--    Left : Maps.Character_Set;
--    Right : Maps.Character_Set);

--  function Head (
--    Source : Unbounded_String;
--    Count : Natural;
--    Pad : Character := Space)
--    return Unbounded_String;

--  procedure Head (
--    Source : in out Unbounded_String;
--    Count : Natural;
--    Pad : Character := Space);

--  function Tail (
--    Source : Unbounded_String;
--    Count : Natural;
--    Pad : Character := Space)
--    return Unbounded_String;

--  procedure Tail (
--    Source : in out Unbounded_String;
--    Count : Natural;
--    Pad : Character := Space);

--  function "*" (
--    Left : Natural;
--    Right : Character)
--    return Unbounded_String;

--  function "*" (Left : Natural; Right : String) return Unbounded_String;

--  function "*" (Left : Natural; Right : Unbounded_String)
--    return Unbounded_String;

end Ada.Strings.Unbounded;
