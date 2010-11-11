pragma License (Unrestricted);
with Ada.Strings.Generic_Unbounded;
--  with Ada.Strings.Wide_Maps;
package Ada.Strings.Wide_Unbounded is
   pragma Preelaborate;

   --  Generic_Unbounded is not able to use directly since some names differ.
   package Instance is new Generic_Unbounded (Wide_Character, Wide_String);

--  type Unbounded_Wide_String is private;
--  pragma Preelaborable_Initialization (Unbounded_Wide_String);
   subtype Unbounded_Wide_String is Instance.Unbounded_String;

--  Null_Unbounded_Wide_String : constant Unbounded_Wide_String;
   function Null_Unbounded_Wide_String return Unbounded_Wide_String
      renames Instance.Null_Unbounded_String;

   function Length (Source : Unbounded_Wide_String) return Natural
      renames Instance.Length;

--  type Wide_String_Access is access all Wide_String;
   subtype Wide_String_Access is Instance.String_Access;
   procedure Free (X : in out Wide_String_Access)
      renames Instance.Free;

   --  Conversion, Concatenation, and Selection functions

   function To_Unbounded_Wide_String (Source : Wide_String)
      return Unbounded_Wide_String
      renames Instance.To_Unbounded_String;

--  function To_Unbounded_String (Length : Natural)
--    return Unbounded_String;

   function To_Wide_String (Source : Unbounded_Wide_String) return Wide_String
      renames Instance.To_String;

--  procedure Set_Unbounded_String (
--    Target : out Unbounded_String;
--    Source : String);

   procedure Append (
      Source : in out Unbounded_Wide_String;
      New_Item : Unbounded_Wide_String)
      renames Instance.Append;

   procedure Append (
      Source : in out Unbounded_Wide_String;
      New_Item : Wide_String)
      renames Instance.Append;

   procedure Append (
      Source : in out Unbounded_Wide_String;
      New_Item : Wide_Character)
      renames Instance.Append;

   function "&" (Left, Right : Unbounded_Wide_String)
      return Unbounded_Wide_String
      renames Instance."&";

   function "&" (Left : Unbounded_Wide_String; Right : Wide_String)
      return Unbounded_Wide_String
      renames Instance."&";

   function "&" (Left : Wide_String; Right : Unbounded_Wide_String)
      return Unbounded_Wide_String
      renames Instance."&";

   function "&" (Left : Unbounded_Wide_String; Right : Wide_Character)
      return Unbounded_Wide_String
      renames Instance."&";

   function "&" (Left : Wide_Character; Right : Unbounded_Wide_String)
      return Unbounded_Wide_String
      renames Instance."&";

--  function Element (Source : Unbounded_String; Index : Positive)
--    return Character;

--  procedure Replace_Element (
--    Source : in out Unbounded_String;
--    Index : Positive;
--    By : Character);

   function Slice (
      Source : Unbounded_Wide_String;
      Low : Positive;
      High : Natural)
      return Wide_String
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

   function "=" (Left, Right : Unbounded_Wide_String) return Boolean
      renames Instance."=";

   function "=" (Left : Unbounded_Wide_String; Right : Wide_String)
      return Boolean
      renames Instance."=";

   function "=" (Left : Wide_String; Right : Unbounded_Wide_String)
      return Boolean
      renames Instance."=";

   function "<" (Left, Right : Unbounded_Wide_String) return Boolean
      renames Instance."<";

--  function "<" (Left : Unbounded_String; Right : String) return Boolean;

--  function "<" (Left : String; Right : Unbounded_String) return Boolean;

--  function "<=" (Left, Right : Unbounded_String) return Boolean;

--  function "<=" (Left : Unbounded_String; Right : String) return Boolean;

--  function "<=" (Left : String; Right : Unbounded_String) return Boolean;

   function ">" (Left, Right : Unbounded_Wide_String) return Boolean
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

end Ada.Strings.Wide_Unbounded;
