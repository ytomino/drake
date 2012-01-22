pragma License (Unrestricted);
with Ada.Strings.Maps;
with Ada.Strings.Unbounded_Strings;
with Ada.Strings.Unbounded_Strings.Functions;
with Ada.Strings.Unbounded_Strings.Functions.Maps;
package Ada.Strings.Unbounded is
   pragma Preelaborate;

--  type Unbounded_String is private;
--  pragma Preelaborable_Initialization (Unbounded_String);
   subtype Unbounded_String is Unbounded_Strings.Unbounded_String;

--  Null_Unbounded_String : constant Unbounded_String;
   function Null_Unbounded_String return Unbounded_String
      renames Unbounded_Strings.Null_Unbounded_String;

   function Length (Source : Unbounded_String) return Natural
      renames Unbounded_Strings.Length;

   --  extended
   procedure Set_Length (Source : in out Unbounded_String; Length : Natural)
      renames Unbounded_Strings.Set_Length;

--  type String_Access is access all String;
   subtype String_Access is Unbounded_Strings.String_Access;
   procedure Free (X : in out String_Access)
      renames Unbounded_Strings.Free;

   --  Conversion, Concatenation, and Selection functions

   function To_Unbounded_String (Source : String)
      return Unbounded_String
      renames Unbounded_Strings.To_Unbounded_String;

   function To_Unbounded_String (Length : Natural)
      return Unbounded_String
      renames Unbounded_Strings.To_Unbounded_String;

   function To_String (Source : Unbounded_String) return String
      renames Unbounded_Strings.To_String;

   procedure Set_Unbounded_String (
      Target : out Unbounded_String;
      Source : String)
      renames Unbounded_Strings.Set_Unbounded_String;

   procedure Append (
      Source : in out Unbounded_String;
      New_Item : Unbounded_String)
      renames Unbounded_Strings.Append;

   procedure Append (
      Source : in out Unbounded_String;
      New_Item : String)
      renames Unbounded_Strings.Append;

   procedure Append (
      Source : in out Unbounded_String;
      New_Item : Character)
      renames Unbounded_Strings.Append;

   function "&" (Left, Right : Unbounded_String) return Unbounded_String
      renames Unbounded_Strings."&";

   function "&" (Left : Unbounded_String; Right : String)
      return Unbounded_String
      renames Unbounded_Strings."&";

   function "&" (Left : String; Right : Unbounded_String)
      return Unbounded_String
      renames Unbounded_Strings."&";

   function "&" (Left : Unbounded_String; Right : Character)
      return Unbounded_String
      renames Unbounded_Strings."&";

   function "&" (Left : Character; Right : Unbounded_String)
      return Unbounded_String
      renames Unbounded_Strings."&";

   function Element (Source : Unbounded_String; Index : Positive)
      return Character
      renames Unbounded_Strings.Element;

   procedure Replace_Element (
      Source : in out Unbounded_String;
      Index : Positive;
      By : Character)
      renames Unbounded_Strings.Replace_Element;

   function Slice (
      Source : Unbounded_String;
      Low : Positive;
      High : Natural)
      return String
      renames Unbounded_Strings.Slice;

   function Unbounded_Slice (
      Source : Unbounded_String;
      Low : Positive;
      High : Natural)
      return Unbounded_String
      renames Unbounded_Strings.Unbounded_Slice;

   procedure Unbounded_Slice (
      Source : Unbounded_String;
      Target : out Unbounded_String;
      Low : Positive;
      High : Natural)
      renames Unbounded_Strings.Unbounded_Slice;

   function "=" (Left, Right : Unbounded_String) return Boolean
      renames Unbounded_Strings."=";
      --  In CXA4030(error since Unicode), conflicted by "use" and "use types",
      --  but CXA4031(PASSED) requires this.

   function "=" (Left : Unbounded_String; Right : String) return Boolean
      renames Unbounded_Strings."=";

   function "=" (Left : String; Right : Unbounded_String) return Boolean
      renames Unbounded_Strings."=";

   function "<" (Left, Right : Unbounded_String) return Boolean
      renames Unbounded_Strings."<";

   function "<" (Left : Unbounded_String; Right : String) return Boolean
      renames Unbounded_Strings."<";

   function "<" (Left : String; Right : Unbounded_String) return Boolean
      renames Unbounded_Strings."<";

   function "<=" (Left, Right : Unbounded_String) return Boolean
      renames Unbounded_Strings."<=";

   function "<=" (Left : Unbounded_String; Right : String) return Boolean
      renames Unbounded_Strings."<=";

   function "<=" (Left : String; Right : Unbounded_String) return Boolean
      renames Unbounded_Strings."<=";

   function ">" (Left, Right : Unbounded_String) return Boolean
      renames Unbounded_Strings.">";

   function ">" (Left : Unbounded_String; Right : String) return Boolean
      renames Unbounded_Strings.">";

   function ">" (Left : String; Right : Unbounded_String) return Boolean
      renames Unbounded_Strings.">";

   function ">=" (Left, Right : Unbounded_String) return Boolean
      renames Unbounded_Strings.">=";

   function ">=" (Left : Unbounded_String; Right : String) return Boolean
      renames Unbounded_Strings.">=";

   function ">=" (Left : String; Right : Unbounded_String) return Boolean
      renames Unbounded_Strings.">=";

   --  Search subprograms

   --  modified
--  function Index (
--    Source : Unbounded_String;
--    Pattern : String;
--    From : Positive;
--    Going : Direction := Forward;
--    Mapping : Maps.Character_Mapping := Maps.Identity)
--    return Natural;
   function Index (
      Source : Unbounded_String;
      Pattern : String;
      From : Positive;
      Going : Direction := Forward)
      return Natural
      renames Unbounded_Strings.Functions.Index;
   function Index (
      Source : Unbounded_String;
      Pattern : String;
      From : Positive;
      Going : Direction := Forward;
      Mapping : Maps.Character_Mapping)
      return Natural
      renames Unbounded_Strings.Functions.Maps.Index;

   --  modified
--  function Index (
--    Source : Unbounded_String;
--    Pattern : String;
--    From : Positive;
--    Going : Direction := Forward;
--    Mapping : Maps.Character_Mapping_Function)
--    return Natural;
   function Index (
      Source : Unbounded_String;
      Pattern : String;
      From : Positive;
      Going : Direction := Forward;
      Mapping : not null access function (From : Character) return Character)
      return Natural
      renames Unbounded_Strings.Functions.Maps.Index_Per_Element;
   function Index (
      Source : Unbounded_String;
      Pattern : String;
      From : Positive;
      Going : Direction := Forward;
      Mapping : not null access function (From : Wide_Wide_Character)
         return Wide_Wide_Character)
      return Natural
      renames Unbounded_Strings.Functions.Maps.Index;

   --  modified
--  function Index (
--    Source : Unbounded_String;
--    Pattern : String;
--    Going : Direction := Forward;
--    Mapping : Maps.Character_Mapping := Maps.Identity)
--    return Natural;
   function Index (
      Source : Unbounded_String;
      Pattern : String;
      Going : Direction := Forward)
      return Natural
      renames Unbounded_Strings.Functions.Index;
   function Index (
      Source : Unbounded_String;
      Pattern : String;
      Going : Direction := Forward;
      Mapping : Maps.Character_Mapping)
      return Natural
      renames Unbounded_Strings.Functions.Maps.Index;

   --  modified
--  function Index (
--    Source : Unbounded_String;
--    Pattern : String;
--    Going : Direction := Forward;
--    Mapping : Maps.Character_Mapping_Function)
--    return Natural;
   function Index (
      Source : Unbounded_String;
      Pattern : String;
      Going : Direction := Forward;
      Mapping : not null access function (From : Character) return Character)
      return Natural
      renames Unbounded_Strings.Functions.Maps.Index_Per_Element;
   function Index (
      Source : Unbounded_String;
      Pattern : String;
      Going : Direction := Forward;
      Mapping : not null access function (From : Wide_Wide_Character)
         return Wide_Wide_Character)
      return Natural
      renames Unbounded_Strings.Functions.Maps.Index;

   function Index (
      Source : Unbounded_String;
      Set : Maps.Character_Set;
      From : Positive;
      Test : Membership := Inside;
      Going : Direction := Forward)
      return Natural
      renames Unbounded_Strings.Functions.Maps.Index;

   function Index (
      Source : Unbounded_String;
      Set : Maps.Character_Set;
      Test : Membership := Inside;
      Going : Direction := Forward)
      return Natural
      renames Unbounded_Strings.Functions.Maps.Index;

   function Index_Non_Blank (
      Source : Unbounded_String;
      From : Positive;
      Going : Direction := Forward)
      return Natural
      renames Unbounded_Strings.Functions.Index_Non_Blank;

   function Index_Non_Blank (
      Source : Unbounded_String;
      Going : Direction := Forward)
      return Natural
      renames Unbounded_Strings.Functions.Index_Non_Blank;

   --  modified
--  function Count (
--    Source : Unbounded_String;
--    Pattern : String;
--    Mapping : Maps.Character_Mapping := Maps.Identity)
--    return Natural;
   function Count (
      Source : Unbounded_String;
      Pattern : String)
      return Natural
      renames Unbounded_Strings.Functions.Count;
   function Count (
      Source : Unbounded_String;
      Pattern : String;
      Mapping : Maps.Character_Mapping)
      return Natural
      renames Unbounded_Strings.Functions.Maps.Count;

   --  modified
--  function Count (
--    Source : Unbounded_String;
--    Pattern : String;
--    Mapping : Maps.Character_Mapping_Function)
--    return Natural;
   function Count (
      Source : Unbounded_String;
      Pattern : String;
      Mapping : not null access function (From : Character) return Character)
      return Natural
      renames Unbounded_Strings.Functions.Maps.Count_Per_Element;
   function Count (
      Source : Unbounded_String;
      Pattern : String;
      Mapping : not null access function (From : Wide_Wide_Character)
         return Wide_Wide_Character)
      return Natural
      renames Unbounded_Strings.Functions.Maps.Count;

   function Count (
      Source : Unbounded_String;
      Set : Maps.Character_Set)
      return Natural
      renames Unbounded_Strings.Functions.Maps.Count;

   procedure Find_Token (
      Source : Unbounded_String;
      Set : Maps.Character_Set;
      From : Positive;
      Test : Membership;
      First : out Positive;
      Last : out Natural)
      renames Unbounded_Strings.Functions.Maps.Find_Token;

   procedure Find_Token (
      Source : Unbounded_String;
      Set : Maps.Character_Set;
      Test : Membership;
      First : out Positive;
      Last : out Natural)
      renames Unbounded_Strings.Functions.Maps.Find_Token;

   --  String translation subprograms

   function Translate (
      Source : Unbounded_String;
      Mapping : Maps.Character_Mapping)
      return Unbounded_String
      renames Unbounded_Strings.Functions.Maps.Translate;

   procedure Translate (
      Source : in out Unbounded_String;
      Mapping : Maps.Character_Mapping)
      renames Unbounded_Strings.Functions.Maps.Translate;

   --  modified
--  function Translate (
--    Source : Unbounded_String;
--    Mapping : Maps.Character_Mapping_Function)
--    return Unbounded_String;
   function Translate (
      Source : Unbounded_String;
      Mapping : not null access function (From : Character) return Character)
      return Unbounded_String
      renames Unbounded_Strings.Functions.Maps.Translate_Per_Element;
   function Translate (
      Source : Unbounded_String;
      Mapping : not null access function (From : Wide_Wide_Character)
         return Wide_Wide_Character)
      return Unbounded_String
      renames Unbounded_Strings.Functions.Maps.Translate;

   --  modified
--  procedure Translate (
--    Source : in out Unbounded_String;
--    Mapping : Maps.Character_Mapping_Function);
   procedure Translate (
      Source : in out Unbounded_String;
      Mapping : not null access function (From : Character) return Character)
      renames Unbounded_Strings.Functions.Maps.Translate_Per_Element;
   procedure Translate (
      Source : in out Unbounded_String;
      Mapping : not null access function (From : Wide_Wide_Character)
         return Wide_Wide_Character)
      renames Unbounded_Strings.Functions.Maps.Translate;

   --  String transformation subprograms

   function Replace_Slice (
      Source : Unbounded_String;
      Low : Positive;
      High : Natural;
      By : String)
      return Unbounded_String
      renames Unbounded_Strings.Functions.Replace_Slice;

   procedure Replace_Slice (
      Source : in out Unbounded_String;
      Low : Positive;
      High : Natural;
      By : String)
      renames Unbounded_Strings.Functions.Replace_Slice;

   function Insert (
      Source : Unbounded_String;
      Before : Positive;
      New_Item : String)
      return Unbounded_String
      renames Unbounded_Strings.Functions.Insert;

   procedure Insert (
      Source : in out Unbounded_String;
      Before : Positive;
      New_Item : String)
      renames Unbounded_Strings.Functions.Insert;

   function Overwrite (
      Source : Unbounded_String;
      Position : Positive;
      New_Item : String)
      return Unbounded_String
      renames Unbounded_Strings.Functions.Overwrite;

   procedure Overwrite (
      Source : in out Unbounded_String;
      Position : Positive;
      New_Item : String)
      renames Unbounded_Strings.Functions.Overwrite;

   function Delete (
      Source : Unbounded_String;
      From : Positive;
      Through : Natural)
      return Unbounded_String
      renames Unbounded_Strings.Functions.Delete;

   procedure Delete (
      Source : in out Unbounded_String;
      From : Positive;
      Through : Natural)
      renames Unbounded_Strings.Functions.Delete;

   --  modified
   function Trim (
      Source : Unbounded_String;
      Side : Trim_End;
      Blank : Character := Space) -- additional
      return Unbounded_String
      renames Unbounded_Strings.Functions.Trim;

   --  modified
   procedure Trim (
      Source : in out Unbounded_String;
      Side : Trim_End;
      Blank : Character := Space) -- additional
      renames Unbounded_Strings.Functions.Trim;

   function Trim (
      Source : Unbounded_String;
      Left : Maps.Character_Set;
      Right : Maps.Character_Set)
      return Unbounded_String
      renames Unbounded_Strings.Functions.Maps.Trim;

   procedure Trim (
      Source : in out Unbounded_String;
      Left : Maps.Character_Set;
      Right : Maps.Character_Set)
      renames Unbounded_Strings.Functions.Maps.Trim;

   function Head (
      Source : Unbounded_String;
      Count : Natural;
      Pad : Character := Space)
      return Unbounded_String
      renames Unbounded_Strings.Functions.Head;

   procedure Head (
      Source : in out Unbounded_String;
      Count : Natural;
      Pad : Character := Space)
      renames Unbounded_Strings.Functions.Head;

   function Tail (
      Source : Unbounded_String;
      Count : Natural;
      Pad : Character := Space)
      return Unbounded_String
      renames Unbounded_Strings.Functions.Tail;

   procedure Tail (
      Source : in out Unbounded_String;
      Count : Natural;
      Pad : Character := Space)
      renames Unbounded_Strings.Functions.Tail;

   function "*" (Left : Natural; Right : Character) return Unbounded_String
      renames Unbounded_Strings.Functions."*";

   function "*" (Left : Natural; Right : String) return Unbounded_String
      renames Unbounded_Strings.Functions."*";

   function "*" (Left : Natural; Right : Unbounded_String)
      return Unbounded_String
      renames Unbounded_Strings.Functions."*";

end Ada.Strings.Unbounded;
