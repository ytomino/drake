pragma License (Unrestricted);
with Ada.Strings.Unbounded_Wide_Wide_Strings;
with Ada.Strings.Unbounded_Wide_Wide_Strings.Functions;
with Ada.Strings.Unbounded_Wide_Wide_Strings.Functions.Maps;
with Ada.Strings.Wide_Wide_Maps;
package Ada.Strings.Wide_Wide_Unbounded is
   pragma Preelaborate;

--  type Unbounded_Wide_Wide_String is private;
--  pragma Preelaborable_Initialization (Unbounded_Wide_Wide_String);
   subtype Unbounded_Wide_Wide_String is
      Unbounded_Wide_Wide_Strings.Unbounded_String;

   --  modified
--  Null_Unbounded_Wide_Wide_String : constant Unbounded_Wide_Wide_String;
   function Null_Unbounded_Wide_Wide_String return Unbounded_Wide_Wide_String
      renames Unbounded_Wide_Wide_Strings.Null_Unbounded_String;

   function Length (Source : Unbounded_Wide_Wide_String) return Natural
      renames Unbounded_Wide_Wide_Strings.Length;

   --  extended
   procedure Set_Length (
      Source : in out Unbounded_Wide_Wide_String;
      Length : Natural)
      renames Unbounded_Wide_Wide_Strings.Set_Length;

--  type Wide_Wide_String_Access is access all Wide_Wide_String;
   subtype Wide_Wide_String_Access is
      Unbounded_Wide_Wide_Strings.String_Access;
   procedure Free (X : in out Wide_Wide_String_Access)
      renames Unbounded_Wide_Wide_Strings.Free;

   --  Conversion, Concatenation, and Selection functions

   function To_Unbounded_Widw_Wide_String (Source : Wide_Wide_String)
      return Unbounded_Wide_Wide_String
      renames Unbounded_Wide_Wide_Strings.To_Unbounded_String;

   function To_Unbounded_Widw_Wide_String (Length : Natural)
      return Unbounded_Wide_Wide_String
      renames Unbounded_Wide_Wide_Strings.To_Unbounded_String;

   function To_Wide_Wide_String (Source : Unbounded_Wide_Wide_String)
      return Wide_Wide_String
      renames Unbounded_Wide_Wide_Strings.To_String;

   procedure Set_Unbounded_Wide_Wide_String (
      Target : out Unbounded_Wide_Wide_String;
      Source : Wide_Wide_String)
      renames Unbounded_Wide_Wide_Strings.Set_Unbounded_String;

   procedure Append (
      Source : in out Unbounded_Wide_Wide_String;
      New_Item : Unbounded_Wide_Wide_String)
      renames Unbounded_Wide_Wide_Strings.Append;

   procedure Append (
      Source : in out Unbounded_Wide_Wide_String;
      New_Item : Wide_Wide_String)
      renames Unbounded_Wide_Wide_Strings.Append;

   procedure Append (
      Source : in out Unbounded_Wide_Wide_String;
      New_Item : Wide_Wide_Character)
      renames Unbounded_Wide_Wide_Strings.Append;

   function "&" (Left, Right : Unbounded_Wide_Wide_String)
      return Unbounded_Wide_Wide_String
      renames Unbounded_Wide_Wide_Strings."&";

   function "&" (Left : Unbounded_Wide_Wide_String; Right : Wide_Wide_String)
      return Unbounded_Wide_Wide_String
      renames Unbounded_Wide_Wide_Strings."&";

   function "&" (Left : Wide_Wide_String; Right : Unbounded_Wide_Wide_String)
      return Unbounded_Wide_Wide_String
      renames Unbounded_Wide_Wide_Strings."&";

   function "&" (
      Left : Unbounded_Wide_Wide_String;
      Right : Wide_Wide_Character)
      return Unbounded_Wide_Wide_String
      renames Unbounded_Wide_Wide_Strings."&";

   function "&" (
      Left : Wide_Wide_Character;
      Right : Unbounded_Wide_Wide_String)
      return Unbounded_Wide_Wide_String
      renames Unbounded_Wide_Wide_Strings."&";

   function Element (Source : Unbounded_Wide_Wide_String; Index : Positive)
      return Wide_Wide_Character
      renames Unbounded_Wide_Wide_Strings.Element;

   procedure Replace_Element (
      Source : in out Unbounded_Wide_Wide_String;
      Index : Positive;
      By : Wide_Wide_Character)
      renames Unbounded_Wide_Wide_Strings.Replace_Element;

   function Slice (
      Source : Unbounded_Wide_Wide_String;
      Low : Positive;
      High : Natural)
      return Wide_Wide_String
      renames Unbounded_Wide_Wide_Strings.Slice;

   function Unbounded_Slice (
      Source : Unbounded_Wide_Wide_String;
      Low : Positive;
      High : Natural)
      return Unbounded_Wide_Wide_String
      renames Unbounded_Wide_Wide_Strings.Unbounded_Slice;

   procedure Unbounded_Slice (
      Source : Unbounded_Wide_Wide_String;
      Target : out Unbounded_Wide_Wide_String;
      Low : Positive;
      High : Natural)
      renames Unbounded_Wide_Wide_Strings.Unbounded_Slice;

   function "=" (Left, Right : Unbounded_Wide_Wide_String) return Boolean
      renames Unbounded_Wide_Wide_Strings."=";

   function "=" (Left : Unbounded_Wide_Wide_String; Right : Wide_Wide_String)
      return Boolean
      renames Unbounded_Wide_Wide_Strings."=";

   function "=" (Left : Wide_Wide_String; Right : Unbounded_Wide_Wide_String)
      return Boolean
      renames Unbounded_Wide_Wide_Strings."=";

   function "<" (Left, Right : Unbounded_Wide_Wide_String) return Boolean
      renames Unbounded_Wide_Wide_Strings."<";

   function "<" (Left : Unbounded_Wide_Wide_String; Right : Wide_Wide_String)
      return Boolean
      renames Unbounded_Wide_Wide_Strings."<";

   function "<" (Left : Wide_Wide_String; Right : Unbounded_Wide_Wide_String)
      return Boolean
      renames Unbounded_Wide_Wide_Strings."<";

   function "<=" (Left, Right : Unbounded_Wide_Wide_String) return Boolean
      renames Unbounded_Wide_Wide_Strings."<=";

   function "<=" (Left : Unbounded_Wide_Wide_String; Right : Wide_Wide_String)
      return Boolean
      renames Unbounded_Wide_Wide_Strings."<=";

   function "<=" (Left : Wide_Wide_String; Right : Unbounded_Wide_Wide_String)
      return Boolean
      renames Unbounded_Wide_Wide_Strings."<=";

   function ">" (Left, Right : Unbounded_Wide_Wide_String) return Boolean
      renames Unbounded_Wide_Wide_Strings.">";

   function ">" (Left : Unbounded_Wide_Wide_String; Right : Wide_Wide_String)
      return Boolean
      renames Unbounded_Wide_Wide_Strings.">";

   function ">" (Left : Wide_Wide_String; Right : Unbounded_Wide_Wide_String)
      return Boolean
      renames Unbounded_Wide_Wide_Strings.">";

   function ">=" (Left, Right : Unbounded_Wide_Wide_String) return Boolean
      renames Unbounded_Wide_Wide_Strings.">=";

   function ">=" (Left : Unbounded_Wide_Wide_String; Right : Wide_Wide_String)
      return Boolean
      renames Unbounded_Wide_Wide_Strings.">=";

   function ">=" (Left : Wide_Wide_String; Right : Unbounded_Wide_Wide_String)
      return Boolean
      renames Unbounded_Wide_Wide_Strings.">=";

   --  Search subprograms

   --  modified
--  function Index (
--    Source : Unbounded_Wide_Wide_String;
--    Pattern : Wide_Wide_String;
--    From : Positive;
--    Going : Direction := Forward;
--    Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping :=
--       Wide_Wide_Maps.Identity)
--    return Natural;
   function Index (
      Source : Unbounded_Wide_Wide_String;
      Pattern : Wide_Wide_String;
      From : Positive;
      Going : Direction := Forward)
      return Natural
      renames Unbounded_Wide_Wide_Strings.Functions.Index;
   function Index (
      Source : Unbounded_Wide_Wide_String;
      Pattern : Wide_Wide_String;
      From : Positive;
      Going : Direction := Forward;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping)
      return Natural
      renames Unbounded_Wide_Wide_Strings.Functions.Maps.Index;

   --  modified
--  function Index (
--    Source : Unbounded_Wide_Wide_String;
--    Pattern : Wide_Wide_String;
--    From : Positive;
--    Going : Direction := Forward;
--    Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping_Function)
--    return Natural;
   function Index (
      Source : Unbounded_Wide_Wide_String;
      Pattern : Wide_Wide_String;
      From : Positive;
      Going : Direction := Forward;
      Mapping : not null access function (From : Wide_Wide_Character)
         return Wide_Wide_Character)
      return Natural
      renames Unbounded_Wide_Wide_Strings.Functions.Maps.Index;

   --  modified
--  function Index (
--    Source : Unbounded_Wide_Wide_String;
--    Pattern : Wide_Wide_String;
--    Going : Direction := Forward;
--    Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping :=
--       Wide_Wide_Maps.Identity)
--    return Natural;
   function Index (
      Source : Unbounded_Wide_Wide_String;
      Pattern : Wide_Wide_String;
      Going : Direction := Forward)
      return Natural
      renames Unbounded_Wide_Wide_Strings.Functions.Index;
   function Index (
      Source : Unbounded_Wide_Wide_String;
      Pattern : Wide_Wide_String;
      Going : Direction := Forward;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping)
      return Natural
      renames Unbounded_Wide_Wide_Strings.Functions.Maps.Index;

   --  modified
--  function Index (
--    Source : Unbounded_Wide_Wide_String;
--    Pattern : Wide_Wide_String;
--    Going : Direction := Forward;
--    Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping_Function)
--    return Natural;
   function Index (
      Source : Unbounded_Wide_Wide_String;
      Pattern : Wide_Wide_String;
      Going : Direction := Forward;
      Mapping : not null access function (From : Wide_Wide_Character)
         return Wide_Wide_Character)
      return Natural
      renames Unbounded_Wide_Wide_Strings.Functions.Maps.Index;

   function Index (
      Source : Unbounded_Wide_Wide_String;
      Set : Wide_Wide_Maps.Wide_Wide_Character_Set;
      From : Positive;
      Test : Membership := Inside;
      Going : Direction := Forward)
      return Natural
      renames Unbounded_Wide_Wide_Strings.Functions.Maps.Index;

   function Index (
      Source : Unbounded_Wide_Wide_String;
      Set : Wide_Wide_Maps.Wide_Wide_Character_Set;
      Test : Membership := Inside;
      Going : Direction := Forward)
      return Natural
      renames Unbounded_Wide_Wide_Strings.Functions.Maps.Index;

   function Index_Non_Blank (
      Source : Unbounded_Wide_Wide_String;
      From : Positive;
      Going : Direction := Forward)
      return Natural
      renames Unbounded_Wide_Wide_Strings.Functions.Index_Non_Blank;

   function Index_Non_Blank (
      Source : Unbounded_Wide_Wide_String;
      Going : Direction := Forward)
      return Natural
      renames Unbounded_Wide_Wide_Strings.Functions.Index_Non_Blank;

   --  modified
--  function Count (
--    Source : Unbounded_Wide_Wide_String;
--    Pattern : Wide_Wide_String;
--    Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping :=
--       Wide_Wide_Maps.Identity)
--    return Natural;
   function Count (
      Source : Unbounded_Wide_Wide_String;
      Pattern : Wide_Wide_String)
      return Natural
      renames Unbounded_Wide_Wide_Strings.Functions.Count;
   function Count (
      Source : Unbounded_Wide_Wide_String;
      Pattern : Wide_Wide_String;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping)
      return Natural
      renames Unbounded_Wide_Wide_Strings.Functions.Maps.Count;

   --  modified
--  function Count (
--    Source : Unbounded_String;
--    Pattern : String;
--    Mapping : Maps.Character_Mapping_Function)
--    return Natural;
   function Count (
      Source : Unbounded_Wide_Wide_String;
      Pattern : Wide_Wide_String;
      Mapping : not null access function (From : Wide_Wide_Character)
         return Wide_Wide_Character)
      return Natural
      renames Unbounded_Wide_Wide_Strings.Functions.Maps.Count;

   function Count (
      Source : Unbounded_Wide_Wide_String;
      Set : Wide_Wide_Maps.Wide_Wide_Character_Set)
      return Natural
      renames Unbounded_Wide_Wide_Strings.Functions.Maps.Count;

   procedure Find_Token (
      Source : Unbounded_Wide_Wide_String;
      Set : Wide_Wide_Maps.Wide_Wide_Character_Set;
      From : Positive;
      Test : Membership;
      First : out Positive;
      Last : out Natural)
      renames Unbounded_Wide_Wide_Strings.Functions.Maps.Find_Token;

   procedure Find_Token (
      Source : Unbounded_Wide_Wide_String;
      Set : Wide_Wide_Maps.Wide_Wide_Character_Set;
      Test : Membership;
      First : out Positive;
      Last : out Natural)
      renames Unbounded_Wide_Wide_Strings.Functions.Maps.Find_Token;

   --  Wide_Wide_String translation subprograms

   function Translate (
      Source : Unbounded_Wide_Wide_String;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping)
      return Unbounded_Wide_Wide_String
      renames Unbounded_Wide_Wide_Strings.Functions.Maps.Translate;

   procedure Translate (
      Source : in out Unbounded_Wide_Wide_String;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping)
      renames Unbounded_Wide_Wide_Strings.Functions.Maps.Translate;

   --  modified
--  function Translate (
--    Source : Unbounded_Wide_Wide_String;
--    Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping_Function)
--    return Unbounded_Wide_Wide_String;
   function Translate (
      Source : Unbounded_Wide_Wide_String;
      Mapping : not null access function (From : Wide_Wide_Character)
         return Wide_Wide_Character)
      return Unbounded_Wide_Wide_String
      renames Unbounded_Wide_Wide_Strings.Functions.Maps.Translate;

   --  modified
--  procedure Translate (
--    Source : in out Unbounded_Wide_Wide_String;
--    Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping_Function);
   procedure Translate (
      Source : in out Unbounded_Wide_Wide_String;
      Mapping : not null access function (From : Wide_Wide_Character)
         return Wide_Wide_Character)
      renames Unbounded_Wide_Wide_Strings.Functions.Maps.Translate;

   --  Wide_Wide_String transformation subprograms

   function Replace_Slice (
      Source : Unbounded_Wide_Wide_String;
      Low : Positive;
      High : Natural;
      By : Wide_Wide_String)
      return Unbounded_Wide_Wide_String
      renames Unbounded_Wide_Wide_Strings.Functions.Replace_Slice;

   procedure Replace_Slice (
      Source : in out Unbounded_Wide_Wide_String;
      Low : Positive;
      High : Natural;
      By : Wide_Wide_String)
      renames Unbounded_Wide_Wide_Strings.Functions.Replace_Slice;

   function Insert (
      Source : Unbounded_Wide_Wide_String;
      Before : Positive;
      New_Item : Wide_Wide_String)
      return Unbounded_Wide_Wide_String
      renames Unbounded_Wide_Wide_Strings.Functions.Insert;

   procedure Insert (
      Source : in out Unbounded_Wide_Wide_String;
      Before : Positive;
      New_Item : Wide_Wide_String)
      renames Unbounded_Wide_Wide_Strings.Functions.Insert;

   function Overwrite (
      Source : Unbounded_Wide_Wide_String;
      Position : Positive;
      New_Item : Wide_Wide_String)
      return Unbounded_Wide_Wide_String
      renames Unbounded_Wide_Wide_Strings.Functions.Overwrite;

   procedure Overwrite (
      Source : in out Unbounded_Wide_Wide_String;
      Position : Positive;
      New_Item : Wide_Wide_String)
      renames Unbounded_Wide_Wide_Strings.Functions.Overwrite;

   function Delete (
      Source : Unbounded_Wide_Wide_String;
      From : Positive;
      Through : Natural)
      return Unbounded_Wide_Wide_String
      renames Unbounded_Wide_Wide_Strings.Functions.Delete;

   procedure Delete (
      Source : in out Unbounded_Wide_Wide_String;
      From : Positive;
      Through : Natural)
      renames Unbounded_Wide_Wide_Strings.Functions.Delete;

   --  modified
   function Trim (
      Source : Unbounded_Wide_Wide_String;
      Side : Trim_End;
      Blank : Wide_Wide_Character := Wide_Wide_Space) -- additional
      return Unbounded_Wide_Wide_String
      renames Unbounded_Wide_Wide_Strings.Functions.Trim;

   --  modified
   procedure Trim (
      Source : in out Unbounded_Wide_Wide_String;
      Side : Trim_End;
      Blank : Wide_Wide_Character := Wide_Wide_Space) -- additional
      renames Unbounded_Wide_Wide_Strings.Functions.Trim;

   function Trim (
      Source : Unbounded_Wide_Wide_String;
      Left : Wide_Wide_Maps.Wide_Wide_Character_Set;
      Right : Wide_Wide_Maps.Wide_Wide_Character_Set)
      return Unbounded_Wide_Wide_String
      renames Unbounded_Wide_Wide_Strings.Functions.Maps.Trim;

   procedure Trim (
      Source : in out Unbounded_Wide_Wide_String;
      Left : Wide_Wide_Maps.Wide_Wide_Character_Set;
      Right : Wide_Wide_Maps.Wide_Wide_Character_Set)
      renames Unbounded_Wide_Wide_Strings.Functions.Maps.Trim;

   function Head (
      Source : Unbounded_Wide_Wide_String;
      Count : Natural;
      Pad : Wide_Wide_Character := Wide_Wide_Space)
      return Unbounded_Wide_Wide_String
      renames Unbounded_Wide_Wide_Strings.Functions.Head;

   procedure Head (
      Source : in out Unbounded_Wide_Wide_String;
      Count : Natural;
      Pad : Wide_Wide_Character := Wide_Wide_Space)
      renames Unbounded_Wide_Wide_Strings.Functions.Head;

   function Tail (
      Source : Unbounded_Wide_Wide_String;
      Count : Natural;
      Pad : Wide_Wide_Character := Wide_Wide_Space)
      return Unbounded_Wide_Wide_String
      renames Unbounded_Wide_Wide_Strings.Functions.Tail;

   procedure Tail (
      Source : in out Unbounded_Wide_Wide_String;
      Count : Natural;
      Pad : Wide_Wide_Character := Wide_Wide_Space)
      renames Unbounded_Wide_Wide_Strings.Functions.Tail;

   function "*" (Left : Natural; Right : Wide_Wide_Character)
      return Unbounded_Wide_Wide_String
      renames Unbounded_Wide_Wide_Strings.Functions."*";

   function "*" (Left : Natural; Right : Wide_Wide_String)
      return Unbounded_Wide_Wide_String
      renames Unbounded_Wide_Wide_Strings.Functions."*";

   function "*" (Left : Natural; Right : Unbounded_Wide_Wide_String)
      return Unbounded_Wide_Wide_String
      renames Unbounded_Wide_Wide_Strings.Functions."*";

end Ada.Strings.Wide_Wide_Unbounded;
