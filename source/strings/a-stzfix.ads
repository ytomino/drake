pragma License (Unrestricted);
with Ada.Strings.Wide_Wide_Functions;
with Ada.Strings.Wide_Wide_Functions.Maps;
with Ada.Strings.Wide_Wide_Maps;
package Ada.Strings.Wide_Wide_Fixed is
   pragma Preelaborate;

   --  "Copy" procedure for strings of possibly different lengths

   procedure Move (
      Source : Wide_Wide_String;
      Target : out Wide_Wide_String;
      Drop : Truncation := Error;
      Justify : Alignment := Left;
      Pad : Wide_Wide_Character := Wide_Wide_Space)
      renames Wide_Wide_Functions.Move;

   --  Search subprograms

   --  extended
   --  These functions search signle character in string.
   function Index (
      Source : Wide_Wide_String;
      Pattern : Wide_Wide_Character;
      From : Positive;
      Going : Direction := Forward)
      return Natural
      renames Wide_Wide_Functions.Index;
   function Index (
      Source : Wide_Wide_String;
      Pattern : Wide_Wide_Character;
      Going : Direction := Forward)
      return Natural
      renames Wide_Wide_Functions.Index;

   --  modified
--  function Index (
--    Source : Wide_Wide_String;
--    Pattern : Wide_Wide_String;
--    From : Positive;
--    Going : Direction := Forward;
--    Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping :=
--       Wide_Wide_Maps.Identity)
--    return Natural;
   function Index (
      Source : Wide_Wide_String;
      Pattern : Wide_Wide_String;
      From : Positive;
      Going : Direction := Forward)
      return Natural
      renames Wide_Wide_Functions.Index;
   function Index (
      Source : Wide_Wide_String;
      Pattern : Wide_Wide_String;
      From : Positive;
      Going : Direction := Forward;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping)
      return Natural
      renames Wide_Wide_Functions.Maps.Index;

   --  modified
--  function Index (
--    Source : Wide_Wide_String;
--    Pattern : Wide_Wide_String;
--    From : Positive;
--    Going : Direction := Forward;
--    Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping_Function)
--    return Natural;
   function Index (
      Source : Wide_Wide_String;
      Pattern : Wide_Wide_String;
      From : Positive;
      Going : Direction := Forward;
      Mapping : not null access function (From : Wide_Wide_Character)
         return Wide_Wide_Character)
      return Natural
      renames Wide_Wide_Functions.Maps.Index;

   --  modified
--  function Index (
--    Source : Wide_Wide_String;
--    Pattern : Wide_Wide_String;
--    Going : Direction := Forward;
--    Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping :=
--       Wide_Wide_Maps.Identity)
--    return Natural;
   function Index (
      Source : Wide_Wide_String;
      Pattern : Wide_Wide_String;
      Going : Direction := Forward)
      return Natural
      renames Wide_Wide_Functions.Index;
   function Index (
      Source : Wide_Wide_String;
      Pattern : Wide_Wide_String;
      Going : Direction := Forward;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping)
      return Natural
      renames Wide_Wide_Functions.Maps.Index;

   --  modified
--  function Index (
--    Source : Wide_Wide_String;
--    Pattern : Wide_Wide_String;
--    Going : Direction := Forward;
--    Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping_Function)
--    return Natural;
   function Index (
      Source : Wide_Wide_String;
      Pattern : Wide_Wide_String;
      Going : Direction := Forward;
      Mapping : not null access function (From : Wide_Wide_Character)
         return Wide_Wide_Character)
      return Natural
      renames Wide_Wide_Functions.Maps.Index;

   function Index (
      Source : Wide_Wide_String;
      Set : Wide_Wide_Maps.Wide_Wide_Character_Set;
      From : Positive;
      Test : Membership := Inside;
      Going : Direction := Forward)
      return Natural
      renames Wide_Wide_Functions.Maps.Index;

   function Index (
      Source : Wide_Wide_String;
      Set : Wide_Wide_Maps.Wide_Wide_Character_Set;
      Test : Membership := Inside;
      Going : Direction := Forward)
      return Natural
      renames Wide_Wide_Functions.Maps.Index;

   function Index_Non_Blank (
      Source : Wide_Wide_String;
      From : Positive;
      Going : Direction := Forward)
      return Natural
      renames Wide_Wide_Functions.Index_Non_Blank;

   function Index_Non_Blank (
      Source : Wide_Wide_String;
      Going : Direction := Forward)
      return Natural
      renames Wide_Wide_Functions.Index_Non_Blank;

   --  modified
--  function Count (
--    Source : Wide_Wide_String;
--    Pattern : Wide_Wide_String;
--    Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping :=
--       Wide_Wide_Maps.Identity)
--    return Natural;
   function Count (
      Source : Wide_Wide_String;
      Pattern : Wide_Wide_String)
      return Natural
      renames Wide_Wide_Functions.Count;
   function Count (
      Source : Wide_Wide_String;
      Pattern : Wide_Wide_String;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping)
      return Natural
      renames Wide_Wide_Functions.Maps.Count;

   --  modified
--  function Count (
--    Source : Wide_Wide_String;
--    Pattern : Wide_Wide_String;
--    Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping_Function)
--    return Natural;
   function Count (
      Source : Wide_Wide_String;
      Pattern : Wide_Wide_String;
      Mapping : not null access function (From : Wide_Wide_Character)
         return Wide_Wide_Character)
      return Natural
      renames Wide_Wide_Functions.Maps.Count;

   function Count (
      Source : Wide_Wide_String;
      Set : Wide_Wide_Maps.Wide_Wide_Character_Set)
      return Natural
      renames Wide_Wide_Functions.Maps.Count;

   procedure Find_Token (
      Source : Wide_Wide_String;
      Set : Wide_Wide_Maps.Wide_Wide_Character_Set;
      From : Positive;
      Test : Membership;
      First : out Positive;
      Last : out Natural)
      renames Wide_Wide_Functions.Maps.Find_Token;

   procedure Find_Token (
      Source : Wide_Wide_String;
      Set : Wide_Wide_Maps.Wide_Wide_Character_Set;
      Test : Membership;
      First : out Positive;
      Last : out Natural)
      renames Wide_Wide_Functions.Maps.Find_Token;

   --  Wide_Wide_String translation subprograms

   function Translate (
      Source : Wide_Wide_String;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping)
      return Wide_Wide_String
      renames Wide_Wide_Functions.Maps.Translate;

   --  modified
   procedure Translate (
      Source : in out Wide_Wide_String;
      Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping;
      Drop : Truncation := Error; -- additional
      Justify : Alignment := Left; -- additional
      Pad : Wide_Wide_Character := Wide_Wide_Space) -- additional
      renames Wide_Wide_Functions.Maps.Translate;

   --  modified
--  function Translate (
--    Source : Wide_Wide_String;
--    Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping_Function)
--    return Wide_Wide_String;
   function Translate (
      Source : Wide_Wide_String;
      Mapping : not null access function (From : Wide_Wide_Character)
         return Wide_Wide_Character)
      return Wide_Wide_String
      renames Wide_Wide_Functions.Maps.Translate;

   --  modified
--  procedure Translate (
--    Source : in out Wide_Wide_String;
--    Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping_Function);
   procedure Translate (
      Source : in out Wide_Wide_String;
      Mapping : not null access function (From : Wide_Wide_Character)
         return Wide_Wide_Character;
      Drop : Truncation := Error; -- additional
      Justify : Alignment := Left; -- additional
      Pad : Wide_Wide_Character := Wide_Wide_Space) -- additional
      renames Wide_Wide_Functions.Maps.Translate;

   --  Wide_Wide_String transformation subprograms

   function Replace_Slice (
      Source : Wide_Wide_String;
      Low : Positive;
      High : Natural;
      By : Wide_Wide_String)
      return Wide_Wide_String
      renames Wide_Wide_Functions.Replace_Slice;

   procedure Replace_Slice (
      Source : in out Wide_Wide_String;
      Low : Positive;
      High : Natural;
      By : Wide_Wide_String;
      Drop : Truncation := Error;
      Justify : Alignment := Left;
      Pad : Wide_Wide_Character := Wide_Wide_Space)
      renames Wide_Wide_Functions.Replace_Slice;

   function Insert (
      Source : Wide_Wide_String;
      Before : Positive;
      New_Item : Wide_Wide_String)
      return Wide_Wide_String
      renames Wide_Wide_Functions.Insert;

   procedure Insert (
      Source : in out Wide_Wide_String;
      Before : Positive;
      New_Item : Wide_Wide_String;
      Drop : Truncation := Error)
      renames Wide_Wide_Functions.Insert;

   function Overwrite (
      Source : Wide_Wide_String;
      Position : Positive;
      New_Item : Wide_Wide_String)
      return Wide_Wide_String
      renames Wide_Wide_Functions.Overwrite;

   procedure Overwrite (
      Source : in out Wide_Wide_String;
      Position : Positive;
      New_Item : Wide_Wide_String;
      Drop : Truncation := Right)
      renames Wide_Wide_Functions.Overwrite;

   function Delete (
      Source : Wide_Wide_String;
      From : Positive;
      Through : Natural)
      return Wide_Wide_String
      renames Wide_Wide_Functions.Delete;

   procedure Delete (
      Source : in out Wide_Wide_String;
      From : Positive;
      Through : Natural;
      Justify : Alignment := Left;
      Pad : Wide_Wide_Character := Wide_Wide_Space)
      renames Wide_Wide_Functions.Delete;

   --  Wide_Wide_String selector subprograms
   --  modified
   function Trim (
      Source : Wide_Wide_String;
      Side : Trim_End;
      Blank : Wide_Wide_Character := Wide_Wide_Space) -- additional
      return Wide_Wide_String
      renames Wide_Wide_Functions.Trim;

   procedure Trim (
      Source : in out Wide_Wide_String;
      Side : Trim_End;
      Justify : Alignment := Left;
      Pad : Wide_Wide_Character := Wide_Wide_Space)
      renames Wide_Wide_Functions.Trim;

   --  extended
   procedure Trim (
      Source : in out Wide_Wide_String;
      Side : Trim_End;
      Blank : Wide_Wide_Character;
      Justify : Alignment := Strings.Left;
      Pad : Wide_Wide_Character := Wide_Wide_Space)
      renames Wide_Wide_Functions.Trim;

   function Trim (
      Source : Wide_Wide_String;
      Left : Wide_Wide_Maps.Wide_Wide_Character_Set;
      Right : Wide_Wide_Maps.Wide_Wide_Character_Set)
      return Wide_Wide_String
      renames Wide_Wide_Functions.Maps.Trim;

   procedure Trim (
      Source : in out Wide_Wide_String;
      Left : Wide_Wide_Maps.Wide_Wide_Character_Set;
      Right : Wide_Wide_Maps.Wide_Wide_Character_Set;
      Justify : Alignment := Strings.Left;
      Pad : Wide_Wide_Character := Wide_Wide_Space)
      renames Wide_Wide_Functions.Maps.Trim;

   function Head (
      Source : Wide_Wide_String;
      Count : Natural;
      Pad : Wide_Wide_Character := Wide_Wide_Space)
      return Wide_Wide_String
      renames Wide_Wide_Functions.Head;

   procedure Head (
      Source : in out Wide_Wide_String;
      Count : Natural;
      Justify : Alignment := Left;
      Pad : Wide_Wide_Character := Wide_Wide_Space)
      renames Wide_Wide_Functions.Head;

   function Tail (
      Source : Wide_Wide_String;
      Count : Natural;
      Pad : Wide_Wide_Character := Wide_Wide_Space)
      return Wide_Wide_String
      renames Wide_Wide_Functions.Tail;

   procedure Tail (
      Source : in out Wide_Wide_String;
      Count : Natural;
      Justify : Alignment := Left;
      Pad : Wide_Wide_Character := Wide_Wide_Space)
      renames Wide_Wide_Functions.Tail;

   --  Wide_Wide_String constructor functions

   function "*" (Left : Natural; Right : Wide_Wide_Character)
      return Wide_Wide_String
      renames Wide_Wide_Functions."*";

   function "*" (Left : Natural; Right : Wide_Wide_String)
      return Wide_Wide_String
      renames Wide_Wide_Functions."*";

end Ada.Strings.Wide_Wide_Fixed;
