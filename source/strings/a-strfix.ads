pragma License (Unrestricted);
with Ada.Strings.Functions;
with Ada.Strings.Functions.Maps;
with Ada.Strings.Maps;
package Ada.Strings.Fixed is
   pragma Preelaborate;

   --  "Copy" procedure for strings of possibly different lengths

   procedure Move (
      Source : String;
      Target : out String;
      Drop : Truncation := Error;
      Justify : Alignment := Left;
      Pad : Character := Space)
      renames Functions.Move;

   --  Search subprograms

   --  extended, character searching
   function Index (
      Source : String;
      Pattern : Character;
      From : Positive;
      Going : Direction := Forward)
      return Natural
      renames Functions.Index;
   function Index (
      Source : String;
      Pattern : Character;
      Going : Direction := Forward)
      return Natural
      renames Functions.Index;

--  function Index (
--    Source : String;
--    Pattern : String;
--    From : Positive;
--    Going : Direction := Forward;
--    Mapping : Maps.Character_Mapping := Maps.Identity)
--    return Natural;
   function Index (
      Source : String;
      Pattern : String;
      From : Positive;
      Going : Direction := Forward)
      return Natural
      renames Functions.Index;
   function Index (
      Source : String;
      Pattern : String;
      From : Positive;
      Going : Direction := Forward;
      Mapping : Maps.Character_Mapping)
      return Natural
      renames Functions.Maps.Index;

--  function Index (
--    Source : String;
--    Pattern : String;
--    From : Positive;
--    Going : Direction := Forward;
--    Mapping : Maps.Character_Mapping_Function)
--    return Natural;
   function Index (
      Source : String;
      Pattern : String;
      From : Positive;
      Going : Direction := Forward;
      Mapping : not null access function (From : Character) return Character)
      return Natural
      renames Functions.Maps.Index_Per_Element;
   function Index (
      Source : String;
      Pattern : String;
      From : Positive;
      Going : Direction := Forward;
      Mapping : not null access function (From : Wide_Wide_Character)
         return Wide_Wide_Character)
      return Natural
      renames Functions.Maps.Index;

--  function Index (
--    Source : in String;
--    Pattern : in String;
--    Going : in Direction := Forward;
--    Mapping : in Maps.Character_Mapping := Maps.Identity)
--    return Natural;
   function Index (
      Source : String;
      Pattern : String;
      Going : Direction := Forward)
      return Natural
      renames Functions.Index;
   function Index (
      Source : String;
      Pattern : String;
      Going : Direction := Forward;
      Mapping : Maps.Character_Mapping)
      return Natural
      renames Functions.Maps.Index;

--  function Index (
--    Source : String;
--    Pattern : String;
--    Going : Direction := Forward;
--    Mapping : Maps.Character_Mapping_Function)
--    return Natural;
   function Index (
      Source : String;
      Pattern : String;
      Going : Direction := Forward;
      Mapping : not null access function (From : Character) return Character)
      return Natural
      renames Functions.Maps.Index_Per_Element;
   function Index (
      Source : String;
      Pattern : String;
      Going : Direction := Forward;
      Mapping : not null access function (From : Wide_Wide_Character)
         return Wide_Wide_Character)
      return Natural
      renames Functions.Maps.Index;

   function Index (
      Source : String;
      Set : Maps.Character_Set;
      From : Positive;
      Test : Membership := Inside;
      Going : Direction := Forward)
      return Natural
      renames Functions.Maps.Index;

   function Index (
      Source : String;
      Set : Maps.Character_Set;
      Test : Membership := Inside;
      Going : Direction := Forward)
      return Natural
      renames Functions.Maps.Index;

   function Index_Non_Blank (
      Source : String;
      From : Positive;
      Going : Direction := Forward)
      return Natural
      renames Functions.Index_Non_Blank;

   function Index_Non_Blank (
      Source : String;
      Going : Direction := Forward)
      return Natural
      renames Functions.Index_Non_Blank;

--  function Count (
--    Source : String;
--    Pattern : String;
--    Mapping : Maps.Character_Mapping := Maps.Identity)
--    return Natural;
   function Count (
      Source : String;
      Pattern : String)
      return Natural
      renames Functions.Count;
   function Count (
      Source : String;
      Pattern : String;
      Mapping : Maps.Character_Mapping)
      return Natural
      renames Functions.Maps.Count;

--  function Count (
--    Source : String;
--    Pattern : String;
--    Mapping : Maps.Character_Mapping_Function)
--    return Natural;
   function Count (
      Source : String;
      Pattern : String;
      Mapping : not null access function (From : Character) return Character)
      return Natural
      renames Functions.Maps.Count_Per_Element;
   function Count (
      Source : String;
      Pattern : String;
      Mapping : not null access function (From : Wide_Wide_Character)
         return Wide_Wide_Character)
      return Natural
      renames Functions.Maps.Count;

   function Count (
      Source : String;
      Set : Maps.Character_Set)
      return Natural
      renames Functions.Maps.Count;

   procedure Find_Token (
      Source : String;
      Set : Maps.Character_Set;
      From : Positive;
      Test : Membership;
      First : out Positive;
      Last : out Natural)
      renames Functions.Maps.Find_Token;

   procedure Find_Token (
      Source : String;
      Set : Maps.Character_Set;
      Test : Membership;
      First : out Positive;
      Last : out Natural)
      renames Functions.Maps.Find_Token;

   --  String translation subprograms

--  function Translate (
--    Source : String;
--    Mapping : Maps.Character_Mapping)
--    return String;
   function Translate (
      Source : String;
      Mapping : Maps.Character_Mapping)
      return String
      renames Functions.Maps.Translate;

--  procedure Translate (
--    Source : in out String;
--    Mapping : Maps.Character_Mapping);
   procedure Translate (
      Source : in out String;
      Mapping : Maps.Character_Mapping;
      Drop : Truncation := Error; -- extended
      Justify : Alignment := Left; -- extended
      Pad : Character := Space) -- extended
      renames Functions.Maps.Translate;

--  function Translate (
--    Source : String;
--    Mapping : Maps.Character_Mapping_Function)
--    return String;
   function Translate (
      Source : String;
      Mapping : not null access function (From : Character) return Character)
      return String
      renames Functions.Maps.Translate_Per_Element;
   function Translate (
      Source : String;
      Mapping : not null access function (From : Wide_Wide_Character)
         return Wide_Wide_Character)
      return String
      renames Functions.Maps.Translate;

--  procedure Translate (
--    Source : in out String;
--    Mapping : Maps.Character_Mapping_Function);
   procedure Translate (
      Source : in out String;
      Mapping : not null access function (From : Character) return Character)
      renames Functions.Maps.Translate_Per_Element;
   procedure Translate (
      Source : in out String;
      Mapping : not null access function (From : Wide_Wide_Character)
         return Wide_Wide_Character;
      Drop : Truncation := Error; -- extended
      Justify : Alignment := Left; -- extended
      Pad : Character := Space) -- extended
      renames Functions.Maps.Translate;

   --  String transformation subprograms

   function Replace_Slice (
      Source : String;
      Low : Positive;
      High : Natural;
      By : String)
      return String
      renames Functions.Replace_Slice;

   procedure Replace_Slice (
      Source : in out String;
      Low : Positive;
      High : Natural;
      By : String;
      Drop : Truncation := Error;
      Justify : Alignment := Left;
      Pad : Character := Space)
      renames Functions.Replace_Slice;

   function Insert (
      Source : String;
      Before : Positive;
      New_Item : String)
      return String
      renames Functions.Insert;

   procedure Insert (
      Source : in out String;
      Before : Positive;
      New_Item : String;
      Drop : Truncation := Error)
      renames Functions.Insert;

   function Overwrite (
      Source : String;
      Position : Positive;
      New_Item : String)
      return String
      renames Functions.Overwrite;

   procedure Overwrite (
      Source : in out String;
      Position : Positive;
      New_Item : String;
      Drop : Truncation := Right)
      renames Functions.Overwrite;

   function Delete (
      Source : String;
      From : Positive;
      Through : Natural)
      return String
      renames Functions.Delete;

   procedure Delete (
      Source : in out String;
      From : Positive;
      Through : Natural;
      Justify : Alignment := Left;
      Pad : Character := Space)
      renames Functions.Delete;

   --  String selector subprograms
--  function Trim (
--    Source : String;
--    Side : Trim_End)
--    return String;
   function Trim (
      Source : String;
      Side : Trim_End;
      Left : Character := Space; -- extended
      Right : Character := Space) -- extended
      return String
      renames Functions.Trim;

--  procedure Trim (
--    Source : in out String;
--    Side : Trim_End;
--    Justify : Alignment := Left;
--    Pad : Character := Space);
   procedure Trim (
      Source : in out String;
      Side : Trim_End;
      Left : Character := Space; -- extended
      Right : Character := Space; -- extended
      Justify : Alignment := Strings.Left;
      Pad : Character := Space)
      renames Functions.Trim;

   function Trim (
      Source : String;
      Left : Maps.Character_Set;
      Right : Maps.Character_Set)
      return String
      renames Functions.Maps.Trim;

   procedure Trim (
      Source : in out String;
      Left : Maps.Character_Set;
      Right : Maps.Character_Set;
      Justify : Alignment := Strings.Left;
      Pad : Character := Space)
      renames Functions.Maps.Trim;

   function Head (
      Source : String;
      Count : Natural;
      Pad : Character := Space)
      return String
      renames Functions.Head;

   procedure Head (
      Source : in out String;
      Count : Natural;
      Justify : Alignment := Left;
      Pad : Character := Space)
      renames Functions.Head;

   function Tail (
      Source : String;
      Count : Natural;
      Pad : Character := Space)
      return String
      renames Functions.Tail;

   procedure Tail (
      Source : in out String;
      Count : Natural;
      Justify : Alignment := Left;
      Pad : Character := Space)
      renames Functions.Tail;

   --  String constructor functions

   function "*" (Left : Natural; Right : Character)
      return String
      renames Functions."*";

   function "*" (Left : Natural; Right : String)
      return String
      renames Functions."*";

end Ada.Strings.Fixed;
