pragma License (Unrestricted);
with Ada.Strings.Wide_Functions;
with Ada.Strings.Wide_Functions.Maps;
with Ada.Strings.Wide_Maps;
package Ada.Strings.Wide_Fixed is
   pragma Preelaborate;

   --  "Copy" procedure for strings of possibly different lengths

   procedure Move (
      Source : Wide_String;
      Target : out Wide_String;
      Drop : Truncation := Error;
      Justify : Alignment := Left;
      Pad : Wide_Character := Wide_Space)
      renames Wide_Functions.Move;

   --  Search subprograms

   --  extended, character searching
   function Index (
      Source : Wide_String;
      Pattern : Wide_Character;
      From : Positive;
      Going : Direction := Forward)
      return Natural
      renames Wide_Functions.Index;
   function Index (
      Source : Wide_String;
      Pattern : Wide_Character;
      Going : Direction := Forward)
      return Natural
      renames Wide_Functions.Index;

--  function Index (
--    Source : Wide_String;
--    Pattern : Wide_String;
--    From : Positive;
--    Going : Direction := Forward;
--    Mapping : Maps.Character_Mapping := Maps.Identity)
--    return Natural;
   function Index (
      Source : Wide_String;
      Pattern : Wide_String;
      From : Positive;
      Going : Direction := Forward)
      return Natural
      renames Wide_Functions.Index;
   function Index (
      Source : Wide_String;
      Pattern : Wide_String;
      From : Positive;
      Going : Direction := Forward;
      Mapping : Wide_Maps.Wide_Character_Mapping)
      return Natural
      renames Wide_Functions.Maps.Index;

--  function Index (
--    Source : Wide_String;
--    Pattern : Wide_String;
--    From : Positive;
--    Going : Direction := Forward;
--    Mapping : Maps.Character_Mapping_Function)
--    return Natural;
   function Index (
      Source : Wide_String;
      Pattern : Wide_String;
      From : Positive;
      Going : Direction := Forward;
      Mapping : not null access function (From : Wide_Character)
         return Wide_Character)
      return Natural
      renames Wide_Functions.Maps.Index_Per_Element;
   function Index (
      Source : Wide_String;
      Pattern : Wide_String;
      From : Positive;
      Going : Direction := Forward;
      Mapping : not null access function (From : Wide_Wide_Character)
         return Wide_Wide_Character)
      return Natural
      renames Wide_Functions.Maps.Index;

--  function Index (
--    Source : in Wide_String;
--    Pattern : in Wide_String;
--    Going : in Direction := Forward;
--    Mapping : in Maps.Character_Mapping := Maps.Identity)
--    return Natural;
   function Index (
      Source : Wide_String;
      Pattern : Wide_String;
      Going : Direction := Forward)
      return Natural
      renames Wide_Functions.Index;
   function Index (
      Source : Wide_String;
      Pattern : Wide_String;
      Going : Direction := Forward;
      Mapping : Wide_Maps.Wide_Character_Mapping)
      return Natural
      renames Wide_Functions.Maps.Index;

--  function Index (
--    Source : Wide_String;
--    Pattern : Wide_String;
--    Going : Direction := Forward;
--    Mapping : Maps.Character_Mapping_Function)
--    return Natural;
   function Index (
      Source : Wide_String;
      Pattern : Wide_String;
      Going : Direction := Forward;
      Mapping : not null access function (From : Wide_Character)
         return Wide_Character)
      return Natural
      renames Wide_Functions.Maps.Index_Per_Element;
   function Index (
      Source : Wide_String;
      Pattern : Wide_String;
      Going : Direction := Forward;
      Mapping : not null access function (From : Wide_Wide_Character)
         return Wide_Wide_Character)
      return Natural
      renames Wide_Functions.Maps.Index;

   function Index (
      Source : Wide_String;
      Set : Wide_Maps.Wide_Character_Set;
      From : Positive;
      Test : Membership := Inside;
      Going : Direction := Forward)
      return Natural
      renames Wide_Functions.Maps.Index;

   function Index (
      Source : Wide_String;
      Set : Wide_Maps.Wide_Character_Set;
      Test : Membership := Inside;
      Going : Direction := Forward)
      return Natural
      renames Wide_Functions.Maps.Index;

   function Index_Non_Blank (
      Source : Wide_String;
      From : Positive;
      Going : Direction := Forward)
      return Natural
      renames Wide_Functions.Index_Non_Blank;

   function Index_Non_Blank (
      Source : Wide_String;
      Going : Direction := Forward)
      return Natural
      renames Wide_Functions.Index_Non_Blank;

--  function Count (
--    Source : Wide_String;
--    Pattern : Wide_String;
--    Mapping : Maps.Character_Mapping := Maps.Identity)
--    return Natural;
   function Count (
      Source : Wide_String;
      Pattern : Wide_String)
      return Natural
      renames Wide_Functions.Count;
   function Count (
      Source : Wide_String;
      Pattern : Wide_String;
      Mapping : Wide_Maps.Wide_Character_Mapping)
      return Natural
      renames Wide_Functions.Maps.Count;

--  function Count (
--    Source : Wide_String;
--    Pattern : Wide_String;
--    Mapping : Maps.Character_Mapping_Function)
--    return Natural;
   function Count (
      Source : Wide_String;
      Pattern : Wide_String;
      Mapping : not null access function (From : Wide_Character)
         return Wide_Character)
      return Natural
      renames Wide_Functions.Maps.Count_Per_Element;
   function Count (
      Source : Wide_String;
      Pattern : Wide_String;
      Mapping : not null access function (From : Wide_Wide_Character)
         return Wide_Wide_Character)
      return Natural
      renames Wide_Functions.Maps.Count;

   function Count (
      Source : Wide_String;
      Set : Wide_Maps.Wide_Character_Set)
      return Natural
      renames Wide_Functions.Maps.Count;

   procedure Find_Token (
      Source : Wide_String;
      Set : Wide_Maps.Wide_Character_Set;
      From : Positive;
      Test : Membership;
      First : out Positive;
      Last : out Natural)
      renames Wide_Functions.Maps.Find_Token;

   procedure Find_Token (
      Source : Wide_String;
      Set : Wide_Maps.Wide_Character_Set;
      Test : Membership;
      First : out Positive;
      Last : out Natural)
      renames Wide_Functions.Maps.Find_Token;

   --  Wide_String translation subprograms

--  function Translate (
--    Source : Wide_String;
--    Mapping : Maps.Character_Mapping)
--    return Wide_String;
   function Translate (
      Source : Wide_String;
      Mapping : Wide_Maps.Wide_Character_Mapping)
      return Wide_String
      renames Wide_Functions.Maps.Translate;

--  procedure Translate (
--    Source : in out Wide_String;
--    Mapping : Maps.Character_Mapping);
   procedure Translate (
      Source : in out Wide_String;
      Mapping : Wide_Maps.Wide_Character_Mapping;
      Drop : Truncation := Error; -- extended
      Justify : Alignment := Left; -- extended
      Pad : Wide_Character := Wide_Space) -- extended
      renames Wide_Functions.Maps.Translate;

--  function Translate (
--    Source : Wide_String;
--    Mapping : Maps.Character_Mapping_Function)
--    return Wide_String;
   function Translate (
      Source : Wide_String;
      Mapping : not null access function (From : Wide_Character)
         return Wide_Character)
      return Wide_String
      renames Wide_Functions.Maps.Translate_Per_Element;
   function Translate (
      Source : Wide_String;
      Mapping : not null access function (From : Wide_Wide_Character)
         return Wide_Wide_Character)
      return Wide_String
      renames Wide_Functions.Maps.Translate;

--  procedure Translate (
--    Source : in out Wide_String;
--    Mapping : Maps.Character_Mapping_Function);
   procedure Translate (
      Source : in out Wide_String;
      Mapping : not null access function (From : Wide_Character)
         return Wide_Character)
      renames Wide_Functions.Maps.Translate_Per_Element;
   procedure Translate (
      Source : in out Wide_String;
      Mapping : not null access function (From : Wide_Wide_Character)
         return Wide_Wide_Character;
      Drop : Truncation := Error; -- extended
      Justify : Alignment := Left; -- extended
      Pad : Wide_Character := Wide_Space) -- extended
      renames Wide_Functions.Maps.Translate;

   --  Wide_String transformation subprograms

   function Replace_Slice (
      Source : Wide_String;
      Low : Positive;
      High : Natural;
      By : Wide_String)
      return Wide_String
      renames Wide_Functions.Replace_Slice;

   procedure Replace_Slice (
      Source : in out Wide_String;
      Low : Positive;
      High : Natural;
      By : Wide_String;
      Drop : Truncation := Error;
      Justify : Alignment := Left;
      Pad : Wide_Character := Wide_Space)
      renames Wide_Functions.Replace_Slice;

   function Insert (
      Source : Wide_String;
      Before : Positive;
      New_Item : Wide_String)
      return Wide_String
      renames Wide_Functions.Insert;

   procedure Insert (
      Source : in out Wide_String;
      Before : Positive;
      New_Item : Wide_String;
      Drop : Truncation := Error)
      renames Wide_Functions.Insert;

   function Overwrite (
      Source : Wide_String;
      Position : Positive;
      New_Item : Wide_String)
      return Wide_String
      renames Wide_Functions.Overwrite;

   procedure Overwrite (
      Source : in out Wide_String;
      Position : Positive;
      New_Item : Wide_String;
      Drop : Truncation := Right)
      renames Wide_Functions.Overwrite;

   function Delete (
      Source : Wide_String;
      From : Positive;
      Through : Natural)
      return Wide_String
      renames Wide_Functions.Delete;

   procedure Delete (
      Source : in out Wide_String;
      From : Positive;
      Through : Natural;
      Justify : Alignment := Left;
      Pad : Wide_Character := Wide_Space)
      renames Wide_Functions.Delete;

   --  Wide_String selector subprograms
--  function Trim (
--    Source : Wide_String;
--    Side : Trim_End)
--    return Wide_String;
   function Trim (
      Source : Wide_String;
      Side : Trim_End;
      Left : Wide_Character := Wide_Space; -- extended
      Right : Wide_Character := Wide_Space) -- extended
      return Wide_String
      renames Wide_Functions.Trim;

--  procedure Trim (
--    Source : in out Wide_String;
--    Side : Trim_End;
--    Justify : Alignment := Left;
--    Pad : Character := Space);
   procedure Trim (
      Source : in out Wide_String;
      Side : Trim_End;
      Left : Wide_Character := Wide_Space; -- extended
      Right : Wide_Character := Wide_Space; -- extended
      Justify : Alignment := Strings.Left;
      Pad : Wide_Character := Wide_Space)
      renames Wide_Functions.Trim;

   function Trim (
      Source : Wide_String;
      Left : Wide_Maps.Wide_Character_Set;
      Right : Wide_Maps.Wide_Character_Set)
      return Wide_String
      renames Wide_Functions.Maps.Trim;

   procedure Trim (
      Source : in out Wide_String;
      Left : Wide_Maps.Wide_Character_Set;
      Right : Wide_Maps.Wide_Character_Set;
      Justify : Alignment := Strings.Left;
      Pad : Wide_Character := Wide_Space)
      renames Wide_Functions.Maps.Trim;

   function Head (
      Source : Wide_String;
      Count : Natural;
      Pad : Wide_Character := Wide_Space)
      return Wide_String
      renames Wide_Functions.Head;

   procedure Head (
      Source : in out Wide_String;
      Count : Natural;
      Justify : Alignment := Left;
      Pad : Wide_Character := Wide_Space)
      renames Wide_Functions.Head;

   function Tail (
      Source : Wide_String;
      Count : Natural;
      Pad : Wide_Character := Wide_Space)
      return Wide_String
      renames Wide_Functions.Tail;

   procedure Tail (
      Source : in out Wide_String;
      Count : Natural;
      Justify : Alignment := Left;
      Pad : Wide_Character := Wide_Space)
      renames Wide_Functions.Tail;

   --  Wide_String constructor functions

   function "*" (Left : Natural; Right : Wide_Character)
      return Wide_String
      renames Wide_Functions."*";

   function "*" (Left : Natural; Right : Wide_String)
      return Wide_String
      renames Wide_Functions."*";

end Ada.Strings.Wide_Fixed;
