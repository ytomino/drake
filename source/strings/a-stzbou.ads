pragma License (Unrestricted);
with Ada.Strings.Bounded_Wide_Wide_Strings.Functions.Maps;
with Ada.Strings.Wide_Wide_Maps;
package Ada.Strings.Wide_Wide_Bounded is
   pragma Preelaborate;

   generic
      Max : Positive; -- Maximum length of a Bounded_String
   package Generic_Bounded_Length is

      --  for renaming
      package Bounded_Wide_Wide_Strings is
         new Strings.Bounded_Wide_Wide_Strings.Generic_Bounded_Length (Max);
      package Functions is
         new Strings.Bounded_Wide_Wide_Strings.Functions
               .Generic_Bounded_Length (
            Bounded_Wide_Wide_Strings);
      package Maps is
         new Strings.Bounded_Wide_Wide_Strings.Functions.Maps
               .Generic_Bounded_Length (
            Bounded_Wide_Wide_Strings);

--    Max_Length : constant Positive := Max;
      Max_Length : Positive
         renames Bounded_Wide_Wide_Strings.Max_Length;

--    type Bounded_Wide_Wide_String is private;
      subtype Bounded_Wide_Wide_String is
         Bounded_Wide_Wide_Strings.Bounded_String;

      --  modified
--    Null_Wide_Wide_Bounded_String : constant Bounded_Wide_Wide_String;
      function Null_Bounded_Wide_Wide_String return Bounded_Wide_Wide_String
         renames Bounded_Wide_Wide_Strings.Null_Bounded_String;

--    subtype Length_Range is Natural range 0 .. Max_Length;
      subtype Length_Range is Bounded_Wide_Wide_Strings.Length_Range;

      function Length (Source : Bounded_Wide_Wide_String) return Length_Range
         renames Bounded_Wide_Wide_Strings.Length;

      --  Conversion, Concatenation, and Selection functions

      function To_Bounded_Wide_Wide_String (
         Source : Wide_Wide_String;
         Drop : Truncation := Error)
         return Bounded_Wide_Wide_String
         renames Bounded_Wide_Wide_Strings.To_Bounded_String;

      function To_Wide_Wide_String (Source : Bounded_Wide_Wide_String)
         return Wide_Wide_String
         renames Bounded_Wide_Wide_Strings.To_String;

      procedure Set_Bounded_Wide_Wide_String (
         Target : out Bounded_Wide_Wide_String;
         Source : Wide_Wide_String;
         Drop : Truncation := Error)
         renames Bounded_Wide_Wide_Strings.Set_Bounded_String;

      function Append (
         Left, Right : Bounded_Wide_Wide_String;
         Drop : Truncation := Error)
         return Bounded_Wide_Wide_String
         renames Bounded_Wide_Wide_Strings.Append;

      function Append (
         Left : Bounded_Wide_Wide_String;
         Right : Wide_Wide_String;
         Drop : Truncation := Error)
         return Bounded_Wide_Wide_String
         renames Bounded_Wide_Wide_Strings.Append;

      function Append (
         Left : Wide_Wide_String;
         Right : Bounded_Wide_Wide_String;
         Drop : Truncation := Error)
         return Bounded_Wide_Wide_String
         renames Bounded_Wide_Wide_Strings.Append;

      function Append (
         Left : Bounded_Wide_Wide_String;
         Right : Wide_Wide_Character;
         Drop : Truncation := Error)
         return Bounded_Wide_Wide_String
         renames Bounded_Wide_Wide_Strings.Append_Element;

      function Append (
         Left : Wide_Wide_Character;
         Right : Bounded_Wide_Wide_String;
         Drop : Truncation := Error)
         return Bounded_Wide_Wide_String
         renames Bounded_Wide_Wide_Strings.Append_Element;

      procedure Append (
         Source : in out Bounded_Wide_Wide_String;
         New_Item : Bounded_Wide_Wide_String;
         Drop : Truncation := Error)
         renames Bounded_Wide_Wide_Strings.Append;

      procedure Append (
         Source : in out Bounded_Wide_Wide_String;
         New_Item : Wide_Wide_String;
         Drop : Truncation := Error)
         renames Bounded_Wide_Wide_Strings.Append;

      procedure Append (
         Source : in out Bounded_Wide_Wide_String;
         New_Item : Wide_Wide_Character;
         Drop : Truncation := Error)
         renames Bounded_Wide_Wide_Strings.Append_Element;

      function "&" (Left, Right : Bounded_Wide_Wide_String)
         return Bounded_Wide_Wide_String
         renames Bounded_Wide_Wide_Strings."&";

      function "&" (Left : Bounded_Wide_Wide_String; Right : Wide_Wide_String)
         return Bounded_Wide_Wide_String
         renames Bounded_Wide_Wide_Strings."&";

      function "&" (Left : Wide_Wide_String; Right : Bounded_Wide_Wide_String)
         return Bounded_Wide_Wide_String
         renames Bounded_Wide_Wide_Strings."&";

      function "&" (
         Left : Bounded_Wide_Wide_String;
         Right : Wide_Wide_Character)
         return Bounded_Wide_Wide_String
         renames Bounded_Wide_Wide_Strings."&";

      function "&" (
         Left : Wide_Wide_Character;
         Right : Bounded_Wide_Wide_String)
         return Bounded_Wide_Wide_String
         renames Bounded_Wide_Wide_Strings."&";

      function Element (
         Source : Bounded_Wide_Wide_String;
         Index : Positive)
         return Wide_Wide_Character
         renames Bounded_Wide_Wide_Strings.Element;

      procedure Replace_Element (
         Source : in out Bounded_Wide_Wide_String;
         Index : Positive;
         By : Wide_Wide_Character)
         renames Bounded_Wide_Wide_Strings.Replace_Element;

      function Slice (
         Source : Bounded_Wide_Wide_String;
         Low : Positive;
         High : Natural)
         return Wide_Wide_String
         renames Bounded_Wide_Wide_Strings.Slice;

      function Bounded_Slice (
         Source : Bounded_Wide_Wide_String;
         Low : Positive;
         High : Natural)
         return Bounded_Wide_Wide_String
         renames Bounded_Wide_Wide_Strings.Bounded_Slice;

      procedure Bounded_Slice (
         Source : Bounded_Wide_Wide_String;
         Target : out Bounded_Wide_Wide_String;
         Low : Positive;
         High : Natural)
         renames Bounded_Wide_Wide_Strings.Bounded_Slice;

      function "=" (Left, Right : Bounded_Wide_Wide_String) return Boolean
         renames Bounded_Wide_Wide_Strings."=";
      function "=" (Left : Bounded_Wide_Wide_String; Right : Wide_Wide_String)
         return Boolean
         renames Bounded_Wide_Wide_Strings."=";

      function "=" (Left : Wide_Wide_String; Right : Bounded_Wide_Wide_String)
         return Boolean
         renames Bounded_Wide_Wide_Strings."=";

      function "<" (Left, Right : Bounded_Wide_Wide_String) return Boolean
         renames Bounded_Wide_Wide_Strings."<";

      function "<" (Left : Bounded_Wide_Wide_String; Right : Wide_Wide_String)
         return Boolean
         renames Bounded_Wide_Wide_Strings."<";

      function "<" (Left : Wide_Wide_String; Right : Bounded_Wide_Wide_String)
         return Boolean
         renames Bounded_Wide_Wide_Strings."<";

      function "<=" (Left, Right : Bounded_Wide_Wide_String) return Boolean
         renames Bounded_Wide_Wide_Strings."<=";

      function "<=" (Left : Bounded_Wide_Wide_String; Right : Wide_Wide_String)
         return Boolean
         renames Bounded_Wide_Wide_Strings."<=";

      function "<=" (Left : Wide_Wide_String; Right : Bounded_Wide_Wide_String)
         return Boolean
         renames Bounded_Wide_Wide_Strings."<=";

      function ">" (Left, Right : Bounded_Wide_Wide_String) return Boolean
         renames Bounded_Wide_Wide_Strings.">";

      function ">" (Left : Bounded_Wide_Wide_String; Right : Wide_Wide_String)
         return Boolean
         renames Bounded_Wide_Wide_Strings.">";

      function ">" (Left : Wide_Wide_String; Right : Bounded_Wide_Wide_String)
         return Boolean
         renames Bounded_Wide_Wide_Strings.">";

      function ">=" (Left, Right : Bounded_Wide_Wide_String) return Boolean
         renames Bounded_Wide_Wide_Strings.">=";

      function ">=" (Left : Bounded_Wide_Wide_String; Right : Wide_Wide_String)
         return Boolean
         renames Bounded_Wide_Wide_Strings.">=";

      function ">=" (Left : Wide_Wide_String; Right : Bounded_Wide_Wide_String)
         return Boolean
         renames Bounded_Wide_Wide_Strings.">=";

      --  Search subprograms

      --  modified
--    function Index (
--       Source : Bounded_Wide_Wide_String;
--       Pattern : Wide_Wide_String;
--       From : Positive;
--       Going : Direction := Forward;
--       Mapping : Maps.Character_Mapping := Maps.Identity)
--       return Natural;
      function Index (
         Source : Bounded_Wide_Wide_String;
         Pattern : Wide_Wide_String;
         From : Positive;
         Going : Direction := Forward)
         return Natural
         renames Functions.Index;
      function Index (
         Source : Bounded_Wide_Wide_String;
         Pattern : Wide_Wide_String;
         From : Positive;
         Going : Direction := Forward;
         Mapping : Strings.Wide_Wide_Maps.Wide_Wide_Character_Mapping)
         return Natural
         renames Maps.Index;

      --  modified
--    function Index (
--       Source : Bounded_Wide_Wide_String;
--       Pattern : Wide_Wide_String;
--       From : Positive;
--       Going : Direction := Forward;
--       Mapping : Maps.Character_Mapping_Function)
--       return Natural;
      function Index (
         Source : Bounded_Wide_Wide_String;
         Pattern : Wide_Wide_String;
         From : Positive;
         Going : Direction := Forward;
         Mapping : not null access function (From : Wide_Wide_Character)
            return Wide_Wide_Character)
         return Natural
         renames Maps.Index_Element;

      --  modified
--    function Index (
--       Source : Bounded_Wide_Wide_String;
--       Pattern : Wide_Wide_String;
--       Going : Direction := Forward;
--       Mapping : Maps.Character_Mapping := Maps.Identity)
--       return Natural;
      function Index (
         Source : Bounded_Wide_Wide_String;
         Pattern : Wide_Wide_String;
         Going : Direction := Forward)
         return Natural
         renames Functions.Index;
      function Index (
         Source : Bounded_Wide_Wide_String;
         Pattern : Wide_Wide_String;
         Going : Direction := Forward;
         Mapping : Strings.Wide_Wide_Maps.Wide_Wide_Character_Mapping)
         return Natural
         renames Maps.Index;

      --  modified
--    function Index (
--       Source : Bounded_Wide_Wide_String;
--       Pattern : Wide_Wide_String;
--       Going : Direction := Forward;
--       Mapping : Maps.Character_Mapping_Function)
--       return Natural;
      function Index (
         Source : Bounded_Wide_Wide_String;
         Pattern : Wide_Wide_String;
         Going : Direction := Forward;
         Mapping : not null access function (From : Wide_Wide_Character)
            return Wide_Wide_Character)
         return Natural
         renames Maps.Index_Element;

      function Index (
         Source : Bounded_Wide_Wide_String;
         Set : Strings.Wide_Wide_Maps.Wide_Wide_Character_Set;
         From : Positive;
         Test : Membership := Inside;
         Going : Direction := Forward)
         return Natural
         renames Maps.Index;

      function Index (
         Source : Bounded_Wide_Wide_String;
         Set : Strings.Wide_Wide_Maps.Wide_Wide_Character_Set;
         Test : Membership := Inside;
         Going : Direction := Forward)
         return Natural
         renames Maps.Index;

      function Index_Non_Blank (
         Source : Bounded_Wide_Wide_String;
         From : Positive;
         Going : Direction := Forward)
         return Natural
         renames Functions.Index_Non_Blank;

      function Index_Non_Blank (
         Source : Bounded_Wide_Wide_String;
         Going : Direction := Forward)
         return Natural
         renames Functions.Index_Non_Blank;

      --  modified
--    function Count (
--       Source : Bounded_Wide_Wide_String;
--       Pattern : Wide_Wide_String;
--       Mapping : Maps.Character_Mapping := Maps.Identity)
--       return Natural;
      function Count (
         Source : Bounded_Wide_Wide_String;
         Pattern : Wide_Wide_String)
         return Natural
         renames Functions.Count;
      function Count (
         Source : Bounded_Wide_Wide_String;
         Pattern : Wide_Wide_String;
         Mapping : Strings.Wide_Wide_Maps.Wide_Wide_Character_Mapping)
         return Natural
         renames Maps.Count;

      --  modified
--    function Count (
--       Source : Bounded_Wide_Wide_String;
--       Pattern : Wide_Wide_String;
--       Mapping : Maps.Character_Mapping_Function)
--       return Natural;
      function Count (
         Source : Bounded_Wide_Wide_String;
         Pattern : Wide_Wide_String;
         Mapping : not null access function (From : Wide_Wide_Character)
            return Wide_Wide_Character)
         return Natural
         renames Maps.Count_Element;

      function Count (
         Source : Bounded_Wide_Wide_String;
         Set : Strings.Wide_Wide_Maps.Wide_Wide_Character_Set)
         return Natural
         renames Maps.Count;

      procedure Find_Token (
         Source : Bounded_Wide_Wide_String;
         Set : Strings.Wide_Wide_Maps.Wide_Wide_Character_Set;
         From : Positive;
         Test : Membership;
         First : out Positive;
         Last : out Natural)
         renames Maps.Find_Token;

      procedure Find_Token (
         Source : Bounded_Wide_Wide_String;
         Set : Strings.Wide_Wide_Maps.Wide_Wide_Character_Set;
         Test : Membership;
         First : out Positive;
         Last : out Natural)
         renames Maps.Find_Token;

      --  Wide_Wide_String translation subprograms

      --  modified
      function Translate (
         Source : Bounded_Wide_Wide_String;
         Mapping : Strings.Wide_Wide_Maps.Wide_Wide_Character_Mapping;
         Drop : Truncation := Error) -- additional
         return Bounded_Wide_Wide_String
         renames Maps.Translate;

      --  modified
      procedure Translate (
         Source : in out Bounded_Wide_Wide_String;
         Mapping : Strings.Wide_Wide_Maps.Wide_Wide_Character_Mapping;
         Drop : Truncation := Error) -- additional
         renames Maps.Translate;

      --  modified
--    function Translate (
--       Source : Bounded_Wide_Wide_String;
--       Mapping : Maps.Character_Mapping_Function)
--       return Bounded_Wide_Wide_String;
      function Translate (
         Source : Bounded_Wide_Wide_String;
         Mapping : not null access function (From : Wide_Wide_Character)
            return Wide_Wide_Character)
         return Bounded_Wide_Wide_String
         renames Maps.Translate_Element;

      --  modified
--    procedure Translate (
--       Source : in out Bounded_Wide_Wide_String;
--       Mapping : Maps.Character_Mapping_Function);
      procedure Translate (
         Source : in out Bounded_Wide_Wide_String;
         Mapping : not null access function (From : Wide_Wide_Character)
            return Wide_Wide_Character)
         renames Maps.Translate_Element;

      --  Wide_Wide_String transformation subprograms

      function Replace_Slice (
         Source : Bounded_Wide_Wide_String;
         Low : Positive;
         High : Natural;
         By : Wide_Wide_String;
         Drop : Truncation := Error)
         return Bounded_Wide_Wide_String
         renames Functions.Replace_Slice;

      procedure Replace_Slice (
         Source : in out Bounded_Wide_Wide_String;
         Low : Positive;
         High : Natural;
         By : Wide_Wide_String;
         Drop : Truncation := Error)
         renames Functions.Replace_Slice;

      function Insert (
         Source : Bounded_Wide_Wide_String;
         Before : Positive;
         New_Item : Wide_Wide_String;
         Drop : Truncation := Error)
         return Bounded_Wide_Wide_String
         renames Functions.Insert;

      procedure Insert (
         Source : in out Bounded_Wide_Wide_String;
         Before : Positive;
         New_Item : Wide_Wide_String;
         Drop : Truncation := Error)
         renames Functions.Insert;

      function Overwrite (
         Source : Bounded_Wide_Wide_String;
         Position : Positive;
         New_Item : Wide_Wide_String;
         Drop : Truncation := Error)
         return Bounded_Wide_Wide_String
         renames Functions.Overwrite;

      procedure Overwrite (
         Source : in out Bounded_Wide_Wide_String;
         Position : Positive;
         New_Item : Wide_Wide_String;
         Drop : Truncation := Error)
         renames Functions.Overwrite;

      function Delete (
         Source : Bounded_Wide_Wide_String;
         From : Positive;
         Through : Natural)
         return Bounded_Wide_Wide_String
         renames Functions.Delete;

      procedure Delete (
         Source : in out Bounded_Wide_Wide_String;
         From : Positive;
         Through : Natural)
         renames Functions.Delete;

      --  Wide_Wide_String selector subprograms

      --  modified
      function Trim (
         Source : Bounded_Wide_Wide_String;
         Side : Trim_End;
         Blank : Wide_Wide_Character := Wide_Wide_Space) -- additional
         return Bounded_Wide_Wide_String
         renames Functions.Trim;
      procedure Trim (
         Source : in out Bounded_Wide_Wide_String;
         Side : Trim_End;
         Blank : Wide_Wide_Character := Wide_Wide_Space) -- additional
         renames Functions.Trim;

      function Trim (
         Source : Bounded_Wide_Wide_String;
         Left : Strings.Wide_Wide_Maps.Wide_Wide_Character_Set;
         Right : Strings.Wide_Wide_Maps.Wide_Wide_Character_Set)
         return Bounded_Wide_Wide_String
         renames Maps.Trim;

      procedure Trim (
         Source : in out Bounded_Wide_Wide_String;
         Left : Strings.Wide_Wide_Maps.Wide_Wide_Character_Set;
         Right : Strings.Wide_Wide_Maps.Wide_Wide_Character_Set)
         renames Maps.Trim;

      function Head (
         Source : Bounded_Wide_Wide_String;
         Count : Natural;
         Pad : Wide_Wide_Character := Wide_Wide_Space;
         Drop : Truncation := Error)
         return Bounded_Wide_Wide_String
         renames Functions.Head;

      procedure Head (
         Source : in out Bounded_Wide_Wide_String;
         Count : Natural;
         Pad : Wide_Wide_Character := Wide_Wide_Space;
         Drop : Truncation := Error)
         renames Functions.Head;

      function Tail (
         Source : Bounded_Wide_Wide_String;
         Count : Natural;
         Pad : Wide_Wide_Character := Wide_Wide_Space;
         Drop : Truncation := Error)
         return Bounded_Wide_Wide_String
         renames Functions.Tail;

      procedure Tail (
         Source : in out Bounded_Wide_Wide_String;
         Count : Natural;
         Pad : Wide_Wide_Character := Wide_Wide_Space;
         Drop : Truncation := Error)
         renames Functions.Tail;

      --  Wide_Wide_String constructor subprograms

      function "*" (Left : Natural; Right : Wide_Wide_Character)
         return Bounded_Wide_Wide_String
         renames Bounded_Wide_Wide_Strings."*";

      function "*" (Left : Natural; Right : Wide_Wide_String)
         return Bounded_Wide_Wide_String
         renames Bounded_Wide_Wide_Strings."*";

      function "*" (Left : Natural; Right : Bounded_Wide_Wide_String)
         return Bounded_Wide_Wide_String
         renames Bounded_Wide_Wide_Strings."*";

      function Replicate (
         Count : Natural;
         Item : Wide_Wide_Character;
         Drop : Truncation := Error)
         return Bounded_Wide_Wide_String
         renames Bounded_Wide_Wide_Strings.Replicate_Element;

      function Replicate (
         Count : Natural;
         Item : Wide_Wide_String;
         Drop : Truncation := Error)
         return Bounded_Wide_Wide_String
         renames Bounded_Wide_Wide_Strings.Replicate;

      function Replicate (
         Count : Natural;
         Item : Bounded_Wide_Wide_String;
         Drop : Truncation := Error)
         return Bounded_Wide_Wide_String
         renames Bounded_Wide_Wide_Strings.Replicate;

   end Generic_Bounded_Length;

end Ada.Strings.Wide_Wide_Bounded;
