pragma License (Unrestricted);
with Ada.Strings.Generic_Bounded;
with Ada.Strings.Wide_Functions;
with Ada.Strings.Wide_Functions.Maps;
with Ada.Strings.Wide_Maps;
package Ada.Strings.Wide_Bounded is
   pragma Preelaborate;

   --  Generic_Bounded is not able to use directly since some names differ.
   package Instance is new Generic_Bounded (
      Character_Type => Wide_Character,
      String_Type => Wide_String);

   generic
      Max : Positive; -- Maximum length of a Bounded_Wide_String
   package Generic_Bounded_Length is

      --  for renaming
      package Instance is new Instance.Generic_Bounded_Length (Max);
      package Functions is new Instance.Generic_Functions (
         Space => Wide_Space,
         Fixed_Index_From => Strings.Wide_Functions.Index,
         Fixed_Index => Strings.Wide_Functions.Index,
         Fixed_Index_Non_Blank_From => Strings.Wide_Functions.Index_Non_Blank,
         Fixed_Index_Non_Blank => Strings.Wide_Functions.Index_Non_Blank,
         Fixed_Count => Strings.Wide_Functions.Count,
         Fixed_Replace_Slice => Strings.Wide_Functions.Replace_Slice,
         Fixed_Insert => Strings.Wide_Functions.Insert,
         Fixed_Overwrite => Strings.Wide_Functions.Overwrite,
         Fixed_Delete => Strings.Wide_Functions.Delete,
         Fixed_Trim => Strings.Wide_Functions.Trim,
         Fixed_Head => Strings.Wide_Functions.Head,
         Fixed_Tail => Strings.Wide_Functions.Tail);
      package Maps is new Functions.Generic_Maps (
         Character_Set => Strings.Wide_Maps.Wide_Character_Set,
         Character_Mapping => Strings.Wide_Maps.Wide_Character_Mapping,
         Fixed_Index_Mapping_From => Strings.Wide_Functions.Maps.Index,
         Fixed_Index_Mapping => Strings.Wide_Functions.Maps.Index,
         Fixed_Index_Mapping_Function_From =>
            Strings.Wide_Functions.Maps.Index,
         Fixed_Index_Mapping_Function => Strings.Wide_Functions.Maps.Index,
         Fixed_Index_Mapping_Function_Per_Element_From =>
            Strings.Wide_Functions.Maps.Index_Per_Element,
         Fixed_Index_Mapping_Function_Per_Element =>
            Strings.Wide_Functions.Maps.Index_Per_Element,
         Fixed_Index_Set_From => Strings.Wide_Functions.Maps.Index,
         Fixed_Index_Set => Strings.Wide_Functions.Maps.Index,
         Fixed_Count_Mapping => Strings.Wide_Functions.Maps.Count,
         Fixed_Count_Mapping_Function => Strings.Wide_Functions.Maps.Count,
         Fixed_Count_Mapping_Function_Per_Element =>
            Strings.Wide_Functions.Maps.Count_Per_Element,
         Fixed_Count_Set => Strings.Wide_Functions.Maps.Count,
         Fixed_Find_Token_From => Strings.Wide_Functions.Maps.Find_Token,
         Fixed_Find_Token => Strings.Wide_Functions.Maps.Find_Token,
         Fixed_Translate_Mapping => Strings.Wide_Functions.Maps.Translate,
         Fixed_Translate_Mapping_Function =>
            Strings.Wide_Functions.Maps.Translate,
         Fixed_Translate_Mapping_Function_Per_Element =>
            Strings.Wide_Functions.Maps.Translate_Per_Element,
         Fixed_Trim_Set => Strings.Wide_Functions.Maps.Trim);

--    Max_Length : constant Positive := Max;
      Max_Length : Positive renames Instance.Max_Length;

--    type Bounded_Wide_String is private;
      subtype Bounded_Wide_String is Instance.Bounded_String;

--    Null_Wide_Bounded_String : constant Bounded_Wide_String;
      function Null_Bounded_Wide_String return Bounded_Wide_String
         renames Instance.Null_Bounded_String;

--    subtype Length_Range is Natural range 0 .. Max_Length;
      subtype Length_Range is Instance.Length_Range;

      function Length (Source : Bounded_Wide_String) return Length_Range
         renames Instance.Length;

      --  Conversion, Concatenation, and Selection functions

      function To_Bounded_Wide_String (
         Source : Wide_String;
         Drop : Truncation := Error)
         return Bounded_Wide_String
         renames Instance.To_Bounded_String;

      function To_Wide_String (Source : Bounded_Wide_String)
         return Wide_String
         renames Instance.To_String;

      procedure Set_Bounded_Wide_String (
         Target : out Bounded_Wide_String;
         Source : Wide_String;
         Drop : Truncation := Error)
         renames Instance.Set_Bounded_String;

      function Append (
         Left, Right : Bounded_Wide_String;
         Drop : Truncation := Error)
         return Bounded_Wide_String
         renames Instance.Append;

      function Append (
         Left : Bounded_Wide_String;
         Right : Wide_String;
         Drop : Truncation := Error)
         return Bounded_Wide_String
         renames Instance.Append;

      function Append (
         Left : Wide_String;
         Right : Bounded_Wide_String;
         Drop : Truncation := Error)
         return Bounded_Wide_String
         renames Instance.Append;

      function Append (
         Left : Bounded_Wide_String;
         Right : Wide_Character;
         Drop : Truncation := Error)
         return Bounded_Wide_String
         renames Instance.Append;

      function Append (
         Left : Wide_Character;
         Right : Bounded_Wide_String;
         Drop : Truncation := Error)
         return Bounded_Wide_String
         renames Instance.Append;

      procedure Append (
         Source : in out Bounded_Wide_String;
         New_Item : Bounded_Wide_String;
         Drop : Truncation := Error)
         renames Instance.Append;

      procedure Append (
         Source : in out Bounded_Wide_String;
         New_Item : Wide_String;
         Drop : Truncation := Error)
         renames Instance.Append;

      procedure Append (
         Source : in out Bounded_Wide_String;
         New_Item : Wide_Character;
         Drop : Truncation := Error)
         renames Instance.Append;

      function "&" (Left, Right : Bounded_Wide_String)
         return Bounded_Wide_String
         renames Instance."&";

      function "&" (Left : Bounded_Wide_String; Right : Wide_String)
         return Bounded_Wide_String
         renames Instance."&";

      function "&" (Left : Wide_String; Right : Bounded_Wide_String)
         return Bounded_Wide_String
         renames Instance."&";

      function "&" (Left : Bounded_Wide_String; Right : Wide_Character)
         return Bounded_Wide_String
         renames Instance."&";

      function "&" (Left : Wide_Character; Right : Bounded_Wide_String)
         return Bounded_Wide_String
         renames Instance."&";

      function Element (
         Source : Bounded_Wide_String;
         Index : Positive)
         return Wide_Character
         renames Instance.Element;

      procedure Replace_Element (
         Source : in out Bounded_Wide_String;
         Index : Positive;
         By : Wide_Character)
         renames Instance.Replace_Element;

      function Slice (
         Source : Bounded_Wide_String;
         Low : Positive;
         High : Natural)
         return Wide_String
         renames Instance.Slice;

      function Bounded_Slice (
         Source : Bounded_Wide_String;
         Low : Positive;
         High : Natural)
         return Bounded_Wide_String
         renames Instance.Bounded_Slice;

      procedure Bounded_Slice (
         Source : Bounded_Wide_String;
         Target : out Bounded_Wide_String;
         Low : Positive;
         High : Natural)
         renames Instance.Bounded_Slice;

      function "=" (Left, Right : Bounded_Wide_String) return Boolean
         renames Instance."=";
      --  In CXA4029, conflicted by "use" and "use types"
      function "=" (Left : Bounded_Wide_String; Right : Wide_String)
         return Boolean
         renames Instance."=";

      function "=" (Left : Wide_String; Right : Bounded_Wide_String)
         return Boolean
         renames Instance."=";

      function "<" (Left, Right : Bounded_Wide_String) return Boolean
         renames Instance."<";

      function "<" (Left : Bounded_Wide_String; Right : Wide_String)
         return Boolean
         renames Instance."<";

      function "<" (Left : Wide_String; Right : Bounded_Wide_String)
         return Boolean
         renames Instance."<";

      function "<=" (Left, Right : Bounded_Wide_String) return Boolean
         renames Instance."<=";

      function "<=" (Left : Bounded_Wide_String; Right : Wide_String)
         return Boolean
         renames Instance."<=";

      function "<=" (Left : Wide_String; Right : Bounded_Wide_String)
         return Boolean
         renames Instance."<=";

      function ">" (Left, Right : Bounded_Wide_String) return Boolean
         renames Instance.">";

      function ">" (Left : Bounded_Wide_String; Right : Wide_String)
         return Boolean
         renames Instance.">";

      function ">" (Left : Wide_String; Right : Bounded_Wide_String)
         return Boolean
         renames Instance.">";

      function ">=" (Left, Right : Bounded_Wide_String) return Boolean
         renames Instance.">=";

      function ">=" (Left : Bounded_Wide_String; Right : Wide_String)
         return Boolean
         renames Instance.">=";

      function ">=" (Left : Wide_String; Right : Bounded_Wide_String)
         return Boolean
         renames Instance.">=";

      --  Search subprograms

--    function Index (
--       Source : Bounded_Wide_String;
--       Pattern : Wide_String;
--       From : Positive;
--       Going : Direction := Forward;
--       Mapping : Maps.Character_Mapping := Maps.Identity)
--       return Natural;
      function Index (
         Source : Bounded_Wide_String;
         Pattern : Wide_String;
         From : Positive;
         Going : Direction := Forward)
         return Natural
         renames Functions.Index;
      function Index (
         Source : Bounded_Wide_String;
         Pattern : Wide_String;
         From : Positive;
         Going : Direction := Forward;
         Mapping : Strings.Wide_Maps.Wide_Character_Mapping)
         return Natural
         renames Maps.Index;

--    function Index (
--       Source : Bounded_Wide_String;
--       Pattern : Wide_String;
--       From : Positive;
--       Going : Direction := Forward;
--       Mapping : Maps.Character_Mapping_Function)
--       return Natural;
      function Index (
         Source : Bounded_Wide_String;
         Pattern : Wide_String;
         From : Positive;
         Going : Direction := Forward;
         Mapping : not null access function (From : Wide_Character)
            return Wide_Character)
         return Natural
         renames Maps.Index_Per_Element;
      function Index (
         Source : Bounded_Wide_String;
         Pattern : Wide_String;
         From : Positive;
         Going : Direction := Forward;
         Mapping : not null access function (From : Wide_Wide_Character)
            return Wide_Wide_Character)
         return Natural
         renames Maps.Index;

--    function Index (
--       Source : Bounded_Wide_String;
--       Pattern : Wide_String;
--       Going : Direction := Forward;
--       Mapping : Maps.Character_Mapping := Maps.Identity)
--       return Natural;
      function Index (
         Source : Bounded_Wide_String;
         Pattern : Wide_String;
         Going : Direction := Forward)
         return Natural
         renames Functions.Index;
      function Index (
         Source : Bounded_Wide_String;
         Pattern : Wide_String;
         Going : Direction := Forward;
         Mapping : Strings.Wide_Maps.Wide_Character_Mapping)
         return Natural
         renames Maps.Index;

--    function Index (
--       Source : Bounded_Wide_String;
--       Pattern : Wide_String;
--       Going : Direction := Forward;
--       Mapping : Maps.Character_Mapping_Function)
--       return Natural;
      function Index (
         Source : Bounded_Wide_String;
         Pattern : Wide_String;
         Going : Direction := Forward;
         Mapping : not null access function (From : Wide_Character)
            return Wide_Character)
         return Natural
         renames Maps.Index_Per_Element;
      function Index (
         Source : Bounded_Wide_String;
         Pattern : Wide_String;
         Going : Direction := Forward;
         Mapping : not null access function (From : Wide_Wide_Character)
            return Wide_Wide_Character)
         return Natural
         renames Maps.Index;

      function Index (
         Source : Bounded_Wide_String;
         Set : Strings.Wide_Maps.Wide_Character_Set;
         From : Positive;
         Test : Membership := Inside;
         Going : Direction := Forward)
         return Natural
         renames Maps.Index;

      function Index (
         Source : Bounded_Wide_String;
         Set : Strings.Wide_Maps.Wide_Character_Set;
         Test : Membership := Inside;
         Going : Direction := Forward)
         return Natural
         renames Maps.Index;

      function Index_Non_Blank (
         Source : Bounded_Wide_String;
         From : Positive;
         Going : Direction := Forward)
         return Natural
         renames Functions.Index_Non_Blank;

      function Index_Non_Blank (
         Source : Bounded_Wide_String;
         Going : Direction := Forward)
         return Natural
         renames Functions.Index_Non_Blank;

--    function Count (
--       Source : Bounded_Wide_String;
--       Pattern : Wide_String;
--       Mapping : Maps.Character_Mapping := Maps.Identity)
--       return Natural;
      function Count (
         Source : Bounded_Wide_String;
         Pattern : Wide_String)
         return Natural
         renames Functions.Count;
      function Count (
         Source : Bounded_Wide_String;
         Pattern : Wide_String;
         Mapping : Strings.Wide_Maps.Wide_Character_Mapping)
         return Natural
         renames Maps.Count;

--    function Count (
--       Source : Bounded_Wide_String;
--       Pattern : Wide_String;
--       Mapping : Maps.Character_Mapping_Function)
--       return Natural;
      function Count (
         Source : Bounded_Wide_String;
         Pattern : Wide_String;
         Mapping : not null access function (From : Wide_Character)
            return Wide_Character)
         return Natural
         renames Maps.Count_Per_Element;
      function Count (
         Source : Bounded_Wide_String;
         Pattern : Wide_String;
         Mapping : not null access function (From : Wide_Wide_Character)
            return Wide_Wide_Character)
         return Natural
         renames Maps.Count;

      function Count (
         Source : Bounded_Wide_String;
         Set : Strings.Wide_Maps.Wide_Character_Set)
         return Natural
         renames Maps.Count;

      procedure Find_Token (
         Source : Bounded_Wide_String;
         Set : Strings.Wide_Maps.Wide_Character_Set;
         From : Positive;
         Test : Membership;
         First : out Positive;
         Last : out Natural)
         renames Maps.Find_Token;

      procedure Find_Token (
         Source : Bounded_Wide_String;
         Set : Strings.Wide_Maps.Wide_Character_Set;
         Test : Membership;
         First : out Positive;
         Last : out Natural)
         renames Maps.Find_Token;

      --  Wide_String translation subprograms

      function Translate (
         Source : Bounded_Wide_String;
         Mapping : Strings.Wide_Maps.Wide_Character_Mapping;
         Drop : Truncation := Error) -- extended
         return Bounded_Wide_String
         renames Maps.Translate;

      procedure Translate (
         Source : in out Bounded_Wide_String;
         Mapping : Strings.Wide_Maps.Wide_Character_Mapping;
         Drop : Truncation := Error) -- extended
         renames Maps.Translate;

--    function Translate (
--       Source : Bounded_Wide_String;
--       Mapping : Maps.Character_Mapping_Function)
--       return Bounded_Wide_String;
      function Translate (
         Source : Bounded_Wide_String;
         Mapping : not null access function (From : Wide_Character)
            return Wide_Character)
         return Bounded_Wide_String
         renames Maps.Translate_Per_Element;
      function Translate (
         Source : Bounded_Wide_String;
         Mapping : not null access function (From : Wide_Wide_Character)
            return Wide_Wide_Character;
         Drop : Truncation := Error) -- extended
         return Bounded_Wide_String
         renames Maps.Translate;

--    procedure Translate (
--       Source : in out Bounded_Wide_String;
--       Mapping : Maps.Character_Mapping_Function);
      procedure Translate (
         Source : in out Bounded_Wide_String;
         Mapping : not null access function (From : Wide_Character)
            return Wide_Character)
         renames Maps.Translate_Per_Element;
      procedure Translate (
         Source : in out Bounded_Wide_String;
         Mapping : not null access function (From : Wide_Wide_Character)
            return Wide_Wide_Character;
         Drop : Truncation := Error) -- extended
         renames Maps.Translate;

      --  Wide_String transformation subprograms

      function Replace_Slice (
         Source : Bounded_Wide_String;
         Low : Positive;
         High : Natural;
         By : Wide_String;
         Drop : Truncation := Error)
         return Bounded_Wide_String
         renames Functions.Replace_Slice;

      procedure Replace_Slice (
         Source : in out Bounded_Wide_String;
         Low : Positive;
         High : Natural;
         By : Wide_String;
         Drop : Truncation := Error)
         renames Functions.Replace_Slice;

      function Insert (
         Source : Bounded_Wide_String;
         Before : Positive;
         New_Item : Wide_String;
         Drop : Truncation := Error)
         return Bounded_Wide_String
         renames Functions.Insert;

      procedure Insert (
         Source : in out Bounded_Wide_String;
         Before : Positive;
         New_Item : Wide_String;
         Drop : Truncation := Error)
         renames Functions.Insert;

      function Overwrite (
         Source : Bounded_Wide_String;
         Position : Positive;
         New_Item : Wide_String;
         Drop : Truncation := Error)
         return Bounded_Wide_String
         renames Functions.Overwrite;

      procedure Overwrite (
         Source : in out Bounded_Wide_String;
         Position : Positive;
         New_Item : Wide_String;
         Drop : Truncation := Error)
         renames Functions.Overwrite;

      function Delete (
         Source : Bounded_Wide_String;
         From : Positive;
         Through : Natural)
         return Bounded_Wide_String
         renames Functions.Delete;

      procedure Delete (
         Source : in out Bounded_Wide_String;
         From : Positive;
         Through : Natural)
         renames Functions.Delete;

      --  Wide_String selector subprograms

      function Trim (
         Source : Bounded_Wide_String;
         Side : Trim_End;
         Left : Wide_Character := Wide_Space;
         Right : Wide_Character := Wide_Space)
         return Bounded_Wide_String
         renames Functions.Trim;
      procedure Trim (
         Source : in out Bounded_Wide_String;
         Side : Trim_End;
         Left : Wide_Character := Wide_Space;
         Right : Wide_Character := Wide_Space)
         renames Functions.Trim;

      function Trim (
         Source : Bounded_Wide_String;
         Left : Strings.Wide_Maps.Wide_Character_Set;
         Right : Strings.Wide_Maps.Wide_Character_Set)
         return Bounded_Wide_String
         renames Maps.Trim;

      procedure Trim (
         Source : in out Bounded_Wide_String;
         Left : Strings.Wide_Maps.Wide_Character_Set;
         Right : Strings.Wide_Maps.Wide_Character_Set)
         renames Maps.Trim;

      function Head (
         Source : Bounded_Wide_String;
         Count : Natural;
         Pad : Wide_Character := Wide_Space;
         Drop : Truncation := Error)
         return Bounded_Wide_String
         renames Functions.Head;

      procedure Head (
         Source : in out Bounded_Wide_String;
         Count : Natural;
         Pad : Wide_Character := Wide_Space;
         Drop : Truncation := Error)
         renames Functions.Head;

      function Tail (
         Source : Bounded_Wide_String;
         Count : Natural;
         Pad : Wide_Character := Wide_Space;
         Drop : Truncation := Error)
         return Bounded_Wide_String
         renames Functions.Tail;

      procedure Tail (
         Source : in out Bounded_Wide_String;
         Count : Natural;
         Pad : Wide_Character := Wide_Space;
         Drop : Truncation := Error)
         renames Functions.Tail;

      --  Wide_String constructor subprograms

      function "*" (Left : Natural; Right : Wide_Character)
         return Bounded_Wide_String
         renames Instance."*";

      function "*" (Left : Natural; Right : Wide_String)
         return Bounded_Wide_String
         renames Instance."*";

      function "*" (Left : Natural; Right : Bounded_Wide_String)
         return Bounded_Wide_String
         renames Instance."*";

      function Replicate (
         Count : Natural;
         Item : Wide_Character;
         Drop : Truncation := Error)
         return Bounded_Wide_String
         renames Instance.Replicate;

      function Replicate (
         Count : Natural;
         Item : Wide_String;
         Drop : Truncation := Error)
         return Bounded_Wide_String
         renames Instance.Replicate;

      function Replicate (
         Count : Natural;
         Item : Bounded_Wide_String;
         Drop : Truncation := Error)
         return Bounded_Wide_String
         renames Instance.Replicate;

   end Generic_Bounded_Length;

end Ada.Strings.Wide_Bounded;
