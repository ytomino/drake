pragma License (Unrestricted);
with Ada.Strings.Functions;
with Ada.Strings.Functions.Maps;
with Ada.Strings.Generic_Bounded;
with Ada.Strings.Maps;
package Ada.Strings.Bounded is
   pragma Preelaborate;

   --  instantiate Generic_Bounded
   package Instance is new Generic_Bounded (
      Character_Type => Character,
      String_Type => String);

   generic
      Max : Positive; -- Maximum length of a Bounded_String
   package Generic_Bounded_Length is

      --  for renaming
      package Instance is new Bounded.Instance.Generic_Bounded_Length (Max);
      package Functions is new Instance.Generic_Functions (
         Space => Space,
         Fixed_Index_From => Strings.Functions.Index,
         Fixed_Index => Strings.Functions.Index,
         Fixed_Index_Non_Blank_From => Strings.Functions.Index_Non_Blank,
         Fixed_Index_Non_Blank => Strings.Functions.Index_Non_Blank,
         Fixed_Count => Strings.Functions.Count,
         Fixed_Replace_Slice => Strings.Functions.Replace_Slice,
         Fixed_Insert => Strings.Functions.Insert,
         Fixed_Overwrite => Strings.Functions.Overwrite,
         Fixed_Delete => Strings.Functions.Delete,
         Fixed_Trim => Strings.Functions.Trim,
         Fixed_Head => Strings.Functions.Head,
         Fixed_Tail => Strings.Functions.Tail);
      package Maps is new Functions.Generic_Maps (
         Character_Set => Strings.Maps.Character_Set,
         Character_Mapping => Strings.Maps.Character_Mapping,
         Fixed_Index_Mapping_From => Strings.Functions.Maps.Index,
         Fixed_Index_Mapping => Strings.Functions.Maps.Index,
         Fixed_Index_Mapping_Function_From => Strings.Functions.Maps.Index,
         Fixed_Index_Mapping_Function => Strings.Functions.Maps.Index,
         Fixed_Index_Mapping_Function_Per_Element_From =>
            Strings.Functions.Maps.Index_Per_Element,
         Fixed_Index_Mapping_Function_Per_Element =>
            Strings.Functions.Maps.Index_Per_Element,
         Fixed_Index_Set_From => Strings.Functions.Maps.Index,
         Fixed_Index_Set => Strings.Functions.Maps.Index,
         Fixed_Count_Mapping => Strings.Functions.Maps.Count,
         Fixed_Count_Mapping_Function => Strings.Functions.Maps.Count,
         Fixed_Count_Mapping_Function_Per_Element =>
            Strings.Functions.Maps.Count_Per_Element,
         Fixed_Count_Set => Strings.Functions.Maps.Count,
         Fixed_Find_Token_From => Strings.Functions.Maps.Find_Token,
         Fixed_Find_Token => Strings.Functions.Maps.Find_Token,
         Fixed_Translate_Mapping => Strings.Functions.Maps.Translate,
         Fixed_Translate_Mapping_Function => Strings.Functions.Maps.Translate,
         Fixed_Translate_Mapping_Function_Per_Element =>
            Strings.Functions.Maps.Translate_Per_Element,
         Fixed_Trim_Set => Strings.Functions.Maps.Trim);

--    Max_Length : constant Positive := Max;
      Max_Length : Positive renames Instance.Max_Length;

--    type Bounded_String is private;
      subtype Bounded_String is Instance.Bounded_String;

--    Null_Bounded_String : constant Bounded_String;
      function Null_Bounded_String return Bounded_String
         renames Instance.Null_Bounded_String;

--    subtype Length_Range is Natural range 0 .. Max_Length;
      subtype Length_Range is Instance.Length_Range;

      function Length (Source : Bounded_String) return Length_Range
         renames Instance.Length;

      --  Conversion, Concatenation, and Selection functions

      function To_Bounded_String (Source : String; Drop : Truncation := Error)
         return Bounded_String
         renames Instance.To_Bounded_String;

      function To_String (Source : Bounded_String) return String
         renames Instance.To_String;

      procedure Set_Bounded_String (
         Target : out Bounded_String;
         Source : String;
         Drop : Truncation := Error)
         renames Instance.Set_Bounded_String;

      function Append (
         Left, Right : Bounded_String;
         Drop : Truncation := Error)
         return Bounded_String
         renames Instance.Append;

      function Append (
         Left : Bounded_String;
         Right : String;
         Drop : Truncation := Error)
         return Bounded_String
         renames Instance.Append;

      function Append (
         Left : String;
         Right : Bounded_String;
         Drop : Truncation := Error)
         return Bounded_String
         renames Instance.Append;

      function Append (
         Left : Bounded_String;
         Right : Character;
         Drop : Truncation := Error)
         return Bounded_String
         renames Instance.Append;

      function Append (
         Left : Character;
         Right : Bounded_String;
         Drop : Truncation := Error)
         return Bounded_String
         renames Instance.Append;

      procedure Append (
         Source : in out Bounded_String;
         New_Item : Bounded_String;
         Drop : Truncation := Error)
         renames Instance.Append;

      procedure Append (
         Source : in out Bounded_String;
         New_Item : String;
         Drop : Truncation := Error)
         renames Instance.Append;

      procedure Append (
         Source : in out Bounded_String;
         New_Item : Character;
         Drop : Truncation := Error)
         renames Instance.Append;

      function "&" (Left, Right : Bounded_String)
         return Bounded_String
         renames Instance."&";

      function "&" (Left : Bounded_String; Right : String)
         return Bounded_String
         renames Instance."&";

      function "&" (Left : String; Right : Bounded_String)
         return Bounded_String
         renames Instance."&";

      function "&" (Left : Bounded_String; Right : Character)
         return Bounded_String
         renames Instance."&";

      function "&" (Left : Character; Right : Bounded_String)
         return Bounded_String
         renames Instance."&";

      function Element (
         Source : Bounded_String;
         Index : Positive)
         return Character
         renames Instance.Element;

      procedure Replace_Element (
         Source : in out Bounded_String;
         Index : Positive;
         By : Character)
         renames Instance.Replace_Element;

      function Slice (
         Source : Bounded_String;
         Low : Positive;
         High : Natural)
         return String
         renames Instance.Slice;

      function Bounded_Slice (
         Source : Bounded_String;
         Low : Positive;
         High : Natural)
         return Bounded_String
         renames Instance.Bounded_Slice;

      procedure Bounded_Slice (
         Source : Bounded_String;
         Target : out Bounded_String;
         Low : Positive;
         High : Natural)
         renames Instance.Bounded_Slice;

      function "=" (Left, Right : Bounded_String) return Boolean
         renames Instance."=";
      --  In CXA4028, conflicted by "use" and "use types"
      --  but CXA5011 requires this.
      function "=" (Left : Bounded_String; Right : String) return Boolean
         renames Instance."=";

      function "=" (Left : String; Right : Bounded_String) return Boolean
         renames Instance."=";

      function "<" (Left, Right : Bounded_String) return Boolean
         renames Instance."<";

      function "<" (Left : Bounded_String; Right : String) return Boolean
         renames Instance."<";

      function "<" (Left : String; Right : Bounded_String) return Boolean
         renames Instance."<";

      function "<=" (Left, Right : Bounded_String) return Boolean
         renames Instance."<=";

      function "<=" (Left : Bounded_String; Right : String) return Boolean
         renames Instance."<=";

      function "<=" (Left : String; Right : Bounded_String) return Boolean
         renames Instance."<=";

      function ">" (Left, Right : Bounded_String) return Boolean
         renames Instance.">";

      function ">" (Left : Bounded_String; Right : String) return Boolean
         renames Instance.">";

      function ">" (Left : String; Right : Bounded_String) return Boolean
         renames Instance.">";

      function ">=" (Left, Right : Bounded_String) return Boolean
         renames Instance.">=";

      function ">=" (Left : Bounded_String; Right : String) return Boolean
         renames Instance.">=";

      function ">=" (Left : String; Right : Bounded_String) return Boolean
         renames Instance.">=";

      --  Search subprograms

--    function Index (
--       Source : Bounded_String;
--       Pattern : String;
--       From : Positive;
--       Going : Direction := Forward;
--       Mapping : Maps.Character_Mapping := Maps.Identity)
--       return Natural;
      function Index (
         Source : Bounded_String;
         Pattern : String;
         From : Positive;
         Going : Direction := Forward)
         return Natural
         renames Functions.Index;
      function Index (
         Source : Bounded_String;
         Pattern : String;
         From : Positive;
         Going : Direction := Forward;
         Mapping : Strings.Maps.Character_Mapping)
         return Natural
         renames Maps.Index;

--    function Index (
--       Source : Bounded_String;
--       Pattern : String;
--       From : Positive;
--       Going : Direction := Forward;
--       Mapping : Maps.Character_Mapping_Function)
--       return Natural;
      function Index (
         Source : Bounded_String;
         Pattern : String;
         From : Positive;
         Going : Direction := Forward;
         Mapping : not null access function (From : Character)
            return Character)
         return Natural
         renames Maps.Index_Per_Element;
      function Index (
         Source : Bounded_String;
         Pattern : String;
         From : Positive;
         Going : Direction := Forward;
         Mapping : not null access function (From : Wide_Wide_Character)
            return Wide_Wide_Character)
         return Natural
         renames Maps.Index;

--    function Index (
--       Source : Bounded_String;
--       Pattern : String;
--       Going : Direction := Forward;
--       Mapping : Maps.Character_Mapping := Maps.Identity)
--       return Natural;
      function Index (
         Source : Bounded_String;
         Pattern : String;
         Going : Direction := Forward)
         return Natural
         renames Functions.Index;
      function Index (
         Source : Bounded_String;
         Pattern : String;
         Going : Direction := Forward;
         Mapping : Strings.Maps.Character_Mapping)
         return Natural
         renames Maps.Index;

--    function Index (
--       Source : Bounded_String;
--       Pattern : String;
--       Going : Direction := Forward;
--       Mapping : Maps.Character_Mapping_Function)
--       return Natural;
      function Index (
         Source : Bounded_String;
         Pattern : String;
         Going : Direction := Forward;
         Mapping : not null access function (From : Character)
            return Character)
         return Natural
         renames Maps.Index_Per_Element;
      function Index (
         Source : Bounded_String;
         Pattern : String;
         Going : Direction := Forward;
         Mapping : not null access function (From : Wide_Wide_Character)
            return Wide_Wide_Character)
         return Natural
         renames Maps.Index;

      function Index (
         Source : Bounded_String;
         Set : Strings.Maps.Character_Set;
         From : Positive;
         Test : Membership := Inside;
         Going : Direction := Forward)
         return Natural
         renames Maps.Index;

      function Index (
         Source : Bounded_String;
         Set : Strings.Maps.Character_Set;
         Test : Membership := Inside;
         Going : Direction := Forward)
         return Natural
         renames Maps.Index;

      function Index_Non_Blank (
         Source : Bounded_String;
         From : Positive;
         Going : Direction := Forward)
         return Natural
         renames Functions.Index_Non_Blank;

      function Index_Non_Blank (
         Source : Bounded_String;
         Going : Direction := Forward)
         return Natural
         renames Functions.Index_Non_Blank;

--    function Count (
--       Source : Bounded_String;
--       Pattern : String;
--       Mapping : Maps.Character_Mapping := Maps.Identity)
--       return Natural;
      function Count (
         Source : Bounded_String;
         Pattern : String)
         return Natural
         renames Functions.Count;
      function Count (
         Source : Bounded_String;
         Pattern : String;
         Mapping : Strings.Maps.Character_Mapping)
         return Natural
         renames Maps.Count;

--    function Count (
--       Source : Bounded_String;
--       Pattern : String;
--       Mapping : Maps.Character_Mapping_Function)
--       return Natural;
      function Count (
         Source : Bounded_String;
         Pattern : String;
         Mapping : not null access function (From : Character)
            return Character)
         return Natural
         renames Maps.Count_Per_Element;
      function Count (
         Source : Bounded_String;
         Pattern : String;
         Mapping : not null access function (From : Wide_Wide_Character)
            return Wide_Wide_Character)
         return Natural
         renames Maps.Count;

      function Count (
         Source : Bounded_String;
         Set : Strings.Maps.Character_Set)
         return Natural
         renames Maps.Count;

      procedure Find_Token (
         Source : Bounded_String;
         Set : Strings.Maps.Character_Set;
         From : Positive;
         Test : Membership;
         First : out Positive;
         Last : out Natural)
         renames Maps.Find_Token;

      procedure Find_Token (
         Source : Bounded_String;
         Set : Strings.Maps.Character_Set;
         Test : Membership;
         First : out Positive;
         Last : out Natural)
         renames Maps.Find_Token;

      --  String translation subprograms

      function Translate (
         Source : Bounded_String;
         Mapping : Strings.Maps.Character_Mapping;
         Drop : Truncation := Error) -- extended
         return Bounded_String
         renames Maps.Translate;

      procedure Translate (
         Source : in out Bounded_String;
         Mapping : Strings.Maps.Character_Mapping;
         Drop : Truncation := Error) -- extended
         renames Maps.Translate;

--    function Translate (
--       Source : Bounded_String;
--       Mapping : Maps.Character_Mapping_Function)
--       return Bounded_String;
      function Translate (
         Source : Bounded_String;
         Mapping : not null access function (From : Character)
            return Character)
         return Bounded_String
         renames Maps.Translate_Per_Element;
      function Translate (
         Source : Bounded_String;
         Mapping : not null access function (From : Wide_Wide_Character)
            return Wide_Wide_Character;
         Drop : Truncation := Error)
         return Bounded_String
         renames Maps.Translate;

--    procedure Translate (
--       Source : in out Bounded_String;
--       Mapping : Maps.Character_Mapping_Function);
      procedure Translate (
         Source : in out Bounded_String;
         Mapping : not null access function (From : Character)
            return Character)
         renames Maps.Translate_Per_Element;
      procedure Translate (
         Source : in out Bounded_String;
         Mapping : not null access function (From : Wide_Wide_Character)
            return Wide_Wide_Character;
         Drop : Truncation := Error)
         renames Maps.Translate;

      --  String transformation subprograms

      function Replace_Slice (
         Source : Bounded_String;
         Low : Positive;
         High : Natural;
         By : String;
         Drop : Truncation := Error)
         return Bounded_String
         renames Functions.Replace_Slice;

      procedure Replace_Slice (
         Source : in out Bounded_String;
         Low : Positive;
         High : Natural;
         By : String;
         Drop : Truncation := Error)
         renames Functions.Replace_Slice;

      function Insert (
         Source : Bounded_String;
         Before : Positive;
         New_Item : String;
         Drop : Truncation := Error)
         return Bounded_String
         renames Functions.Insert;

      procedure Insert (
         Source : in out Bounded_String;
         Before : Positive;
         New_Item : String;
         Drop : Truncation := Error)
         renames Functions.Insert;

      function Overwrite (
         Source : Bounded_String;
         Position : Positive;
         New_Item : String;
         Drop : Truncation := Error)
         return Bounded_String
         renames Functions.Overwrite;

      procedure Overwrite (
         Source : in out Bounded_String;
         Position : Positive;
         New_Item : String;
         Drop : Truncation := Error)
         renames Functions.Overwrite;

      function Delete (
         Source : Bounded_String;
         From : Positive;
         Through : Natural)
         return Bounded_String
         renames Functions.Delete;

      procedure Delete (
         Source : in out Bounded_String;
         From : Positive;
         Through : Natural)
         renames Functions.Delete;

      --  String selector subprograms

      function Trim (
         Source : Bounded_String;
         Side : Trim_End;
         Left : Character := Space;
         Right : Character := Space)
         return Bounded_String
         renames Functions.Trim;
      procedure Trim (
         Source : in out Bounded_String;
         Side : Trim_End;
         Left : Character := Space;
         Right : Character := Space)
         renames Functions.Trim;

      function Trim (
         Source : Bounded_String;
         Left : Strings.Maps.Character_Set;
         Right : Strings.Maps.Character_Set)
         return Bounded_String
         renames Maps.Trim;

      procedure Trim (
         Source : in out Bounded_String;
         Left : Strings.Maps.Character_Set;
         Right : Strings.Maps.Character_Set)
         renames Maps.Trim;

      function Head (
         Source : Bounded_String;
         Count : Natural;
         Pad : Character := Space;
         Drop : Truncation := Error)
         return Bounded_String
         renames Functions.Head;

      procedure Head (
         Source : in out Bounded_String;
         Count : Natural;
         Pad : Character := Space;
         Drop : Truncation := Error)
         renames Functions.Head;

      function Tail (
         Source : Bounded_String;
         Count : Natural;
         Pad : Character := Space;
         Drop : Truncation := Error)
         return Bounded_String
         renames Functions.Tail;

      procedure Tail (
         Source : in out Bounded_String;
         Count : Natural;
         Pad : Character := Space;
         Drop : Truncation := Error)
         renames Functions.Tail;

      --  String constructor subprograms

      function "*" (Left : Natural; Right : Character)
         return Bounded_String
         renames Instance."*";

      function "*" (Left : Natural; Right : String)
         return Bounded_String
         renames Instance."*";

      function "*" (Left : Natural; Right : Bounded_String)
         return Bounded_String
         renames Instance."*";

      function Replicate (
         Count : Natural;
         Item : Character;
         Drop : Truncation := Error)
         return Bounded_String
         renames Instance.Replicate;

      function Replicate (
         Count : Natural;
         Item : String;
         Drop : Truncation := Error)
         return Bounded_String
         renames Instance.Replicate;

      function Replicate (
         Count : Natural;
         Item : Bounded_String;
         Drop : Truncation := Error)
         return Bounded_String
         renames Instance.Replicate;

   end Generic_Bounded_Length;

end Ada.Strings.Bounded;
