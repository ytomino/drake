pragma License (Unrestricted);
--  generic implementation of Ada.Strings.Bounded
generic
   type Character_Type is (<>);
   type String_Type is array (Positive range <>) of Character_Type;
package Ada.Strings.Generic_Bounded is
   pragma Preelaborate;

   --  extended
   type Bounded_String (Max : Positive) is record
      Length : Natural := 0;
      Data : String_Type (1 .. Max);
   end record;

   generic
      Max : Positive;    -- Maximum length of a Bounded_String
   package Generic_Bounded_Length is

      Max_Length : constant Positive := Max;

--    type Bounded_String is private;
      type Bounded_String is
         new Generic_Bounded.Bounded_String (Max); --  extended

--    Null_Bounded_String : constant Bounded_String;
      function Null_Bounded_String return Bounded_String;
      pragma Inline (Null_Bounded_String);

      subtype Length_Range is Natural range 0 .. Max_Length;

      function Length (Source : Bounded_String) return Length_Range;
      pragma Inline (Length);

      --  Conversion, Concatenation, and Selection functions

--    function To_Bounded_String (Source : String; Drop : Truncation := Error)
--       return Bounded_String;

--    function To_String (Source : Bounded_String) return String;

--    procedure Set_Bounded_String (
--       Target : out Bounded_String;
--        Source : String;
--        Drop : Truncation := Error);

--    function Append (
--       Left, Right : Bounded_String;
--       Drop : Truncation := Error)
--       return Bounded_String;

--    function Append (
--       Left : Bounded_String;
--       Right : String;
--       Drop : Truncation := Error)
--       return Bounded_String;

--    function Append (
--       Left : String;
--       Right : Bounded_String;
--       Drop : Truncation := Error)
--       return Bounded_String;

--    function Append (
--       Left : Bounded_String;
--       Right : Character;
--       Drop : Truncation := Error)
--       return Bounded_String;

--    function Append (
--       Left : Character;
--       Right : Bounded_String;
--       Drop : Truncation := Error)
--       return Bounded_String;

--    procedure Append (
--       Source : in out Bounded_String;
--       New_Item : Bounded_String;
--       Drop : Truncation := Error);

--    procedure Append (
--       Source : in out Bounded_String;
--       New_Item : String;
--       Drop : Truncation := Error);

--    procedure Append (
--       Source : in out Bounded_String;
--       New_Item : Character;
--       Drop : Truncation := Error);

--    function "&" (Left, Right : Bounded_String)
--       return Bounded_String;

--    function "&" (Left : Bounded_String; Right : String)
--       return Bounded_String;

--    function "&" (Left : String; Right : Bounded_String)
--       return Bounded_String;

--    function "&" (Left : Bounded_String; Right : Character)
--       return Bounded_String;

--    function "&" (Left : Character; Right : Bounded_String)
--       return Bounded_String;

--    function Element (
--       Source : Bounded_String;
--       Index : Positive)
--       return Character;

--    procedure Replace_Element (
--       Source : in out Bounded_String;
--       Index : Positive;
--       By : Character);

--    function Slice (
--       Source : Bounded_String;
--       Low : Positive;
--       High : Natural)
--       return String;

--    function Bounded_Slice (
--       Source : Bounded_String;
--       Low : Positive;
--       High : Natural)
--       return Bounded_String;

--    procedure Bounded_Slice (
--       Source : Bounded_String;
--       Target : out Bounded_String;
--       Low : Positive;
--       High : Natural);

--    function "=" (Left, Right : Bounded_String) return Boolean;
--    function "=" (Left : Bounded_String; Right : String)
--      return Boolean;

--    function "=" (Left : String; Right : Bounded_String)
--      return Boolean;

--    function "<" (Left, Right : Bounded_String) return Boolean;

--    function "<" (Left : Bounded_String; Right : String)
--      return Boolean;

--    function "<" (Left : String; Right : Bounded_String)
--      return Boolean;

--    function "<=" (Left, Right : Bounded_String) return Boolean;

--    function "<=" (Left : Bounded_String; Right : String)
--      return Boolean;

--    function "<=" (Left : String; Right : Bounded_String)
--      return Boolean;

--    function ">" (Left, Right : Bounded_String) return Boolean;

--    function ">" (Left : Bounded_String; Right : String)
--      return Boolean;

--    function ">" (Left : String; Right : Bounded_String)
--      return Boolean;

--    function ">=" (Left, Right : Bounded_String) return Boolean;

--    function ">=" (Left : Bounded_String; Right : String)
--      return Boolean;

--    function ">=" (Left : String; Right : Bounded_String)
--      return Boolean;

      --  Search subprograms

--    function Index (
--       Source : Bounded_String;
--       Pattern : String;
--       From : Positive;
--       Going : Direction := Forward;
--       Mapping : Maps.Character_Mapping := Maps.Identity)
--       return Natural;

--    function Index (
--       Source : Bounded_String;
--       Pattern : String;
--       From : Positive;
--       Going : Direction := Forward;
--       Mapping : Maps.Character_Mapping_Function)
--       return Natural;

--    function Index (
--       Source : Bounded_String;
--       Pattern : String;
--       Going : Direction := Forward;
--       Mapping : Maps.Character_Mapping := Maps.Identity)
--       return Natural;

--    function Index (
--       Source : Bounded_String;
--       Pattern : String;
--       Going : Direction := Forward;
--       Mapping : Maps.Character_Mapping_Function)
--       return Natural;

--    function Index (
--       Source : Bounded_String;
--       Set : Maps.Character_Set;
--       From : Positive;
--       Test : Membership := Inside;
--       Going : Direction := Forward)
--       return Natural;

--    function Index (
--       Source : Bounded_String;
--       Set : Maps.Character_Set;
--       Test : Membership := Inside;
--       Going : Direction := Forward)
--       return Natural;

--    function Index_Non_Blank (
--       Source : Bounded_String;
--       From : Positive;
--       Going : Direction := Forward)
--       return Natural;

--    function Index_Non_Blank (
--       Source : Bounded_String;
--       Going : Direction := Forward)
--       return Natural;

--    function Count (
--       Source : Bounded_String;
--       Pattern : String;
--       Mapping : Maps.Character_Mapping := Maps.Identity)
--       return Natural;

--    function Count (
--       Source : Bounded_String;
--       Pattern : String;
--       Mapping : Maps.Character_Mapping_Function)
--       return Natural;

--    function Count (Source : Bounded_String; Set : Maps.Character_Set)
--       return Natural;

--    procedure Find_Token (
--       Source : Bounded_String;
--       Set : Maps.Character_Set;
--       From : Positive;
--       Test : Membership;
--       First : out Positive;
--       Last : out Natural);

--    procedure Find_Token (
--       Source : Bounded_String;
--       Set : Maps.Character_Set;
--       Test : Membership;
--       First : out Positive;
--       Last : out Natural);

      --  String translation subprograms

--    function Translate (
--       Source : Bounded_String;
--       Mapping : Maps.Character_Mapping)
--       return Bounded_String;

--    procedure Translate (
--       Source : in out Bounded_String;
--       Mapping : Maps.Character_Mapping);

--    function Translate (
--       Source : Bounded_String;
--       Mapping : Maps.Character_Mapping_Function)
--       return Bounded_String;

--    procedure Translate (
--       Source : in out Bounded_String;
--       Mapping : Maps.Character_Mapping_Function);

      --  String transformation subprograms

--    function Replace_Slice (
--       Source : Bounded_String;
--       Low : Positive;
--       High : Natural;
--       By : String;
--       Drop : Truncation := Error)
--       return Bounded_String;

--    procedure Replace_Slice (
--       Source : in out Bounded_String;
--       Low : Positive;
--       High : Natural;
--       By : String;
--       Drop : Truncation := Error);

--    function Insert (
--       Source : Bounded_String;
--       Before : Positive;
--       New_Item : String;
--       Drop : Truncation := Error)
--       return Bounded_String;

--    procedure Insert (
--       Source : in out Bounded_String;
--       Before : Positive;
--       New_Item : String;
--       Drop : Truncation := Error);

--    function Overwrite (
--       Source : Bounded_String;
--       Position : Positive;
--       New_Item : String;
--       Drop : Truncation := Error)
--       return Bounded_String;

--    procedure Overwrite (
--       Source : in out Bounded_String;
--       Position : Positive;
--       New_Item : String;
--       Drop : Truncation := Error);

--    function Delete (
--       Source : Bounded_String;
--       From : Positive;
--       Through : Natural)
--       return Bounded_String;

--    procedure Delete (
--       Source : in out Bounded_String;
--       From : Positive;
--       Through : Natural);

      --  String selector subprograms

--    function Trim (
--       Source : Bounded_String;
--       Side : Trim_End)
--       return Bounded_String;
--    procedure Trim (
--       Source : in out Bounded_String;
--       Side : Trim_End);

--    function Trim (
--       Source : Bounded_String;
--       Left : Maps.Character_Set;
--       Right : Maps.Character_Set)
--       return Bounded_String;

--    procedure Trim (
--       Source : in out Bounded_String;
--       Left : Maps.Character_Set;
--       Right : Maps.Character_Set);

--    function Head (
--       Source : Bounded_String;
--       Count : Natural;
--       Pad : Character := Space;
--       Drop : Truncation := Error)
--       return Bounded_String;

--    procedure Head (
--       Source : in out Bounded_String;
--       Count : Natural;
--       Pad : Character := Space;
--       Drop : Truncation := Error);

--    function Tail (
--       Source : Bounded_String;
--       Count : Natural;
--       Pad : Character := Space;
--       Drop : Truncation := Error)
--       return Bounded_String;

--    procedure Tail (
--       Source : in out Bounded_String;
--       Count : Natural;
--       Pad : Character := Space;
--       Drop : Truncation := Error);

      --  String constructor subprograms

--    function "*" (Left : Natural; Right : Character)
--       return Bounded_String;

--    function "*" (Left : Natural; Right : String)
--       return Bounded_String;

--    function "*" (Left : Natural; Right : Bounded_String)
--       return Bounded_String;

--    function Replicate (
--       Count : Natural;
--       Item : Character;
--       Drop : Truncation := Error)
--       return Bounded_String;

--    function Replicate (
--       Count : Natural;
--       Item : String;
--       Drop : Truncation := Error)
--       return Bounded_String;

--    function Replicate (
--       Count : Natural;
--       Item : Bounded_String;
--       Drop : Truncation := Error)
--       return Bounded_String;

   end Generic_Bounded_Length;

end Ada.Strings.Generic_Bounded;
