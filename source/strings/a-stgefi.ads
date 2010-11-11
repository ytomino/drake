pragma License (Unrestricted);
--  generic implementation of Ada.Strings.Fixed
generic
   type Character_Type is (<>);
   type String_Type is array (Positive range <>) of Character_Type;
   Space : Character_Type;
package Ada.Strings.Generic_Fixed is
   pragma Preelaborate;

   --  "Copy" procedure for strings of possibly different lengths

--  procedure Move (
--    Source : String_Type;
--    Target : out String_Type;
--    Drop : Truncation := Error;
--    Justify : Alignment  := Left;
--    Pad : Character_Type  := Space);

   --  Search subprograms

--  function Index (
--    Source : String_Type;
--    Pattern : String_Type;
--    From : Positive;
--    Going : Direction := Forward;
--    Mapping : Maps.Character_Mapping := Maps.Identity)
--    return Natural;

--  function Index (
--    Source : String_Type;
--    Pattern : String_Type;
--    From : Positive;
--    Going : Direction := Forward;
--    Mapping : Maps.Character_Mapping_Function)
--    return Natural;

--  function Index (
--    Source : String_Type;
--    Pattern : String_Type;
--    Going : Direction := Forward;
--    Mapping : Maps.Character_Mapping := Maps.Identity)
--    return Natural;

--  function Index (
--    Source : String_Type;
--    Pattern : String_Type;
--    Going : Direction := Forward;
--    Mapping : Maps.Character_Mapping_Function)
--    return Natural;

--  function Index (
--    Source : String_Type;
--    Set : Maps.Character_Set;
--    From : Positive;
--    Test : Membership := Inside;
--    Going : Direction := Forward)
--    return Natural;

--  function Index (
--    Source : String_Type;
--    Set : Maps.Character_Set;
--    Test : Membership := Inside;
--    Going : Direction  := Forward)
--    return Natural;

   --  extended, simple version (forward only)
   function Index (
      Source : String_Type;
      Pattern : String_Type)
      return Natural;
   function Index (
      Source : String_Type;
      Pattern : String_Type;
      From : Positive)
      return Natural; --  forward only
   --  extended, simple version (backward only)
   function Index (
      Source : String_Type;
      Pattern : String_Type;
      From : Positive;
      Going : Backward_Direction)
      return Natural;

   --  extended, character searching (forward only)
   function Index (
      Source : String_Type;
      Pattern : Character_Type;
      From : Positive)
      return Natural;
   --  extended, character searching (backward only)
   function Index (
      Source : String_Type;
      Pattern : Character_Type;
      From : Positive;
      Going : Backward_Direction)
      return Natural;

--  function Index_Non_Blank (
--    Source : String_Type;
--    From : Positive;
--    Going : Direction := Forward)
--    return Natural;

--  function Index_Non_Blank (
--    Source : String_Type;
--    Going : Direction := Forward)
--    return Natural;

--  function Count (
--    Source : String_Type;
--    Pattern : String_Type;
--    Mapping : Maps.Character_Mapping := Maps.Identity)
--    return Natural;

--  function Count (
--    Source : String_Type;
--    Pattern : String_Type;
--    Mapping : Maps.Character_Mapping_Function)
--    return Natural;

--  function Count (
--    Source : String_Type;
--    Set : Maps.Character_Set)
--    return Natural;

--  procedure Find_Token (
--    Source : String_Type;
--    Set : Maps.Character_Set;
--    From : Positive;
--    Test : Membership;
--    First : out Positive;
--    Last : out Natural);

--  procedure Find_Token (
--    Source : String_Type;
--    Set : Maps.Character_Set;
--    Test : Membership;
--    First : out Positive;
--    Last : out Natural);

   --  String_Type translation subprograms

--  function Translate (
--    Source : String_Type;
--    Mapping : Maps.Character_Mapping)
--    return String_Type;

--  procedure Translate (
--    Source : in out String_Type;
--    Mapping : Maps.Character_Mapping);

--  function Translate (
--    Source : String_Type;
--    Mapping : Maps.Character_Mapping_Function)
--    return String_Type;

--  procedure Translate (
--    Source : in out String_Type;
--    Mapping : Maps.Character_Mapping_Function);

   --  String_Type transformation subprograms

--  function Replace_Slice (
--    Source : String_Type;
--    Low : Positive;
--    High : Natural;
--    By : String_Type)
--    return String_Type;

--  procedure Replace_Slice (
--    Source : in out String_Type;
--    Low : Positive;
--    High : Natural;
--    By : String_Type;
--    Drop : Truncation := Error;
--    Justify : Alignment  := Left;
--    Pad : Character_Type  := Space);

--  function Insert (
--    Source : String_Type;
--    Before : Positive;
--    New_Item : String_Type)
--    return String_Type;

--  procedure Insert (
--    Source : in out String_Type;
--    Before : Positive;
--    New_Item : String_Type;
--    Drop : Truncation := Error);

--  function Overwrite (
--    Source : String_Type;
--    Position : Positive;
--    New_Item : String_Type)
--    return String_Type;

--  procedure Overwrite (
--    Source : in out String_Type;
--    Position : Positive;
--    New_Item : String_Type;
--    Drop : Truncation := Right);

--  function Delete (
--    Source : String_Type;
--    From : Positive;
--    Through : Natural)
--    return String_Type;

--  procedure Delete (
--    Source : in out String_Type;
--    From : Positive;
--    Through : Natural;
--    Justify : Alignment := Left;
--    Pad : Character_Type := Space);

   --  String_Type selector subprograms
   function Trim (Source : String_Type; Side : Trim_End) return String_Type;

--  procedure Trim (
--    Source : in out String_Type;
--    Side : Trim_End;
--    Justify : Alignment := Left;
--    Pad : Character_Type := Space);

--  function Trim (
--    Source : String_Type;
--    Left : Maps.Character_Set;
--    Right : Maps.Character_Set)
--    return String_Type;

--  procedure Trim (
--    Source : in out String_Type;
--    Left : Maps.Character_Set;
--    Right : Maps.Character_Set;
--    Justify : Alignment := Strings.Left;
--    Pad : Character_Type := Space);

--  function Head (
--    Source : String_Type;
--    Count : Natural;
--    Pad : Character_Type := Space)
--    return String_Type;

--  procedure Head (
--    Source : in out String_Type;
--    Count : Natural;
--    Justify : Alignment := Left;
--    Pad : Character_Type := Space);

--  function Tail (
--    Source : String_Type;
--    Count : Natural;
--    Pad : Character_Type := Space)
--    return String_Type;

--  procedure Tail (
--    Source : in out String_Type;
--    Count : Natural;
--    Justify : Alignment := Left;
--    Pad : Character_Type := Space);

   --  String_Type constructor functions

   function "*" (Left : Natural; Right : Character_Type)
      return String_Type;

   function "*" (Left : Natural; Right : String_Type)
      return String_Type;

end Ada.Strings.Generic_Fixed;
