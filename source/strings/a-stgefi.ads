pragma License (Unrestricted);
--  generalized unit of Ada.Strings.Fixed
generic
   type Character_Type is (<>);
   type String_Type is array (Positive range <>) of Character_Type;
   Space : Character_Type;
package Ada.Strings.Generic_Fixed is
   pragma Preelaborate;

   --  "Copy" procedure for strings of possibly different lengths

   procedure Move (
      Source : String_Type;
      Target : out String_Type;
      Drop : Truncation := Error;
      Justify : Alignment := Left;
      Pad : Character_Type := Space);

   --  Search subprograms

   --  extended, character searching
   function Index (
      Source : String_Type;
      Pattern : Character_Type;
      From : Positive;
      Going : Direction := Forward)
      return Natural;
   pragma Inline (Index);
   function Index (
      Source : String_Type;
      Pattern : Character_Type;
      Going : Direction := Forward)
      return Natural;
   pragma Inline (Index);
   function Index_Forward (Source : String_Type; Pattern : Character_Type)
      return Natural;
   function Index_Backward (Source : String_Type; Pattern : Character_Type)
      return Natural;

   function Index (
      Source : String_Type;
      Pattern : String_Type;
      From : Positive;
      Going : Direction := Forward)
      return Natural;
   pragma Inline (Index);

   function Index (
      Source : String_Type;
      Pattern : String_Type;
      Going : Direction := Forward)
      return Natural;
   pragma Inline (Index);

   --  extended, forward or backward only version
   function Index_Forward (Source : String_Type; Pattern : String_Type)
      return Natural;
   function Index_Backward (Source : String_Type; Pattern : String_Type)
      return Natural;

   function Index_Non_Blank (
      Source : String_Type;
      From : Positive;
      Going : Direction := Forward)
      return Natural;
   pragma Inline (Index_Non_Blank);

   function Index_Non_Blank (
      Source : String_Type;
      Going : Direction := Forward)
      return Natural;
   pragma Inline (Index_Non_Blank);

   --  extended, forward or backward only version
   function Index_Non_Blank_Forward (
      Source : String_Type;
      Blank : Character_Type := Space)
      return Natural;
   function Index_Non_Blank_Backward (
      Source : String_Type;
      Blank : Character_Type := Space)
      return Natural;

   function Count (
      Source : String_Type;
      Pattern : String_Type)
      return Natural;

   --  String transformation subprograms

   function Replace_Slice (
      Source : String_Type;
      Low : Positive;
      High : Natural;
      By : String_Type)
      return String_Type;

   procedure Replace_Slice (
      Source : in out String_Type;
      Low : Positive;
      High : Natural;
      By : String_Type;
      Drop : Truncation := Error;
      Justify : Alignment := Left;
      Pad : Character_Type := Space);

   function Insert (
      Source : String_Type;
      Before : Positive;
      New_Item : String_Type)
      return String_Type;

   procedure Insert (
      Source : in out String_Type;
      Before : Positive;
      New_Item : String_Type;
      Drop : Truncation := Error);

   function Overwrite (
      Source : String_Type;
      Position : Positive;
      New_Item : String_Type)
      return String_Type;

   procedure Overwrite (
      Source : in out String_Type;
      Position : Positive;
      New_Item : String_Type;
      Drop : Truncation := Right);

   function Delete (
      Source : String_Type;
      From : Positive;
      Through : Natural)
      return String_Type;

   procedure Delete (
      Source : in out String_Type;
      From : Positive;
      Through : Natural;
      Justify : Alignment := Left;
      Pad : Character_Type := Space);

   --  extended, for Bounede_String and Unbounede_String
   procedure Delete (
      Source : in out String_Type;
      Last : in out Natural;
      From : Positive;
      Through : Natural);

   --  String selector subprograms

   function Trim (
      Source : String_Type;
      Side : Trim_End;
      Blank : Character_Type := Space) -- additional
      return String_Type;

   procedure Trim (
      Source : in out String_Type;
      Side : Trim_End;
      Justify : Alignment := Left;
      Pad : Character_Type := Space);

   --  extended, explicit blank (default parameter is wrong for CXA4025)
   procedure Trim (
      Source : in out String_Type;
      Side : Trim_End;
      Blank : Character_Type;
      Justify : Alignment := Left;
      Pad : Character_Type := Space);

   --  extended, no copying
   procedure Trim (
      Source : String_Type;
      Side : Trim_End;
      Blank : Character_Type := Space;
      First : out Positive;
      Last : out Natural);

   function Head (
      Source : String_Type;
      Count : Natural;
      Pad : Character_Type := Space)
      return String_Type;

   procedure Head (
      Source : in out String_Type;
      Count : Natural;
      Justify : Alignment := Left;
      Pad : Character_Type := Space);

   function Tail (
      Source : String_Type;
      Count : Natural;
      Pad : Character_Type := Space)
      return String_Type;

   procedure Tail (
      Source : in out String_Type;
      Count : Natural;
      Justify : Alignment := Left;
      Pad : Character_Type := Space);

   --  String constructor functions

   function "*" (Left : Natural; Right : Character_Type)
      return String_Type;

   function "*" (Left : Natural; Right : String_Type)
      return String_Type;

   generic
      Expanding : Natural;
      with procedure Put (
         Value : Wide_Wide_Character;
         Item : out String_Type;
         Last : out Natural);
      with procedure Get (
         Item : String_Type;
         Last : out Natural;
         Value : out Wide_Wide_Character;
         Is_Illegal_Sequence : out Boolean);
      with procedure Get_Reverse (
         Item : String_Type;
         First : out Positive;
         Value : out Wide_Wide_Character;
         Is_Illegal_Sequence : out Boolean);
      type Character_Set is private;
      with function Is_In (
         Element : Wide_Wide_Character;
         Set : Character_Set)
         return Boolean;
      type Character_Mapping is private;
      with function Value (
         Map : Character_Mapping;
         Element : Wide_Wide_Character)
         return Wide_Wide_Character;
   package Generic_Maps is

      --  Search subprograms

      function Index (
         Source : String_Type;
         Pattern : String_Type;
         From : Positive;
         Going : Direction := Forward;
         Mapping : Character_Mapping)
         return Natural;
      pragma Inline (Index);

      function Index (
         Source : String_Type;
         Pattern : String_Type;
         Going : Direction := Forward;
         Mapping : Character_Mapping)
         return Natural;
      pragma Inline (Index);

      --  extended, forward or backward only version
      function Index_Forward (
         Source : String_Type;
         Pattern : String_Type;
         Mapping : Character_Mapping)
         return Natural;
      function Index_Backward (
         Source : String_Type;
         Pattern : String_Type;
         Mapping : Character_Mapping)
         return Natural;

      function Index (
         Source : String_Type;
         Pattern : String_Type;
         From : Positive;
         Going : Direction := Forward;
         Mapping : not null access function (From : Wide_Wide_Character)
            return Wide_Wide_Character)
         return Natural;
      pragma Inline (Index);

      function Index (
         Source : String_Type;
         Pattern : String_Type;
         Going : Direction := Forward;
         Mapping : not null access function (From : Wide_Wide_Character)
            return Wide_Wide_Character)
         return Natural;
      pragma Inline (Index);

      --  extended, forward or backward only version
      function Index_Forward (
         Source : String_Type;
         Pattern : String_Type;
         Mapping : not null access function (From : Wide_Wide_Character)
            return Wide_Wide_Character)
         return Natural;
      function Index_Backward (
         Source : String_Type;
         Pattern : String_Type;
         Mapping : not null access function (From : Wide_Wide_Character)
            return Wide_Wide_Character)
         return Natural;

      function Index_Per_Element (
         Source : String_Type;
         Pattern : String_Type;
         From : Positive;
         Going : Direction := Forward;
         Mapping : not null access function (From : Character_Type)
            return Character_Type)
         return Natural;
      pragma Inline (Index_Per_Element);

      function Index_Per_Element (
         Source : String_Type;
         Pattern : String_Type;
         Going : Direction := Forward;
         Mapping : not null access function (From : Character_Type)
            return Character_Type)
         return Natural;
      pragma Inline (Index_Per_Element);

      --  extended, forward or backward only version
      function Index_Per_Element_Forward (
         Source : String_Type;
         Pattern : String_Type;
         Mapping : not null access function (From : Character_Type)
            return Character_Type)
         return Natural;
      function Index_Per_Element_Backward (
         Source : String_Type;
         Pattern : String_Type;
         Mapping : not null access function (From : Character_Type)
            return Character_Type)
         return Natural;

      function Index (
         Source : String_Type;
         Set : Character_Set;
         From : Positive;
         Test : Membership := Inside;
         Going : Direction := Forward)
         return Natural;
      pragma Inline (Index);

      function Index (
         Source : String_Type;
         Set : Character_Set;
         Test : Membership := Inside;
         Going : Direction := Forward)
         return Natural;
      pragma Inline (Index);

      --  extended, forward or backward only version
      function Index_Forward (
         Source : String_Type;
         Set : Character_Set;
         Test : Membership := Inside)
         return Natural;
      function Index_Backward (
         Source : String_Type;
         Set : Character_Set;
         Test : Membership := Inside)
         return Natural;

      function Count (
         Source : String_Type;
         Pattern : String_Type;
         Mapping : Character_Mapping)
         return Natural;

      function Count (
         Source : String_Type;
         Pattern : String_Type;
         Mapping : not null access function (From : Wide_Wide_Character)
            return Wide_Wide_Character)
         return Natural;

      function Count_Per_Element (
         Source : String_Type;
         Pattern : String_Type;
         Mapping : not null access function (From : Character_Type)
            return Character_Type)
         return Natural;

      function Count (
         Source : String_Type;
         Set : Character_Set)
         return Natural;

      procedure Find_Token (
         Source : String_Type;
         Set : Character_Set;
         From : Positive;
         Test : Membership;
         First : out Positive;
         Last : out Natural);

      procedure Find_Token (
         Source : String_Type;
         Set : Character_Set;
         Test : Membership;
         First : out Positive;
         Last : out Natural);

      --  extended
      function Find_Token_Last (
         Source : String_Type;
         Set : Character_Set;
         Test : Membership)
         return Natural;
      function Find_Token_First (
         Source : String_Type;
         Set : Character_Set;
         Test : Membership)
         return Positive;

      --  String translation subprograms

      function Translate (
         Source : String_Type;
         Mapping : Character_Mapping)
         return String_Type;

      procedure Translate (
         Source : in out String_Type;
         Mapping : Character_Mapping;
         Drop : Truncation := Error; -- additional
         Justify : Alignment := Left; -- additional
         Pad : Character_Type := Space); -- additional

      function Translate (
         Source : String_Type;
         Mapping : not null access function (From : Wide_Wide_Character)
            return Wide_Wide_Character)
         return String_Type;

      procedure Translate (
         Source : in out String_Type;
         Mapping : not null access function (From : Wide_Wide_Character)
            return Wide_Wide_Character;
         Drop : Truncation := Error; -- additional
         Justify : Alignment := Left; -- additional
         Pad : Character_Type := Space); -- additional

      function Translate_Per_Element (
         Source : String_Type;
         Mapping : not null access function (From : Character_Type)
            return Character_Type)
         return String_Type;

      procedure Translate_Per_Element (
         Source : in out String_Type;
         Mapping : not null access function (From : Character_Type)
            return Character_Type);

      --  String selector subprograms

      function Trim (
         Source : String_Type;
         Left : Character_Set;
         Right : Character_Set)
         return String_Type;

      procedure Trim (
         Source : in out String_Type;
         Left : Character_Set;
         Right : Character_Set;
         Justify : Alignment := Strings.Left;
         Pad : Character_Type := Space);

      --  extended, no copying
      procedure Trim (
         Source : String_Type;
         Left : Character_Set;
         Right : Character_Set;
         First : out Positive;
         Last : out Natural);

   end Generic_Maps;

end Ada.Strings.Generic_Fixed;
