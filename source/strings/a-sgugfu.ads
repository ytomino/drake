pragma License (Unrestricted);
--  generalized unit of a part of Ada.Strings.Unbounded
with Ada.Strings.Generic_Functions;
generic
   with package Fixed_Functions is
      new Strings.Generic_Functions (
         Character_Type => Character_Type,
         String_Type => String_Type,
         Space => <>);
package Ada.Strings.Generic_Unbounded.Generic_Functions is
   pragma Preelaborate;

   --  Search subprograms

   function Index (
      Source : Unbounded_String;
      Pattern : String_Type;
      From : Positive;
      Going : Direction := Forward)
      return Natural;

   function Index (
      Source : Unbounded_String;
      Pattern : String_Type;
      Going : Direction := Forward)
      return Natural;

   function Index_Non_Blank (
      Source : Unbounded_String;
      From : Positive;
      Going : Direction := Forward)
      return Natural;

   function Index_Non_Blank (
      Source : Unbounded_String;
      Going : Direction := Forward)
      return Natural;

   function Count (
      Source : Unbounded_String;
      Pattern : String_Type)
      return Natural;

   --  String transformation subprograms

   function Replace_Slice (
      Source : Unbounded_String;
      Low : Positive;
      High : Natural;
      By : String_Type)
      return Unbounded_String;

   procedure Replace_Slice (
      Source : in out Unbounded_String;
      Low : Positive;
      High : Natural;
      By : String_Type);

   function Insert (
      Source : Unbounded_String;
      Before : Positive;
      New_Item : String_Type)
      return Unbounded_String;

   procedure Insert (
      Source : in out Unbounded_String;
      Before : Positive;
      New_Item : String_Type);

   function Overwrite (
      Source : Unbounded_String;
      Position : Positive;
      New_Item : String_Type)
      return Unbounded_String;

   procedure Overwrite (
      Source : in out Unbounded_String;
      Position : Positive;
      New_Item : String_Type);

   function Delete (
      Source : Unbounded_String;
      From : Positive;
      Through : Natural)
      return Unbounded_String;

   procedure Delete (
      Source : in out Unbounded_String;
      From : Positive;
      Through : Natural);

   --  String selector subprograms

   function Trim (
      Source : Unbounded_String;
      Side : Trim_End;
      Blank : Character_Type := Fixed_Functions.Space) -- additional
      return Unbounded_String;

   procedure Trim (
      Source : in out Unbounded_String;
      Side : Trim_End;
      Blank : Character_Type := Fixed_Functions.Space); -- additional

   function Head (
      Source : Unbounded_String;
      Count : Natural;
      Pad : Character_Type := Fixed_Functions.Space)
      return Unbounded_String;

   procedure Head (
      Source : in out Unbounded_String;
      Count : Natural;
      Pad : Character_Type := Fixed_Functions.Space);

   function Tail (
      Source : Unbounded_String;
      Count : Natural;
      Pad : Character_Type := Fixed_Functions.Space)
      return Unbounded_String;

   procedure Tail (
      Source : in out Unbounded_String;
      Count : Natural;
      Pad : Character_Type := Fixed_Functions.Space);

   --  String constructor functions

   function "*" (Left : Natural; Right : Character_Type)
      return Unbounded_String;

   function "*" (Left : Natural; Right : String_Type)
      return Unbounded_String;

   function "*" (Left : Natural; Right : Unbounded_String)
      return Unbounded_String;

   generic
      with package Fixed_Maps is new Fixed_Functions.Generic_Maps (<>);
   package Generic_Maps is

      --  Search subprograms

      function Index (
         Source : Unbounded_String;
         Pattern : String_Type;
         From : Positive;
         Going : Direction := Forward;
         Mapping : Fixed_Maps.Character_Mapping)
         return Natural;

      function Index (
         Source : Unbounded_String;
         Pattern : String_Type;
         Going : Direction := Forward;
         Mapping : Fixed_Maps.Character_Mapping)
         return Natural;

      function Index (
         Source : Unbounded_String;
         Pattern : String_Type;
         From : Positive;
         Going : Direction := Forward;
         Mapping : not null access function (From : Wide_Wide_Character)
            return Wide_Wide_Character)
         return Natural;

      function Index (
         Source : Unbounded_String;
         Pattern : String_Type;
         Going : Direction := Forward;
         Mapping : not null access function (From : Wide_Wide_Character)
            return Wide_Wide_Character)
         return Natural;

      function Index_Element (
         Source : Unbounded_String;
         Pattern : String_Type;
         From : Positive;
         Going : Direction := Forward;
         Mapping : not null access function (From : Character_Type)
            return Character_Type)
         return Natural;

      function Index_Element (
         Source : Unbounded_String;
         Pattern : String_Type;
         Going : Direction := Forward;
         Mapping : not null access function (From : Character_Type)
            return Character_Type)
         return Natural;

      function Index (
         Source : Unbounded_String;
         Set : Fixed_Maps.Character_Set;
         From : Positive;
         Test : Membership := Inside;
         Going : Direction := Forward)
         return Natural;

      function Index (
         Source : Unbounded_String;
         Set : Fixed_Maps.Character_Set;
         Test : Membership := Inside;
         Going : Direction := Forward)
         return Natural;

      function Count (
         Source : Unbounded_String;
         Pattern : String_Type;
         Mapping : Fixed_Maps.Character_Mapping)
         return Natural;

      function Count (
         Source : Unbounded_String;
         Pattern : String_Type;
         Mapping : not null access function (From : Wide_Wide_Character)
            return Wide_Wide_Character)
         return Natural;

      function Count_Element (
         Source : Unbounded_String;
         Pattern : String_Type;
         Mapping : not null access function (From : Character_Type)
            return Character_Type)
         return Natural;

      function Count (
         Source : Unbounded_String;
         Set : Fixed_Maps.Character_Set)
         return Natural;

      procedure Find_Token (
         Source : Unbounded_String;
         Set : Fixed_Maps.Character_Set;
         From : Positive;
         Test : Membership;
         First : out Positive;
         Last : out Natural);

      procedure Find_Token (
         Source : Unbounded_String;
         Set : Fixed_Maps.Character_Set;
         Test : Membership;
         First : out Positive;
         Last : out Natural);

      --  String translation subprograms

      function Translate (
         Source : Unbounded_String;
         Mapping : Fixed_Maps.Character_Mapping)
         return Unbounded_String;

      procedure Translate (
         Source : in out Unbounded_String;
         Mapping : Fixed_Maps.Character_Mapping);

      function Translate (
         Source : Unbounded_String;
         Mapping : not null access function (From : Wide_Wide_Character)
            return Wide_Wide_Character)
         return Unbounded_String;

      procedure Translate (
         Source : in out Unbounded_String;
         Mapping : not null access function (From : Wide_Wide_Character)
            return Wide_Wide_Character);

      function Translate_Element (
         Source : Unbounded_String;
         Mapping : not null access function (From : Character_Type)
            return Character_Type)
         return Unbounded_String;

      procedure Translate_Element (
         Source : in out Unbounded_String;
         Mapping : not null access function (From : Character_Type)
            return Character_Type);

      --  String selector subprograms

      function Trim (
         Source : Unbounded_String;
         Left : Fixed_Maps.Character_Set;
         Right : Fixed_Maps.Character_Set)
         return Unbounded_String;

      procedure Trim (
         Source : in out Unbounded_String;
         Left : Fixed_Maps.Character_Set;
         Right : Fixed_Maps.Character_Set);

   end Generic_Maps;

end Ada.Strings.Generic_Unbounded.Generic_Functions;
