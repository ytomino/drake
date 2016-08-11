pragma License (Unrestricted);
--  generalized unit of a part of Ada.Strings.Bounded
with Ada.Strings.Generic_Functions;
generic
   with package Fixed_Functions is
      new Strings.Generic_Functions (
         Character_Type => Character_Type,
         String_Type => String_Type,
         Space => <>);
package Ada.Strings.Generic_Bounded.Generic_Functions is
   pragma Preelaborate;

   generic
      with package Bounded is
         new Generic_Bounded.Generic_Bounded_Length (<>);
   package Generic_Bounded_Length is

      --  Search subprograms

      function Index (
         Source : Bounded.Bounded_String;
         Pattern : String_Type;
         From : Positive;
         Going : Direction := Forward)
         return Natural;

      function Index (
         Source : Bounded.Bounded_String;
         Pattern : String_Type;
         Going : Direction := Forward)
         return Natural;

      function Index_Non_Blank (
         Source : Bounded.Bounded_String;
         From : Positive;
         Going : Direction := Forward)
         return Natural;

      function Index_Non_Blank (
         Source : Bounded.Bounded_String;
         Going : Direction := Forward)
         return Natural;

      function Count (Source : Bounded.Bounded_String; Pattern : String_Type)
         return Natural;

      --  String transformation subprograms

      function Replace_Slice (
         Source : Bounded.Bounded_String;
         Low : Positive;
         High : Natural;
         By : String_Type;
         Drop : Truncation := Error)
         return Bounded.Bounded_String;

      procedure Replace_Slice (
         Source : in out Bounded.Bounded_String;
         Low : Positive;
         High : Natural;
         By : String_Type;
         Drop : Truncation := Error);

      function Insert (
         Source : Bounded.Bounded_String;
         Before : Positive;
         New_Item : String_Type;
         Drop : Truncation := Error)
         return Bounded.Bounded_String;

      procedure Insert (
         Source : in out Bounded.Bounded_String;
         Before : Positive;
         New_Item : String_Type;
         Drop : Truncation := Error);

      function Overwrite (
         Source : Bounded.Bounded_String;
         Position : Positive;
         New_Item : String_Type;
         Drop : Truncation := Error)
         return Bounded.Bounded_String;

      procedure Overwrite (
         Source : in out Bounded.Bounded_String;
         Position : Positive;
         New_Item : String_Type;
         Drop : Truncation := Error);

      function Delete (
         Source : Bounded.Bounded_String;
         From : Positive;
         Through : Natural)
         return Bounded.Bounded_String;

      procedure Delete (
         Source : in out Bounded.Bounded_String;
         From : Positive;
         Through : Natural);

      --  String selector subprograms

      function Trim (
         Source : Bounded.Bounded_String;
         Side : Trim_End;
         Blank : Character_Type := Fixed_Functions.Space) -- additional
         return Bounded.Bounded_String;

      procedure Trim (
         Source : in out Bounded.Bounded_String;
         Side : Trim_End;
         Blank : Character_Type := Fixed_Functions.Space); -- additional

      function Head (
         Source : Bounded.Bounded_String;
         Count : Natural;
         Pad : Character_Type := Fixed_Functions.Space;
         Drop : Truncation := Error)
         return Bounded.Bounded_String;

      procedure Head (
         Source : in out Bounded.Bounded_String;
         Count : Natural;
         Pad : Character_Type := Fixed_Functions.Space;
         Drop : Truncation := Error);

      function Tail (
         Source : Bounded.Bounded_String;
         Count : Natural;
         Pad : Character_Type := Fixed_Functions.Space;
         Drop : Truncation := Error)
         return Bounded.Bounded_String;

      procedure Tail (
         Source : in out Bounded.Bounded_String;
         Count : Natural;
         Pad : Character_Type := Fixed_Functions.Space;
         Drop : Truncation := Error);

   end Generic_Bounded_Length;

   generic
      with package Fixed_Maps is new Fixed_Functions.Generic_Maps (<>);
   package Generic_Maps is

      generic
         with package Bounded is
            new Generic_Bounded.Generic_Bounded_Length (<>);
      package Generic_Bounded_Length is

         --  Search subprograms

         function Index (
            Source : Bounded.Bounded_String;
            Pattern : String_Type;
            From : Positive;
            Going : Direction := Forward;
            Mapping : Fixed_Maps.Character_Mapping)
            return Natural;

         function Index (
            Source : Bounded.Bounded_String;
            Pattern : String_Type;
            Going : Direction := Forward;
            Mapping : Fixed_Maps.Character_Mapping)
            return Natural;

         function Index (
            Source : Bounded.Bounded_String;
            Pattern : String_Type;
            From : Positive;
            Going : Direction := Forward;
            Mapping : not null access function (From : Wide_Wide_Character)
               return Wide_Wide_Character)
            return Natural;

         function Index (
            Source : Bounded.Bounded_String;
            Pattern : String_Type;
            Going : Direction := Forward;
            Mapping : not null access function (From : Wide_Wide_Character)
               return Wide_Wide_Character)
            return Natural;

         function Index_Element (
            Source : Bounded.Bounded_String;
            Pattern : String_Type;
            From : Positive;
            Going : Direction := Forward;
            Mapping : not null access function (From : Character_Type)
               return Character_Type)
            return Natural;

         function Index_Element (
            Source : Bounded.Bounded_String;
            Pattern : String_Type;
            Going : Direction := Forward;
            Mapping : not null access function (From : Character_Type)
               return Character_Type)
            return Natural;

         function Index (
            Source : Bounded.Bounded_String;
            Set : Fixed_Maps.Character_Set;
            From : Positive;
            Test : Membership := Inside;
            Going : Direction := Forward)
            return Natural;

         function Index (
            Source : Bounded.Bounded_String;
            Set : Fixed_Maps.Character_Set;
            Test : Membership := Inside;
            Going : Direction := Forward)
            return Natural;

         function Count (
            Source : Bounded.Bounded_String;
            Pattern : String_Type;
            Mapping : Fixed_Maps.Character_Mapping)
            return Natural;

         function Count (
            Source : Bounded.Bounded_String;
            Pattern : String_Type;
            Mapping : not null access function (From : Wide_Wide_Character)
               return Wide_Wide_Character)
            return Natural;

         function Count_Element (
            Source : Bounded.Bounded_String;
            Pattern : String_Type;
            Mapping : not null access function (From : Character_Type)
               return Character_Type)
            return Natural;

         function Count (
            Source : Bounded.Bounded_String;
            Set : Fixed_Maps.Character_Set)
            return Natural;

         procedure Find_Token (
            Source : Bounded.Bounded_String;
            Set : Fixed_Maps.Character_Set;
            From : Positive;
            Test : Membership;
            First : out Positive;
            Last : out Natural);

         procedure Find_Token (
            Source : Bounded.Bounded_String;
            Set : Fixed_Maps.Character_Set;
            Test : Membership;
            First : out Positive;
            Last : out Natural);

         --  String translation subprograms

         function Translate (
            Source : Bounded.Bounded_String;
            Mapping : Fixed_Maps.Character_Mapping;
            Drop : Truncation := Error) -- additional
            return Bounded.Bounded_String;

         procedure Translate (
            Source : in out Bounded.Bounded_String;
            Mapping : Fixed_Maps.Character_Mapping;
            Drop : Truncation := Error); -- additional

         function Translate (
            Source : Bounded.Bounded_String;
            Mapping : not null access function (From : Wide_Wide_Character)
               return Wide_Wide_Character;
            Drop : Truncation := Error) -- additional
            return Bounded.Bounded_String;

         procedure Translate (
            Source : in out Bounded.Bounded_String;
            Mapping : not null access function (From : Wide_Wide_Character)
               return Wide_Wide_Character;
            Drop : Truncation := Error); -- additional

         function Translate_Element (
            Source : Bounded.Bounded_String;
            Mapping : not null access function (From : Character_Type)
               return Character_Type)
            return Bounded.Bounded_String;

         procedure Translate_Element (
            Source : in out Bounded.Bounded_String;
            Mapping : not null access function (From : Character_Type)
               return Character_Type);

         --  String selector subprograms

         function Trim (
            Source : Bounded.Bounded_String;
            Left : Fixed_Maps.Character_Set;
            Right : Fixed_Maps.Character_Set)
            return Bounded.Bounded_String;

         procedure Trim (
            Source : in out Bounded.Bounded_String;
            Left : Fixed_Maps.Character_Set;
            Right : Fixed_Maps.Character_Set);

      end Generic_Bounded_Length;

   end Generic_Maps;

end Ada.Strings.Generic_Bounded.Generic_Functions;
