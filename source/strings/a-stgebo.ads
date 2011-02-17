pragma License (Unrestricted);
--  generic implementation of Ada.Strings.Bounded
with System.Arrays;
generic
   type Character_Type is (<>);
   type String_Type is array (Positive range <>) of Character_Type;
package Ada.Strings.Generic_Bounded is
   pragma Preelaborate;

   --  extended
   type Bounded_String (Max : Positive) is record
      Length : Natural := 0;
      Data : aliased String_Type (1 .. Max);
   end record;

   function Length (Source : Bounded_String) return Natural;
   pragma Inline (Length);

   function To_String (Source : Bounded_String) return String_Type;

   procedure Set_Bounded_String (
      Target : out Bounded_String;
      Source : String_Type;
      Drop : Truncation := Error);

   procedure Append (
      Source : in out Bounded_String;
      New_Item : Bounded_String;
      Drop : Truncation := Error);

   procedure Append (
      Source : in out Bounded_String;
      New_Item : String_Type;
      Drop : Truncation := Error);

   procedure Append (
      Source : in out Bounded_String;
      New_Item : Character_Type;
      Drop : Truncation := Error);

   function Element (
      Source : Bounded_String;
      Index : Positive)
      return Character_Type;
   pragma Inline (Element);

   procedure Replace_Element (
      Source : in out Bounded_String;
      Index : Positive;
      By : Character_Type);
   pragma Inline (Replace_Element);

   function Slice (
      Source : Bounded_String;
      Low : Positive;
      High : Natural)
      return String_Type;
   pragma Inline (Slice);

   procedure Bounded_Slice (
      Source : Bounded_String;
      Target : out Bounded_String;
      Low : Positive;
      High : Natural);

   function "=" (Left, Right : Bounded_String) return Boolean;
   function "=" (Left : Bounded_String; Right : String_Type) return Boolean;
   function "=" (Left : String_Type; Right : Bounded_String) return Boolean;

   function "<" (Left, Right : Bounded_String) return Boolean;
   function "<" (Left : Bounded_String; Right : String_Type) return Boolean;
   function "<" (Left : String_Type; Right : Bounded_String) return Boolean;

   function "<=" (Left, Right : Bounded_String) return Boolean;
   function "<=" (Left : Bounded_String; Right : String_Type) return Boolean;
   function "<=" (Left : String_Type; Right : Bounded_String) return Boolean;

   function ">" (Left, Right : Bounded_String) return Boolean;
   function ">" (Left : Bounded_String; Right : String_Type) return Boolean;
   function ">" (Left : String_Type; Right : Bounded_String) return Boolean;

   function ">=" (Left, Right : Bounded_String) return Boolean;
   function ">=" (Left : Bounded_String; Right : String_Type) return Boolean;
   function ">=" (Left : String_Type; Right : Bounded_String) return Boolean;

   --  extended
   package Slicing is new System.Arrays.Generic_Slicing (
      Positive,
      Character_Type,
      String_Type);
   function Reference (Source : not null access Bounded_String)
      return Slicing.Reference_Type;
   function Reference (
      Source : not null access Bounded_String;
      First_Index : Positive;
      Last_Index : Natural)
      return Slicing.Reference_Type;

   generic
      Max : Positive; -- Maximum length of a Bounded_String
   package Generic_Bounded_Length is

      Max_Length : constant Positive := Max;

--    type Bounded_String is private;
      type Bounded_String is
         new Generic_Bounded.Bounded_String (Max); --  extended

--    Null_Bounded_String : constant Bounded_String;
      function Null_Bounded_String return Bounded_String;
      pragma Inline (Null_Bounded_String);

      subtype Length_Range is Natural range 0 .. Max_Length;

--    function Length (Source : Bounded_String) return Length_Range;
      --  function Length is inherited

      --  Conversion, Concatenation, and Selection functions

      function To_Bounded_String (
         Source : String_Type;
         Drop : Truncation := Error)
         return Bounded_String;

--    function To_String (Source : Bounded_String) return String_Type;
      --  function To_String is inheried

--    procedure Set_Bounded_String (
--       Target : out Bounded_String;
--       Source : String_Type;
--       Drop : Truncation := Error);
      --  procedure Set_Bounded_String is inheried

      function Append (
         Left, Right : Bounded_String;
         Drop : Truncation := Error)
         return Bounded_String;

      function Append (
         Left : Bounded_String;
         Right : String_Type;
         Drop : Truncation := Error)
         return Bounded_String;

      function Append (
         Left : String_Type;
         Right : Bounded_String;
         Drop : Truncation := Error)
         return Bounded_String;

      function Append (
         Left : Bounded_String;
         Right : Character_Type;
         Drop : Truncation := Error)
         return Bounded_String;

      function Append (
         Left : Character_Type;
         Right : Bounded_String;
         Drop : Truncation := Error)
         return Bounded_String;

--    procedure Append (
--       Source : in out Bounded_String;
--       New_Item : Bounded_String;
--       Drop : Truncation := Error);
      --  procedure Append is inherited

--    procedure Append (
--       Source : in out Bounded_String;
--       New_Item : String_Type;
--       Drop : Truncation := Error);
      --  procedure Append is inherited

--    procedure Append (
--       Source : in out Bounded_String;
--       New_Item : Character_Type;
--       Drop : Truncation := Error);
      --  procedure Append is inherited

      function "&" (Left, Right : Bounded_String)
         return Bounded_String;

      function "&" (Left : Bounded_String; Right : String_Type)
         return Bounded_String;

      function "&" (Left : String_Type; Right : Bounded_String)
         return Bounded_String;

      function "&" (Left : Bounded_String; Right : Character_Type)
         return Bounded_String;

      function "&" (Left : Character_Type; Right : Bounded_String)
         return Bounded_String;

--    function Element (
--       Source : Bounded_String;
--       Index : Positive)
--       return Character_Type;
      --  function Element is inherited

--    procedure Replace_Element (
--       Source : in out Bounded_String;
--       Index : Positive;
--       By : Character_Type);
      --  procedure Replace_Element is inherited

--    function Slice (
--       Source : Bounded_String;
--       Low : Positive;
--       High : Natural)
--       return String_Type;
      --  function Slice is inherited

      function Bounded_Slice (
         Source : Bounded_String;
         Low : Positive;
         High : Natural)
         return Bounded_String;

--    procedure Bounded_Slice (
--       Source : Bounded_String;
--       Target : out Bounded_String;
--       Low : Positive;
--       High : Natural);
      --  procedure Bounded_Slice is inherited

--    function "=" (Left, Right : Bounded_String) return Boolean;
--    function "=" (Left : Bounded_String; Right : String_Type) return Boolean;
--    function "=" (Left : String_Type; Right : Bounded_String) return Boolean;
--    function "<" (Left, Right : Bounded_String) return Boolean;
--    function "<" (Left : Bounded_String; Right : String_Type) return Boolean;
--    function "<" (Left : String_Type; Right : Bounded_String) return Boolean;
--    function "<=" (Left, Right : Bounded_String) return Boolean;
--    function "<=" (Left : Bounded_String; Right : String_Type)
--       return Boolean;
--    function "<=" (Left : String_Type; Right : Bounded_String)
--       return Boolean;
--    function ">" (Left, Right : Bounded_String) return Boolean;
--    function ">" (Left : Bounded_String; Right : String_Type) return Boolean;
--    function ">" (Left : String_Type; Right : Bounded_String) return Boolean;
--    function ">=" (Left, Right : Bounded_String) return Boolean;
--    function ">=" (Left : Bounded_String; Right : String_Type)
--       return Boolean;
--    function ">=" (Left : String_Type; Right : Bounded_String)
--       return Boolean;
      --  "=", "<", "<=", ">" and ">=" are inherited

      --  String constructor subprograms

      function "*" (Left : Natural; Right : Character_Type)
         return Bounded_String;
      --  function "*" is required to be primitive by CXA4007

      function "*" (Left : Natural; Right : String_Type)
         return Bounded_String;
      --  function "*" is required to be primitive by CXA4007

      function "*" (Left : Natural; Right : Bounded_String)
         return Bounded_String;

      function Replicate (
         Count : Natural;
         Item : Character_Type;
         Drop : Truncation := Error)
         return Bounded_String;

      function Replicate (
         Count : Natural;
         Item : String_Type;
         Drop : Truncation := Error)
         return Bounded_String;

      function Replicate (
         Count : Natural;
         Item : Bounded_String;
         Drop : Truncation := Error)
         return Bounded_String;

      generic
         Space : Character_Type;
         with function Fixed_Index_From (
            Source : String_Type;
            Pattern : String_Type;
            From : Positive;
            Going : Direction)
            return Natural;
         with function Fixed_Index (
            Source : String_Type;
            Pattern : String_Type;
            Going : Direction)
            return Natural;
         with function Fixed_Index_Non_Blank_From (
            Source : String_Type;
            From : Positive;
            Going : Direction)
            return Natural;
         with function Fixed_Index_Non_Blank (
            Source : String_Type;
            Going : Direction)
            return Natural;
         with function Fixed_Count (
            Source : String_Type;
            Pattern : String_Type)
            return Natural;
         with function Fixed_Replace_Slice (
            Source : String_Type;
            Low : Positive;
            High : Natural;
            By : String_Type)
            return String_Type;
         with function Fixed_Insert (
            Source : String_Type;
            Before : Positive;
            New_Item : String_Type)
            return String_Type;
         with function Fixed_Overwrite (
            Source : String_Type;
            Position : Positive;
            New_Item : String_Type)
            return String_Type;
         with function Fixed_Delete (
            Source : String_Type;
            From : Positive;
            Through : Natural)
            return String_Type;
         with procedure Fixed_Trim (
            Source : String_Type;
            Side : Trim_End;
            Left : Character_Type;
            Right : Character_Type;
            First : out Positive;
            Last : out Natural);
         with function Fixed_Head (
            Source : String_Type;
            Count : Natural;
            Pad : Character_Type)
            return String_Type;
         with function Fixed_Tail (
            Source : String_Type;
            Count : Natural;
            Pad : Character_Type)
            return String_Type;
      package Generic_Functions is

         --  Search subprograms

         function Index (
            Source : Bounded_String;
            Pattern : String_Type;
            From : Positive;
            Going : Direction := Forward)
            return Natural;

         function Index (
            Source : Bounded_String;
            Pattern : String_Type;
            Going : Direction := Forward)
            return Natural;

         function Index_Non_Blank (
            Source : Bounded_String;
            From : Positive;
            Going : Direction := Forward)
            return Natural;

         function Index_Non_Blank (
            Source : Bounded_String;
            Going : Direction := Forward)
            return Natural;

         function Count (
            Source : Bounded_String;
            Pattern : String_Type)
            return Natural;

         --  String transformation subprograms

         function Replace_Slice (
            Source : Bounded_String;
            Low : Positive;
            High : Natural;
            By : String_Type;
            Drop : Truncation := Error)
            return Bounded_String;

         procedure Replace_Slice (
            Source : in out Bounded_String;
            Low : Positive;
            High : Natural;
            By : String_Type;
            Drop : Truncation := Error);

         function Insert (
            Source : Bounded_String;
            Before : Positive;
            New_Item : String_Type;
            Drop : Truncation := Error)
            return Bounded_String;

         procedure Insert (
            Source : in out Bounded_String;
            Before : Positive;
            New_Item : String_Type;
            Drop : Truncation := Error);

         function Overwrite (
            Source : Bounded_String;
            Position : Positive;
            New_Item : String_Type;
            Drop : Truncation := Error)
            return Bounded_String;

         procedure Overwrite (
            Source : in out Bounded_String;
            Position : Positive;
            New_Item : String_Type;
            Drop : Truncation := Error);

         function Delete (
            Source : Bounded_String;
            From : Positive;
            Through : Natural)
            return Bounded_String;

         procedure Delete (
            Source : in out Bounded_String;
            From : Positive;
            Through : Natural);

         --  String selector subprograms

         function Trim (
            Source : Bounded_String;
            Side : Trim_End;
            Left : Character_Type := Space; -- extended
            Right : Character_Type := Space) -- extended
            return Bounded_String;

         procedure Trim (
            Source : in out Bounded_String;
            Side : Trim_End;
            Left : Character_Type := Space; -- extended
            Right : Character_Type := Space); -- extended

         function Head (
            Source : Bounded_String;
            Count : Natural;
            Pad : Character_Type := Space;
            Drop : Truncation := Error)
            return Bounded_String;

         procedure Head (
            Source : in out Bounded_String;
            Count : Natural;
            Pad : Character_Type := Space;
            Drop : Truncation := Error);

         function Tail (
            Source : Bounded_String;
            Count : Natural;
            Pad : Character_Type := Space;
            Drop : Truncation := Error)
            return Bounded_String;

         procedure Tail (
            Source : in out Bounded_String;
            Count : Natural;
            Pad : Character_Type := Space;
            Drop : Truncation := Error);

         generic
            type Character_Set is private;
            type Character_Mapping is private;
            with function Fixed_Index_Mapping_From (
               Source : String_Type;
               Pattern : String_Type;
               From : Positive;
               Going : Direction;
               Mapping : Character_Mapping)
               return Natural;
            with function Fixed_Index_Mapping (
               Source : String_Type;
               Pattern : String_Type;
               Going : Direction;
               Mapping : Character_Mapping)
               return Natural;
            with function Fixed_Index_Mapping_Function_From (
               Source : String_Type;
               Pattern : String_Type;
               From : Positive;
               Going : Direction;
               Mapping : not null access function (From : Wide_Wide_Character)
                  return Wide_Wide_Character)
               return Natural;
            with function Fixed_Index_Mapping_Function (
               Source : String_Type;
               Pattern : String_Type;
               Going : Direction;
               Mapping : not null access function (From : Wide_Wide_Character)
                  return Wide_Wide_Character)
               return Natural;
            with function Fixed_Index_Mapping_Function_Per_Element_From (
               Source : String_Type;
               Pattern : String_Type;
               From : Positive;
               Going : Direction;
               Mapping : not null access function (From : Character_Type)
                  return Character_Type)
               return Natural;
            with function Fixed_Index_Mapping_Function_Per_Element (
               Source : String_Type;
               Pattern : String_Type;
               Going : Direction;
               Mapping : not null access function (From : Character_Type)
                  return Character_Type)
               return Natural;
            with function Fixed_Index_Set_From (
               Source : String_Type;
               Set : Character_Set;
               From : Positive;
               Test : Membership;
               Going : Direction)
               return Natural;
            with function Fixed_Index_Set (
               Source : String_Type;
               Set : Character_Set;
               Test : Membership;
               Going : Direction)
               return Natural;
            with function Fixed_Count_Mapping (
               Source : String_Type;
               Pattern : String_Type;
               Mapping : Character_Mapping)
               return Natural;
            with function Fixed_Count_Mapping_Function (
               Source : String_Type;
               Pattern : String_Type;
               Mapping : not null access function (From : Wide_Wide_Character)
                  return Wide_Wide_Character)
               return Natural;
            with function Fixed_Count_Mapping_Function_Per_Element (
               Source : String_Type;
               Pattern : String_Type;
               Mapping : not null access function (From : Character_Type)
                  return Character_Type)
               return Natural;
            with function Fixed_Count_Set (
               Source : String_Type;
               Set : Character_Set)
               return Natural;
            with procedure Fixed_Find_Token_From (
               Source : String_Type;
               Set : Character_Set;
               From : Positive;
               Test : Membership;
               First : out Positive;
               Last : out Natural);
            with procedure Fixed_Find_Token (
               Source : String_Type;
               Set : Character_Set;
               Test : Membership;
               First : out Positive;
               Last : out Natural);
            with function Fixed_Translate_Mapping (
               Source : String_Type;
               Mapping : Character_Mapping)
               return String_Type;
            with function Fixed_Translate_Mapping_Function (
               Source : String_Type;
               Mapping : not null access function (From : Wide_Wide_Character)
                  return Wide_Wide_Character)
               return String_Type;
            with function Fixed_Translate_Mapping_Function_Per_Element (
               Source : String_Type;
               Mapping : not null access function (From : Character_Type)
                  return Character_Type)
               return String_Type;
            with procedure Fixed_Trim_Set (
               Source : String_Type;
               Left : Character_Set;
               Right : Character_Set;
               First : out Positive;
               Last : out Natural);
         package Generic_Maps is

            --  Search subprograms

            function Index (
               Source : Bounded_String;
               Pattern : String_Type;
               From : Positive;
               Going : Direction := Forward;
               Mapping : Character_Mapping)
               return Natural;

            function Index (
               Source : Bounded_String;
               Pattern : String_Type;
               Going : Direction := Forward;
               Mapping : Character_Mapping)
               return Natural;

            function Index (
               Source : Bounded_String;
               Pattern : String_Type;
               From : Positive;
               Going : Direction := Forward;
               Mapping : not null access function (From : Wide_Wide_Character)
                  return Wide_Wide_Character)
               return Natural;

            function Index (
               Source : Bounded_String;
               Pattern : String_Type;
               Going : Direction := Forward;
               Mapping : not null access function (From : Wide_Wide_Character)
                  return Wide_Wide_Character)
               return Natural;

            function Index_Per_Element (
               Source : Bounded_String;
               Pattern : String_Type;
               From : Positive;
               Going : Direction := Forward;
               Mapping : not null access function (From : Character_Type)
                  return Character_Type)
               return Natural;

            function Index_Per_Element (
               Source : Bounded_String;
               Pattern : String_Type;
               Going : Direction := Forward;
               Mapping : not null access function (From : Character_Type)
                  return Character_Type)
               return Natural;

            function Index (
               Source : Bounded_String;
               Set : Character_Set;
               From : Positive;
               Test : Membership := Inside;
               Going : Direction := Forward)
               return Natural;

            function Index (
               Source : Bounded_String;
               Set : Character_Set;
               Test : Membership := Inside;
               Going : Direction := Forward)
               return Natural;

            function Count (
               Source : Bounded_String;
               Pattern : String_Type;
               Mapping : Character_Mapping)
               return Natural;

            function Count (
               Source : Bounded_String;
               Pattern : String_Type;
               Mapping : not null access function (From : Wide_Wide_Character)
                  return Wide_Wide_Character)
               return Natural;

            function Count_Per_Element (
               Source : Bounded_String;
               Pattern : String_Type;
               Mapping : not null access function (From : Character_Type)
                  return Character_Type)
               return Natural;

            function Count (Source : Bounded_String; Set : Character_Set)
               return Natural;

            procedure Find_Token (
               Source : Bounded_String;
               Set : Character_Set;
               From : Positive;
               Test : Membership;
               First : out Positive;
               Last : out Natural);

            procedure Find_Token (
               Source : Bounded_String;
               Set : Character_Set;
               Test : Membership;
               First : out Positive;
               Last : out Natural);

            --  String translation subprograms

            function Translate (
               Source : Bounded_String;
               Mapping : Character_Mapping;
               Drop : Truncation := Error) -- extended
               return Bounded_String;

            procedure Translate (
               Source : in out Bounded_String;
               Mapping : Character_Mapping;
               Drop : Truncation := Error); -- extended

            function Translate (
               Source : Bounded_String;
               Mapping : not null access function (From : Wide_Wide_Character)
                  return Wide_Wide_Character;
               Drop : Truncation := Error) -- extended
               return Bounded_String;

            procedure Translate (
               Source : in out Bounded_String;
               Mapping : not null access function (From : Wide_Wide_Character)
                  return Wide_Wide_Character;
               Drop : Truncation := Error); -- extended

            function Translate_Per_Element (
               Source : Bounded_String;
               Mapping : not null access function (From : Character_Type)
                  return Character_Type)
               return Bounded_String;

            procedure Translate_Per_Element (
               Source : in out Bounded_String;
               Mapping : not null access function (From : Character_Type)
                  return Character_Type);

            --  String selector subprograms

            function Trim (
               Source : Bounded_String;
               Left : Character_Set;
               Right : Character_Set)
               return Bounded_String;

            procedure Trim (
               Source : in out Bounded_String;
               Left : Character_Set;
               Right : Character_Set);

         end Generic_Maps;

      end Generic_Functions;

   end Generic_Bounded_Length;

end Ada.Strings.Generic_Bounded;
