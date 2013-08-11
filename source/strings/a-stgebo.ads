pragma License (Unrestricted);
--  generic implementation of Ada.Strings.Bounded
with Ada.References;
with Ada.Streams;
generic
   type Character_Type is (<>);
   type String_Type is array (Positive range <>) of Character_Type;
   with procedure Read (
      Stream : not null access Streams.Root_Stream_Type'Class;
      Item : out String_Type);
   with procedure Write (
      Stream : not null access Streams.Root_Stream_Type'Class;
      Item : String_Type);
   with package Slicing is new References.Generic_Slicing (
      Positive,
      Character_Type,
      String_Type);
package Ada.Strings.Generic_Bounded is
   pragma Preelaborate;

   --  extended
   type Bounded_String (Capacity : Positive) is record
      Length : Natural := 0;
      Element : aliased String_Type (1 .. Capacity);
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
   pragma Inline ("<=");

   function ">" (Left, Right : Bounded_String) return Boolean;
   function ">" (Left : Bounded_String; Right : String_Type) return Boolean;
   function ">" (Left : String_Type; Right : Bounded_String) return Boolean;
   pragma Inline (">");

   function ">=" (Left, Right : Bounded_String) return Boolean;
   function ">=" (Left : Bounded_String; Right : String_Type) return Boolean;
   function ">=" (Left : String_Type; Right : Bounded_String) return Boolean;
   pragma Inline (">=");

   --  extended
   --  There are reference version of slicing functions.
   function Constant_Reference (
      Source : aliased Bounded_String)
      return Slicing.Constant_Reference_Type;
   function Constant_Reference (
      Source : aliased Bounded_String;
      First_Index : Positive;
      Last_Index : Natural)
      return Slicing.Constant_Reference_Type;
   function Reference (
      Source : aliased in out Bounded_String)
      return Slicing.Reference_Type;
   function Reference (
      Source : aliased in out Bounded_String;
      First_Index : Positive;
      Last_Index : Natural)
      return Slicing.Reference_Type;

   generic
      Max : Positive; -- Maximum length of a Bounded_String
   package Generic_Bounded_Length is

      Max_Length : constant Positive := Max;

      --  modified
--    type Bounded_String is private;
      type Bounded_String is new Generic_Bounded.Bounded_String (Max);

      --  modified
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

      --  extended for shorthand
      function "+" (Source : String_Type) return Bounded_String;

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

   private

      package No_Primitives is
         procedure Read (
            Stream : not null access Streams.Root_Stream_Type'Class;
            Item : out Bounded_String);
         function Input (
            Stream : not null access Streams.Root_Stream_Type'Class)
            return Bounded_String;
         procedure Write (
            Stream : not null access Streams.Root_Stream_Type'Class;
            Item : Bounded_String);
      end No_Primitives;

      for Bounded_String'Read use No_Primitives.Read;
      for Bounded_String'Write use No_Primitives.Write;
      for Bounded_String'Input use No_Primitives.Input;
      for Bounded_String'Output use No_Primitives.Write;

   end Generic_Bounded_Length;

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
      with procedure Fixed_Delete (
         Source : in out String_Type;
         Last : in out Natural;
         From : Positive;
         Through : Natural);
      with procedure Fixed_Trim (
         Source : String_Type;
         Side : Trim_End;
         Blank : Character_Type;
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

         function Count (
            Source : Bounded.Bounded_String;
            Pattern : String_Type)
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
            Blank : Character_Type := Space) -- additional
            return Bounded.Bounded_String;

         procedure Trim (
            Source : in out Bounded.Bounded_String;
            Side : Trim_End;
            Blank : Character_Type := Space); -- additional

         function Head (
            Source : Bounded.Bounded_String;
            Count : Natural;
            Pad : Character_Type := Space;
            Drop : Truncation := Error)
            return Bounded.Bounded_String;

         procedure Head (
            Source : in out Bounded.Bounded_String;
            Count : Natural;
            Pad : Character_Type := Space;
            Drop : Truncation := Error);

         function Tail (
            Source : Bounded.Bounded_String;
            Count : Natural;
            Pad : Character_Type := Space;
            Drop : Truncation := Error)
            return Bounded.Bounded_String;

         procedure Tail (
            Source : in out Bounded.Bounded_String;
            Count : Natural;
            Pad : Character_Type := Space;
            Drop : Truncation := Error);

      end Generic_Bounded_Length;

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
               Mapping : Character_Mapping)
               return Natural;

            function Index (
               Source : Bounded.Bounded_String;
               Pattern : String_Type;
               Going : Direction := Forward;
               Mapping : Character_Mapping)
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

            function Index_Per_Element (
               Source : Bounded.Bounded_String;
               Pattern : String_Type;
               From : Positive;
               Going : Direction := Forward;
               Mapping : not null access function (From : Character_Type)
                  return Character_Type)
               return Natural;

            function Index_Per_Element (
               Source : Bounded.Bounded_String;
               Pattern : String_Type;
               Going : Direction := Forward;
               Mapping : not null access function (From : Character_Type)
                  return Character_Type)
               return Natural;

            function Index (
               Source : Bounded.Bounded_String;
               Set : Character_Set;
               From : Positive;
               Test : Membership := Inside;
               Going : Direction := Forward)
               return Natural;

            function Index (
               Source : Bounded.Bounded_String;
               Set : Character_Set;
               Test : Membership := Inside;
               Going : Direction := Forward)
               return Natural;

            function Count (
               Source : Bounded.Bounded_String;
               Pattern : String_Type;
               Mapping : Character_Mapping)
               return Natural;

            function Count (
               Source : Bounded.Bounded_String;
               Pattern : String_Type;
               Mapping : not null access function (From : Wide_Wide_Character)
                  return Wide_Wide_Character)
               return Natural;

            function Count_Per_Element (
               Source : Bounded.Bounded_String;
               Pattern : String_Type;
               Mapping : not null access function (From : Character_Type)
                  return Character_Type)
               return Natural;

            function Count (
               Source : Bounded.Bounded_String;
               Set : Character_Set)
               return Natural;

            procedure Find_Token (
               Source : Bounded.Bounded_String;
               Set : Character_Set;
               From : Positive;
               Test : Membership;
               First : out Positive;
               Last : out Natural);

            procedure Find_Token (
               Source : Bounded.Bounded_String;
               Set : Character_Set;
               Test : Membership;
               First : out Positive;
               Last : out Natural);

            --  String translation subprograms

            function Translate (
               Source : Bounded.Bounded_String;
               Mapping : Character_Mapping;
               Drop : Truncation := Error) -- additional
               return Bounded.Bounded_String;

            procedure Translate (
               Source : in out Bounded.Bounded_String;
               Mapping : Character_Mapping;
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

            function Translate_Per_Element (
               Source : Bounded.Bounded_String;
               Mapping : not null access function (From : Character_Type)
                  return Character_Type)
               return Bounded.Bounded_String;

            procedure Translate_Per_Element (
               Source : in out Bounded.Bounded_String;
               Mapping : not null access function (From : Character_Type)
                  return Character_Type);

            --  String selector subprograms

            function Trim (
               Source : Bounded.Bounded_String;
               Left : Character_Set;
               Right : Character_Set)
               return Bounded.Bounded_String;

            procedure Trim (
               Source : in out Bounded.Bounded_String;
               Left : Character_Set;
               Right : Character_Set);

         end Generic_Bounded_Length;

      end Generic_Maps;

   end Generic_Functions;

end Ada.Strings.Generic_Bounded;
