pragma License (Unrestricted);
--  generic implementation of Ada.Strings.Unbounded
with Ada.References;
with Ada.Streams;
with Ada.Unchecked_Deallocation;
private with Ada.Finalization;
private with System.Reference_Counting;
generic
   type Character_Type is (<>);
   type String_Type is array (Positive range <>) of Character_Type;
   with procedure Read (
      Stream : not null access Streams.Root_Stream_Type'Class;
      Item : out String_Type) is String_Type'Read;
   with procedure Write (
      Stream : not null access Streams.Root_Stream_Type'Class;
      Item : String_Type) is String_Type'Write;
   with package Slicing is
      new References.Generic_Slicing (Positive, Character_Type, String_Type);
package Ada.Strings.Generic_Unbounded is
   pragma Preelaborate;

   --  modified, tagged for dot notation
--  type Unbounded_String is private;
   type Unbounded_String is tagged private;
   pragma Preelaborable_Initialization (Unbounded_String);

   --  modified
--  Null_Unbounded_String : constant Unbounded_String;
   function Null_Unbounded_String return Unbounded_String;

   --  extended
   function Is_Null (Source : Unbounded_String) return Boolean;
   pragma Inline (Is_Null);

   function Length (Source : Unbounded_String) return Natural;
   pragma Inline (Length);

   --  extended
   procedure Set_Length (Source : in out Unbounded_String; Length : Natural);
   function Capacity (Source : Unbounded_String'Class) return Natural;
   pragma Inline (Capacity);

   type String_Access is access all String_Type;
--  procedure Free (X : in out String_Access);
   procedure Free is new Unchecked_Deallocation (String_Type, String_Access);

   --  extended
   type String_Constant_Access is access constant String_Type;

   --  Conversion, Concatenation, and Selection functions

   function To_Unbounded_String (Source : String_Type)
      return Unbounded_String;
   --  extended for shorthand
   function "+" (Source : String_Type) return Unbounded_String
      renames To_Unbounded_String;

   function To_Unbounded_String (Length : Natural)
      return Unbounded_String;

   function To_String (Source : Unbounded_String) return String_Type;

   procedure Set_Unbounded_String (
      Target : out Unbounded_String;
      Source : String_Type);

   procedure Append (
      Source : in out Unbounded_String;
      New_Item : Unbounded_String);

   procedure Append (
      Source : in out Unbounded_String;
      New_Item : String_Type);

   procedure Append (
      Source : in out Unbounded_String;
      New_Item : Character_Type);

   function "&" (Left, Right : Unbounded_String) return Unbounded_String;

   function "&" (Left : Unbounded_String; Right : String_Type)
      return Unbounded_String;

   function "&" (Left : String_Type; Right : Unbounded_String)
      return Unbounded_String;

   function "&" (Left : Unbounded_String; Right : Character_Type)
      return Unbounded_String;

   function "&" (Left : Character_Type; Right : Unbounded_String)
      return Unbounded_String;

   function Element (Source : Unbounded_String; Index : Positive)
      return Character_Type;
   pragma Inline (Element);

   procedure Replace_Element (
      Source : in out Unbounded_String;
      Index : Positive;
      By : Character_Type);

   function Slice (
      Source : Unbounded_String;
      Low : Positive;
      High : Natural)
      return String_Type;

   function Unbounded_Slice (
      Source : Unbounded_String;
      Low : Positive;
      High : Natural)
      return Unbounded_String;

   procedure Unbounded_Slice (
      Source : Unbounded_String;
      Target : out Unbounded_String;
      Low : Positive;
      High : Natural);

   overriding function "=" (Left, Right : Unbounded_String) return Boolean;
   function "=" (Left : Unbounded_String; Right : String_Type) return Boolean;
   function "=" (Left : String_Type; Right : Unbounded_String) return Boolean;

   function "<" (Left, Right : Unbounded_String) return Boolean;
   function "<" (Left : Unbounded_String; Right : String_Type) return Boolean;
   function "<" (Left : String_Type; Right : Unbounded_String) return Boolean;

   function "<=" (Left, Right : Unbounded_String) return Boolean;
   function "<=" (Left : Unbounded_String; Right : String_Type) return Boolean;
   function "<=" (Left : String_Type; Right : Unbounded_String) return Boolean;
   pragma Inline ("<=");

   function ">" (Left, Right : Unbounded_String) return Boolean;
   function ">" (Left : Unbounded_String; Right : String_Type) return Boolean;
   function ">" (Left : String_Type; Right : Unbounded_String) return Boolean;
   pragma Inline (">");

   function ">=" (Left, Right : Unbounded_String) return Boolean;
   function ">=" (Left : Unbounded_String; Right : String_Type) return Boolean;
   function ">=" (Left : String_Type; Right : Unbounded_String) return Boolean;
   pragma Inline (">=");

   --  extended
   --  There are reference version of slicing functions.
   function Constant_Reference (
      Source : aliased Unbounded_String)
      return Slicing.Constant_Reference_Type;
   function Constant_Reference (
      Source : aliased Unbounded_String;
      First_Index : Positive;
      Last_Index : Natural)
      return Slicing.Constant_Reference_Type;
   function Reference (
      Source : aliased in out Unbounded_String)
      return Slicing.Reference_Type;
   function Reference (
      Source : aliased in out Unbounded_String;
      First_Index : Positive;
      Last_Index : Natural)
      return Slicing.Reference_Type;

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
         Blank : Character_Type := Space) -- additional
         return Unbounded_String;

      procedure Trim (
         Source : in out Unbounded_String;
         Side : Trim_End;
         Blank : Character_Type := Space); -- additional

      function Head (
         Source : Unbounded_String;
         Count : Natural;
         Pad : Character_Type := Space)
         return Unbounded_String;

      procedure Head (
         Source : in out Unbounded_String;
         Count : Natural;
         Pad : Character_Type := Space);

      function Tail (
         Source : Unbounded_String;
         Count : Natural;
         Pad : Character_Type := Space)
         return Unbounded_String;

      procedure Tail (
         Source : in out Unbounded_String;
         Count : Natural;
         Pad : Character_Type := Space);

      --  String constructor functions

      function "*" (Left : Natural; Right : Character_Type)
         return Unbounded_String;

      function "*" (Left : Natural; Right : String_Type)
         return Unbounded_String;

      function "*" (Left : Natural; Right : Unbounded_String)
         return Unbounded_String;

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
            Source : Unbounded_String;
            Pattern : String_Type;
            From : Positive;
            Going : Direction := Forward;
            Mapping : Character_Mapping)
            return Natural;

         function Index (
            Source : Unbounded_String;
            Pattern : String_Type;
            Going : Direction := Forward;
            Mapping : Character_Mapping)
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

         function Index_Per_Element (
            Source : Unbounded_String;
            Pattern : String_Type;
            From : Positive;
            Going : Direction := Forward;
            Mapping : not null access function (From : Character_Type)
               return Character_Type)
            return Natural;

         function Index_Per_Element (
            Source : Unbounded_String;
            Pattern : String_Type;
            Going : Direction := Forward;
            Mapping : not null access function (From : Character_Type)
               return Character_Type)
            return Natural;

         function Index (
            Source : Unbounded_String;
            Set : Character_Set;
            From : Positive;
            Test : Membership := Inside;
            Going : Direction := Forward)
            return Natural;

         function Index (
            Source : Unbounded_String;
            Set : Character_Set;
            Test : Membership := Inside;
            Going : Direction := Forward)
            return Natural;

         function Count (
            Source : Unbounded_String;
            Pattern : String_Type;
            Mapping : Character_Mapping)
            return Natural;

         function Count (
            Source : Unbounded_String;
            Pattern : String_Type;
            Mapping : not null access function (From : Wide_Wide_Character)
               return Wide_Wide_Character)
            return Natural;

         function Count_Per_Element (
            Source : Unbounded_String;
            Pattern : String_Type;
            Mapping : not null access function (From : Character_Type)
               return Character_Type)
            return Natural;

         function Count (Source : Unbounded_String; Set : Character_Set)
            return Natural;

         procedure Find_Token (
            Source : Unbounded_String;
            Set : Character_Set;
            From : Positive;
            Test : Membership;
            First : out Positive;
            Last : out Natural);

         procedure Find_Token (
            Source : Unbounded_String;
            Set : Character_Set;
            Test : Membership;
            First : out Positive;
            Last : out Natural);

         --  String translation subprograms

         function Translate (
            Source : Unbounded_String;
            Mapping : Character_Mapping)
            return Unbounded_String;

         procedure Translate (
            Source : in out Unbounded_String;
            Mapping : Character_Mapping);

         function Translate (
            Source : Unbounded_String;
            Mapping : not null access function (From : Wide_Wide_Character)
               return Wide_Wide_Character)
            return Unbounded_String;

         procedure Translate (
            Source : in out Unbounded_String;
            Mapping : not null access function (From : Wide_Wide_Character)
               return Wide_Wide_Character);

         function Translate_Per_Element (
            Source : Unbounded_String;
            Mapping : not null access function (From : Character_Type)
               return Character_Type)
            return Unbounded_String;

         procedure Translate_Per_Element (
            Source : in out Unbounded_String;
            Mapping : not null access function (From : Character_Type)
               return Character_Type);

         --  String selector subprograms

         function Trim (
            Source : Unbounded_String;
            Left : Character_Set;
            Right : Character_Set)
            return Unbounded_String;

         procedure Trim (
            Source : in out Unbounded_String;
            Left : Character_Set;
            Right : Character_Set);

      end Generic_Maps;

   end Generic_Functions;

   --  for making constant Unbounded_String without dynamic allocation
   generic
      S : not null String_Constant_Access;
   package Generic_Constant is
      function Value return Unbounded_String;
   end Generic_Constant;

private

   type Data is record -- "limited" prevents No_Elaboration_Code
      Reference_Count : aliased System.Reference_Counting.Counter;
      Max_Length : aliased Natural;
      Items : not null String_Access;
      --  the storage be allocated in here
   end record;
   pragma Suppress_Initialization (Data);
   pragma Compile_Time_Error (Data'Size rem Integer'Size > 0, "misaligned");

   type Data_Access is access all Data;

   Empty_String : aliased constant String_Type :=
      (1 .. 0 => Character_Type'Val (0));
   Empty_Data : aliased constant Data := (
      Reference_Count => System.Reference_Counting.Static,
      Max_Length => 0,
      Items => Empty_String'Unrestricted_Access);

   type Unbounded_String is new Finalization.Controlled with record
      Data : aliased not null Data_Access := Empty_Data'Unrestricted_Access;
      Length : aliased Natural := 0;
   end record;

   overriding procedure Adjust (Object : in out Unbounded_String);
   overriding procedure Finalize (Object : in out Unbounded_String);

   package Streaming is

      procedure Read (
         Stream : not null access Streams.Root_Stream_Type'Class;
         Item : out Unbounded_String);
      procedure Write (
         Stream : not null access Streams.Root_Stream_Type'Class;
         Item : Unbounded_String);

   end Streaming;

   for Unbounded_String'Read use Streaming.Read;
   for Unbounded_String'Write use Streaming.Write;

end Ada.Strings.Generic_Unbounded;
