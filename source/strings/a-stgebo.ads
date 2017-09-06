pragma License (Unrestricted);
--  generalized unit of Ada.Strings.Bounded
with Ada.References;
with Ada.Streams;
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

   procedure Append_Element (
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

   overriding function "=" (Left, Right : Bounded_String) return Boolean;
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
   --  These functions provides a convenient way to directly access.
   function Constant_Reference (
      Source : aliased Bounded_String)
      return Slicing.Constant_Reference_Type;
   function Reference (
      Source : aliased in out Bounded_String)
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

      --  extended
      --  For shorthand.
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

      function Append_Element (
         Left : Bounded_String;
         Right : Character_Type;
         Drop : Truncation := Error)
         return Bounded_String;

      function Append_Element (
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

--    procedure Append_Element (
--       Source : in out Bounded_String;
--       New_Item : Character_Type;
--       Drop : Truncation := Error);
         --  procedure Append_Element is inherited

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

      procedure Bounded_Slice (
         Source : Bounded_String;
         Target : out Bounded_String;
         Low : Positive;
         High : Natural);

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
         --  "=", "<", "<=", ">", and ">=" are inherited

      --  extended
      --  Efficient copying.
      procedure Assign (
         Target : in out Bounded_String;
         Source : Bounded_String);
      procedure Move (
         Target : in out Bounded_String;
         Source : in out Bounded_String);

      --  String constructor subprograms

      function "*" (Left : Natural; Right : Character_Type)
         return Bounded_String;
         --  function "*" should be primitive for CXA4007

      function "*" (Left : Natural; Right : String_Type)
         return Bounded_String;
         --  function "*" should be primitive for CXA4007

      function "*" (Left : Natural; Right : Bounded_String)
         return Bounded_String;

      function Replicate_Element (
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

      package Streaming is

         procedure Read (
            Stream : not null access Streams.Root_Stream_Type'Class;
            Item : out Bounded_String);
         procedure Write (
            Stream : not null access Streams.Root_Stream_Type'Class;
            Item : Bounded_String);
         function Input (
            Stream : not null access Streams.Root_Stream_Type'Class)
            return Bounded_String;

      end Streaming;

      for Bounded_String'Read use Streaming.Read;
      for Bounded_String'Write use Streaming.Write;
      for Bounded_String'Input use Streaming.Input;
      for Bounded_String'Output use Streaming.Write;

   end Generic_Bounded_Length;

end Ada.Strings.Generic_Bounded;
