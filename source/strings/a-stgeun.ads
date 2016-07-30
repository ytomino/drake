pragma License (Unrestricted);
--  generalized unit of Ada.Strings.Unbounded
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

   --  modified
   --  Unbounded_String is declared as tagged for dot notation.
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
   procedure Reserve_Capacity (
      Item : in out Unbounded_String;
      Capacity : Natural);

   pragma Inline (Capacity);

   type String_Access is access all String_Type;
--  procedure Free (X : in out String_Access);
   procedure Free is new Unchecked_Deallocation (String_Type, String_Access);

   --  extended
   type String_Constant_Access is access constant String_Type;

   --  Conversion, Concatenation, and Selection functions

   function To_Unbounded_String (Source : String_Type)
      return Unbounded_String;
   --  extended
   --  For shorthand.
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

   procedure Append_Element (
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
   --  Efficient copying.
   procedure Assign (
      Target : in out Unbounded_String;
      Source : Unbounded_String);
   procedure Move (
      Target : in out Unbounded_String;
      Source : in out Unbounded_String);

   --  extended
   --  These functions provides a convenient way to directly access.
   function Constant_Reference (
      Source : aliased Unbounded_String)
      return Slicing.Constant_Reference_Type;
   function Reference (
      Source : aliased in out Unbounded_String)
      return Slicing.Reference_Type;

   --  for making constant Unbounded_String without dynamic allocation
   generic
      S : not null String_Constant_Access;
   package Generic_Constant is
      function Value return Unbounded_String;
   end Generic_Constant;

private

   subtype Fixed_String is String_Type (Positive);

   type Fixed_String_Access is access all Fixed_String;

   type Data is record -- "limited" prevents No_Elaboration_Code
      Reference_Count : aliased System.Reference_Counting.Counter;
      Capacity : Natural;
      Max_Length : aliased System.Reference_Counting.Length_Type;
      Items : not null Fixed_String_Access;
      --  the storage would be allocated in here
   end record;
   pragma Suppress_Initialization (Data);
   pragma Compile_Time_Error (Data'Size rem Wide_Wide_Character'Size > 0,
      "misaligned");

   type Data_Access is access all Data;

   Empty_String : aliased constant String_Type (1 .. 0) :=
      (others => Character_Type'Val (0));
   Empty_Data : aliased constant Data := (
      Reference_Count => System.Reference_Counting.Static,
      Capacity => 0,
      Max_Length => 0,
      Items => Fixed_String'Deref (Empty_String'Address)'Unrestricted_Access);

   type Unbounded_String is new Finalization.Controlled with record
      Data : aliased not null Data_Access := Empty_Data'Unrestricted_Access;
      Length : aliased Natural := 0;
   end record;

   procedure Unique (Item : in out Unbounded_String'Class);

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
