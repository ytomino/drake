pragma License (Unrestricted);
--  generic implementation of Ada.Strings.Unbounded
with Ada.Unchecked_Deallocation;
with System.Arrays;
private with Ada.Finalization;
private with Interfaces;
generic
   type Character_Type is (<>);
   type String_Type is array (Positive range <>) of Character_Type;
package Ada.Strings.Generic_Unbounded is
   pragma Preelaborate;

   type Unbounded_String is private;
   pragma Preelaborable_Initialization (Unbounded_String);

--  Null_Unbounded_String : constant Unbounded_String;
   function Null_Unbounded_String return Unbounded_String; --  extended

   function Length (Source : Unbounded_String) return Natural;
   pragma Inline (Length);

   type String_Access is access all String_Type;
--  procedure Free (X : in out String_Access);
   procedure Free is new Unchecked_Deallocation (String_Type, String_Access);

   --  Conversion, Concatenation, and Selection functions

   function To_Unbounded_String (Source : String_Type)
      return Unbounded_String;

--  function To_Unbounded_String (Length : Natural)
--    return Unbounded_String;

   function To_String (Source : Unbounded_String) return String_Type;

--  procedure Set_Unbounded_String (
--    Target : out Unbounded_String;
--    Source : String);

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

--  function Element (Source : Unbounded_String; Index : Positive)
--    return Character;

--  procedure Replace_Element (
--    Source : in out Unbounded_String;
--    Index : Positive;
--    By : Character);

   function Slice (
      Source : Unbounded_String;
      Low : Positive;
      High : Natural)
      return String_Type;

--  function Unbounded_Slice (
--    Source : Unbounded_String;
--    Low : Positive;
--    High : Natural)
--    return Unbounded_String;

--  procedure Unbounded_Slice (
--    Source : Unbounded_String;
--    Target : out Unbounded_String;
--    Low : Positive;
--    High : Natural);

   function "=" (Left, Right : Unbounded_String) return Boolean;

   function "=" (Left : Unbounded_String; Right : String_Type) return Boolean;

   function "=" (Left : String_Type; Right : Unbounded_String) return Boolean;

   function "<" (Left, Right : Unbounded_String) return Boolean;

--  function "<" (Left : Unbounded_String; Right : String) return Boolean;

--  function "<" (Left : String; Right : Unbounded_String) return Boolean;

--  function "<=" (Left, Right : Unbounded_String) return Boolean;

--  function "<=" (Left : Unbounded_String; Right : String) return Boolean;

--  function "<=" (Left : String; Right : Unbounded_String) return Boolean;

   function ">" (Left, Right : Unbounded_String) return Boolean;

--  function ">" (Left : Unbounded_String; Right : String) return Boolean;

--  function ">" (Left : String; Right : Unbounded_String) return Boolean;

--  function ">=" (Left, Right : Unbounded_String) return Boolean;

--  function ">=" (Left : Unbounded_String; Right : String) return Boolean;

--  function ">=" (Left : String; Right : Unbounded_String) return Boolean;

   --  Search subprograms

--  function Index (
--    Source : Unbounded_String;
--    Pattern : String;
--    From : Positive;
--    Going : Direction := Forward;
--    Mapping : Maps.Character_Mapping := Maps.Identity)
--    return Natural;

--  function Index (
--    Source : Unbounded_String;
--    Pattern : String;
--    From : Positive;
--    Going : Direction := Forward;
--    Mapping : Maps.Character_Mapping_Function)
--    return Natural;

--  function Index (
--    Source : Unbounded_String;
--    Pattern : String;
--    Going : Direction := Forward;
--    Mapping : Maps.Character_Mapping := Maps.Identity)
--    return Natural;

--  function Index (
--    Source : Unbounded_String;
--    Pattern : String;
--    Going : Direction := Forward;
--    Mapping : Maps.Character_Mapping_Function)
--    return Natural;

--  function Index (
--    Source : Unbounded_String;
--    Set : Maps.Character_Set;
--    From : Positive;
--    Test : Membership := Inside;
--    Going : Direction := Forward)
--    return Natural;

--  function Index (
--    Source : Unbounded_String;
--    Set : Maps.Character_Set;
--    Test : Membership := Inside;
--    Going : Direction  := Forward)
--    return Natural;

--  function Index_Non_Blank (
--    Source : Unbounded_String;
--    From : Positive;
--    Going : Direction := Forward)
--    return Natural;

--  function Index_Non_Blank (
--    Source : Unbounded_String;
--    Going : Direction := Forward)
--    return Natural;

--  function Count (
--    Source : Unbounded_String;
--    Pattern : String;
--    Mapping : Maps.Character_Mapping := Maps.Identity)
--    return Natural;

--  function Count (
--    Source : Unbounded_String;
--    Pattern : String;
--    Mapping : Maps.Character_Mapping_Function)
--    return Natural;

--  function Count (
--    Source : Unbounded_String;
--    Set : Maps.Character_Set)
--    return Natural;

--  procedure Find_Token (
--    Source : Unbounded_String;
--    Set : Maps.Character_Set;
--    From : Positive;
--    Test : Membership;
--    First : out Positive;
--    Last : out Natural);

--  procedure Find_Token (
--    Source : Unbounded_String;
--    Set : Maps.Character_Set;
--    Test : Membership;
--    First : out Positive;
--    Last : out Natural);

   --  String translation subprograms

--  function Translate (
--    Source : Unbounded_String;
--    Mapping : Maps.Character_Mapping)
--    return Unbounded_String;

--  procedure Translate (
--    Source : in out Unbounded_String;
--    Mapping : Maps.Character_Mapping);

--  function Translate (
--    Source : Unbounded_String;
--    Mapping : Maps.Character_Mapping_Function)
--    return Unbounded_String;

--  procedure Translate (
--    Source : in out Unbounded_String;
--    Mapping : Maps.Character_Mapping_Function);

   --  String transformation subprograms

--  function Replace_Slice (
--    Source : Unbounded_String;
--    Low : Positive;
--    High : Natural;
--    By : String)
--    return Unbounded_String;

--  procedure Replace_Slice (
--    Source : in out Unbounded_String;
--    Low : Positive;
--    High : Natural;
--    By : String);

--  function Insert (
--    Source : Unbounded_String;
--    Before : Positive;
--    New_Item : String)
--    return Unbounded_String;

--  procedure Insert (
--    Source : in out Unbounded_String;
--    Before : Positive;
--    New_Item : String);

--  function Overwrite (
--    Source : Unbounded_String;
--    Position : Positive;
--    New_Item : String)
--    return Unbounded_String;

--  procedure Overwrite (
--    Source : in out Unbounded_String;
--    Position : Positive;
--    New_Item : String);

--  function Delete (
--    Source : Unbounded_String;
--    From : Positive;
--    Through : Natural)
--    return Unbounded_String;

--  procedure Delete (
--    Source : in out Unbounded_String;
--    From : Positive;
--    Through : Natural);

--  function Trim (
--    Source : Unbounded_String;
--    Side : Trim_End)
--    return Unbounded_String;

--  procedure Trim (
--    Source : in out Unbounded_String;
--    Side : Trim_End);

--  function Trim (
--    Source : Unbounded_String;
--    Left : Maps.Character_Set;
--    Right : Maps.Character_Set)
--    return Unbounded_String;

--  procedure Trim (
--    Source : in out Unbounded_String;
--    Left : Maps.Character_Set;
--    Right : Maps.Character_Set);

--  function Head (
--    Source : Unbounded_String;
--    Count : Natural;
--    Pad : Character := Space)
--    return Unbounded_String;

--  procedure Head (
--    Source : in out Unbounded_String;
--    Count : Natural;
--    Pad : Character := Space);

--  function Tail (
--    Source : Unbounded_String;
--    Count : Natural;
--    Pad : Character := Space)
--    return Unbounded_String;

--  procedure Tail (
--    Source : in out Unbounded_String;
--    Count : Natural;
--    Pad : Character := Space);

--  function "*" (
--    Left : Natural;
--    Right : Character)
--    return Unbounded_String;

--  function "*" (Left : Natural; Right : String) return Unbounded_String;

--  function "*" (Left : Natural; Right : Unbounded_String)
--    return Unbounded_String;

   --  extended
   package Slicing is new System.Arrays.Generic_Slicing (
      Positive,
      Character_Type,
      String_Type);
   function Reference (Source : not null access Unbounded_String)
      return Slicing.Reference_Type;
   function Reference (
      Source : not null access Unbounded_String;
      First_Index : Positive;
      Last_Index : Natural)
      return Slicing.Reference_Type;

private

   type Data (Capacity : Natural) is limited record
      Reference_Count : aliased Interfaces.Integer_32;
      Max_Length : aliased Interfaces.Integer_32;
      Items : aliased String_Type (1 .. Capacity);
   end record;
   pragma Suppress_Initialization (Data);

   type Data_Access is access Data;

   Empty_Data : aliased constant Data := (
      Capacity => 0,
      Reference_Count => Interfaces.Integer_32'Last,
      Max_Length => 0,
      Items => <>);

   type Unbounded_String is new Finalization.Controlled with record
      Data : not null Data_Access := Empty_Data'Unrestricted_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Object : in out Unbounded_String);
   overriding procedure Finalize (Object : in out Unbounded_String);

end Ada.Strings.Generic_Unbounded;
