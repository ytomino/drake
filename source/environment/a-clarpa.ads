pragma License (Unrestricted);
--  implementation unit
private package Ada.Command_Line.Argument_Parsing is
   --  Command line parser only for one argument.
   pragma Preelaborate;

   type State_Type is record
      Posixly_Correct : Boolean := False; -- stop parsing after argument
      Not_Option : Boolean := False; -- after "--"
   end record;
   pragma Pack (State_Type);

   type Cursor is private;

   No_Element : constant Cursor;

   function Has_Element (Position : Cursor) return Boolean;

   type Argument_Iterator is limited private; -- new Forward_Iterator with

   function Iterate (Argument : String; Initial_State : State_Type)
      return Argument_Iterator; -- Forward_Iterator'Class

   function State (Iterator : Argument_Iterator) return State_Type;

   type Option_Character is (
      ' ', -- without value
      ':', -- with value by no space or space
      '?'); -- with value by no space (ex. -M50% --prefix=/usr/local)

   function Is_Option (
      Argument : String;
      Position : in out Cursor;
      Short_Name : Character;
      Option : Option_Character := ' ')
      return Boolean;
   function Is_Option (
      Argument : String;
      Position : in out Cursor;
      Long_Name : String;
      Option : Option_Character := ' ')
      return Boolean;
   function Is_Option (
      Argument : String;
      Position : in out Cursor;
      Short_Name : Character;
      Long_Name : String;
      Option : Option_Character := ' ')
      return Boolean;
   function Is_Unknown_Option (
      Argument : String;
      Position : in out Cursor)
      return Boolean;

   function Name (Argument : String; Position : Cursor) return String;
   function Short_Name (Argument : String; Position : Cursor)
      return Character;
   function Long_Name (Argument : String; Position : Cursor) return String;

   type Value_Location is (None, Same, Next);
   pragma Discard_Names (Value_Location);

   function Has_Value (Argument : String; Position : Cursor)
      return Value_Location;
   function Value (Argument : String; Position : Cursor) return String;

   --  package Argument_Iterator_Interfaces
   function First (Object : Argument_Iterator) return Cursor;
   function Next (Object : Argument_Iterator; Position : Cursor)
      return Cursor;

private

   type Argument_Kind is (
      Not_Option,
      Short_Option,
      Long_Option,
      Double_Hyphen);
   pragma Discard_Names (Argument_Kind);

   type Cursor is record
      State : State_Type;
      Kind : Argument_Kind;
      Has_Value : Boolean;
      Index : Natural;
      Option_Index : Natural;
   end record;
   for Cursor'Size use 3 * Integer'Size;
   for Cursor use record
      State at 0 range 0 .. 1;
      Kind at 0 range 2 .. 3;
      Has_Value at 0 range 4 .. 4;
      Index at 0 range Integer'Size .. 2 * Integer'Size - 1;
      Option_Index at 0 range 2 * Integer'Size .. 3 * Integer'Size - 1;
   end record;
   pragma Suppress_Initialization (Cursor);

   No_Element : constant Cursor := (
      State => (Posixly_Correct => False, Not_Option => False),
      Kind => Not_Option,
      Has_Value => False,
      Index => 0,
      Option_Index => 0);

   type Argument_Iterator is
      limited -- new Forward_Iterator with
   record
      First : Cursor;
   end record;
   pragma Suppress_Initialization (Argument_Iterator);

end Ada.Command_Line.Argument_Parsing;
