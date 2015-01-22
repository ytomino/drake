pragma License (Unrestricted);
--  extended unit
with Ada.Finalization;
with Ada.Iterator_Interfaces;
private with Ada.Command_Line.Argument_Parsing;
generic
   type Input_Cursor is private;
   with function Has_Element (Position : Input_Cursor) return Boolean is <>;
   with package Input_Iterator_Interfaces is
      new Ada.Iterator_Interfaces (Input_Cursor, Has_Element);
   Input_Iterator : Input_Iterator_Interfaces.Forward_Iterator'Class;
   with function Argument (Position : Input_Cursor) return String is <>;
package Ada.Command_Line.Generic_Parsing is
   --  Command line parser for any lists.
   pragma Preelaborate;

   type Cursor is private;

   function Has_Element (Position : Cursor) return Boolean;

   package Iterator_Interfaces is
      new Ada.Iterator_Interfaces (Cursor, Has_Element);

   function Iterate (Posixly_Correct : Boolean := False)
      return Iterator_Interfaces.Forward_Iterator'Class;

   function Argument (Position : Cursor) return String;

   type Option_Character is (
      ' ', -- without value
      ':', -- with value by no space or space
      '?'); -- with value by no space (ex. -M50% --prefix=/usr/local)

   function Is_Option (
      Position : Cursor;
      Name : Character;
      Option : Option_Character := ' ')
      return Boolean;
   function Is_Option (
      Position : Cursor;
      Long_Name : String;
      Option : Option_Character := ' ')
      return Boolean;
   function Is_Option (
      Position : Cursor;
      Name : Character;
      Long_Name : String;
      Option : Option_Character := ' ')
      return Boolean;
   function Is_Unknown_Option (Position : Cursor) return Boolean;

   function Name (Position : Cursor) return String;
   function Short_Name (Position : Cursor) return Character;
   function Long_Name (Position : Cursor) return String;

   function Value (Position : Cursor) return String;

private

   type String_Access is access String;

   type Argument_Iterator_Access is
      access Argument_Parsing.Argument_Iterator; -- Forward_Iterator'Class

   type Argument_Context is record
      Reference_Count : Natural;
      Index : Input_Cursor;
      Next_Index : Input_Cursor;
      Argument : String_Access;
      Argument_Iterator : Argument_Iterator_Access;
   end record;
   pragma Suppress_Initialization (Argument_Context);

   type Argument_Context_Access is access Argument_Context;

   package Cursors is

      type Cursor is private;

      function Create (
         Argument_Context : Argument_Context_Access;
         Subindex : Argument_Parsing.Cursor)
         return Cursor;

      function Get_Argument_Context (Position : Cursor)
         return Argument_Context_Access;
      function Get_Subindex (Position : Cursor)
         return not null access Argument_Parsing.Cursor;

   private

      type Cursor is new Finalization.Controlled with record
         Argument_Context : aliased Argument_Context_Access;
         Subindex : aliased Argument_Parsing.Cursor;
      end record;

      overriding procedure Adjust (Object : in out Cursor);
      overriding procedure Finalize (Object : in out Cursor);

   end Cursors;

   type Cursor is new Cursors.Cursor;

   type Iterator is new Iterator_Interfaces.Forward_Iterator with record
      State : Argument_Parsing.State_Type;
   end record;

   overriding function First (Object : Iterator) return Cursor;
   overriding function Next (Object : Iterator; Position : Cursor)
      return Cursor;

end Ada.Command_Line.Generic_Parsing;
