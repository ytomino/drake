pragma License (Unrestricted);
--  extended unit
private with Ada.Command_Line.Argument_Parsing;
private with Ada.Finalization;
private with Ada.Streams;
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
      '?'); -- with value by no space (e.g. -M50% --prefix=/usr/local)

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

   type Non_Controlled_Cursor is record
      Argument_Context : aliased Argument_Context_Access;
      Subindex : aliased Argument_Parsing.Cursor;
   end record;
   pragma Suppress_Initialization (Non_Controlled_Cursor);

   package Controlled is

      type Cursor is private;

      function Create (
         Argument_Context : Argument_Context_Access;
         Subindex : Argument_Parsing.Cursor)
         return Cursor;

      function Reference (Position : Generic_Parsing.Cursor)
         return not null access Non_Controlled_Cursor;
      pragma Inline (Reference);

   private

      type Cursor is new Finalization.Controlled with record
         Data : aliased Non_Controlled_Cursor := (
            Argument_Context => null,
            Subindex => Argument_Parsing.No_Element);
      end record;

      overriding procedure Adjust (Object : in out Cursor);
      overriding procedure Finalize (Object : in out Cursor);

      package Streaming is

         procedure Missing_Read (
            Stream : access Streams.Root_Stream_Type'Class;
            Item : out Cursor)
            with Import,
               Convention => Ada, External_Name => "__drake_program_error";
         procedure Missing_Write (
            Stream : access Streams.Root_Stream_Type'Class;
            Item : Cursor)
            with Import,
               Convention => Ada, External_Name => "__drake_program_error";
         function Missing_Input (
            Stream : access Streams.Root_Stream_Type'Class)
            return Cursor
            with Import,
               Convention => Ada, External_Name => "__drake_program_error";

         pragma No_Return (Missing_Read);
         pragma No_Return (Missing_Write);
         pragma Machine_Attribute (Missing_Input, "noreturn");

      end Streaming;

      for Cursor'Read use Streaming.Missing_Read;
      for Cursor'Write use Streaming.Missing_Write;
      for Cursor'Input use Streaming.Missing_Input;

   end Controlled;

   type Cursor is new Controlled.Cursor;

   type Iterator is new Iterator_Interfaces.Forward_Iterator with record
      State : Argument_Parsing.State_Type;
   end record;

   overriding function First (Object : Iterator) return Cursor;
   overriding function Next (Object : Iterator; Position : Cursor)
      return Cursor;

end Ada.Command_Line.Generic_Parsing;
