pragma License (Unrestricted);
--  extended unit
with Ada.IO_Exceptions;
with Ada.Iterator_Interfaces;
private with Ada.Finalization;
private with Ada.Streams;
generic
   type Input_Cursor is private;
   with function Has_Element (Position : Input_Cursor) return Boolean is <>;
   with package Input_Iterator_Interfaces is
      new Iterator_Interfaces (Input_Cursor, Has_Element);
package Ada.Containers.Forward_Iterators is
   --  This package makes a normal forward iterator
   --    from a non-rollbackable forward iterator (input iterator).
   pragma Preelaborate;

   type Cursor is private;
   pragma Preelaborable_Initialization (Cursor);

   function Has_Element (Position : Cursor) return Boolean;

   type Input_Cursor_Ref (Element : not null access constant Input_Cursor) is
      null record
      with Implicit_Dereference => Element;

   function To (Position : Cursor) return Input_Cursor_Ref;

   package Iterator_Interfaces is
      new Ada.Iterator_Interfaces (Cursor, Has_Element);

   function Satisfy (
      Iterator : aliased in out
         Input_Iterator_Interfaces.Forward_Iterator'Class)
      return Iterator_Interfaces.Forward_Iterator'Class;

   Status_Error : exception
      renames IO_Exceptions.Status_Error;

private

   type Input_Iterator_Access is
      access all Input_Iterator_Interfaces.Forward_Iterator'Class;
   for Input_Iterator_Access'Storage_Size use 0;

   type Iterator;
   type Iterator_Access is access all Iterator;
   for Iterator_Access'Storage_Size use 0;

   type Node;
   type Node_Access is access Node;
   type Node is limited record
      Reference_Count : Integer;
      Next : Node_Access;
      Original : aliased Input_Cursor;
   end record;

   package Cursors is

      type Cursor is private;

      function Reference (Position : Cursor) return Node_Access;
      pragma Inline (Reference);

      procedure Assign (Position : in out Cursor; Node : Node_Access);
      pragma Inline (Assign);

   private

      type Cursor is new Finalization.Controlled with record
         Node : Node_Access;
      end record;

      overriding procedure Adjust (Object : in out Cursor);
      overriding procedure Finalize (Object : in out Cursor);

      package Streaming is

         procedure Missing_Read (
            Stream : access Streams.Root_Stream_Type'Class;
            Item : out Cursor);
         function Missing_Input (
            Stream : not null access Streams.Root_Stream_Type'Class)
            return Cursor;
         procedure Missing_Write (
            Stream : access Streams.Root_Stream_Type'Class;
            Item : Cursor);

         pragma Import (Ada, Missing_Read, "__drake_program_error");
         pragma Import (Ada, Missing_Input, "__drake_program_error");
         pragma Import (Ada, Missing_Write, "__drake_program_error");

      end Streaming;

      for Cursor'Read use Streaming.Missing_Read;
      for Cursor'Input use Streaming.Missing_Input;
      for Cursor'Write use Streaming.Missing_Write;
      for Cursor'Output use Streaming.Missing_Write;

   end Cursors;

   type Cursor is new Cursors.Cursor;

   type State_Type is (First, Next, No_Element);
   pragma Discard_Names (State_Type);

   type Iterator is
      new Finalization.Limited_Controlled
      and Iterator_Interfaces.Forward_Iterator with
   record
      Input_Iterator : not null Input_Iterator_Access;
      Last : Node_Access;
      State : State_Type;
   end record;

   overriding procedure Finalize (Object : in out Iterator);

   overriding function First (Object : Iterator) return Cursor;
   overriding function Next (Object : Iterator; Position : Cursor)
      return Cursor;

end Ada.Containers.Forward_Iterators;
