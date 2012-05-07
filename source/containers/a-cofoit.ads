pragma License (Unrestricted);
--  extended unit
with Ada.IO_Exceptions;
--  with Ada.Iterator_Interfaces; -- [gcc 4.6] can not instantiate it
private with Ada.Finalization;
generic
   type Input_Cursor is private;
   with function Has_Element (Position : Input_Cursor) return Boolean is <>;
--  with package Input_Iterator_Interfaces is
--    new Iterator_Interfaces (Input_Cursor, Has_Element);
   --  [gcc 4.6] can not instantiate Ada.Iterator_Interfaces
   type Input_Iterator is limited interface;
   with function First (Object : Input_Iterator)
      return Input_Cursor is abstract <>;
   with function Next (
      Object : Input_Iterator;
      Position : Input_Cursor)
      return Input_Cursor is abstract <>;
package Ada.Containers.Forward_Iterators is
   --  This package makes a normal forward iterator
   --    from a non-rollbackable forward iterator (input iterator).
   pragma Preelaborate;

   package Input_Iterator_Interfaces is -- [gcc 4.6] renaming for compile
      subtype Forward_Iterator is Input_Iterator;
   end Input_Iterator_Interfaces;

   type Cursor is private;
   pragma Preelaborable_Initialization (Cursor);

   function Has_Element (Position : Cursor) return Boolean;

   type Input_Cursor_Ref (Element : not null access constant Input_Cursor) is
      null record;
--    with Implicit_Dereference => Element; -- [gcc 4.6]

   function To (Position : Cursor) return Input_Cursor_Ref;

   package Iterator_Interfaces is
--    new Ada.Iterator_Interfaces (Cursor, Has_Element);
      --  [gcc 4.6] Cursor is incomplete type
      type Forward_Iterator is limited interface;
      function First (Object : Forward_Iterator) return Cursor is abstract;
      function Next (Object : Forward_Iterator; Position : Cursor)
         return Cursor is abstract;
   end Iterator_Interfaces;

   function Satisfy (
      Iterator : -- [gcc 4.6] pass by reference because tagged
         Input_Iterator_Interfaces.Forward_Iterator'Class)
      return Iterator_Interfaces.Forward_Iterator'Class;

   Status_Error : exception
      renames IO_Exceptions.Status_Error;

private

   type Input_Iterator_Access is
      access constant Input_Iterator_Interfaces.Forward_Iterator'Class;
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
