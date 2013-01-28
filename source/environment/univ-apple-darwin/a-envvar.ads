pragma License (Unrestricted);
--  with Ada.Iterator_Interfaces; -- [gcc 4.6] can not instantiate it
with Ada.References.String;
private with System;
package Ada.Environment_Variables is
   pragma Preelaborate;

   function Value (Name : String) return String;

   function Exists (Name : String) return Boolean;

   procedure Set (Name : String; Value : String);

   procedure Clear (Name : String);
   procedure Clear;

   procedure Iterate (
      Process : not null access procedure (Name, Value : String));

   --  extended
   --  There is an iterator for AI12-0009-1 (?)
   type Cursor is private;
   pragma Preelaborable_Initialization (Cursor);
   function Has_Element (Position : Cursor) return Boolean;
   function Name (Position : Cursor)
      return References.String.Slicing.Constant_Reference_Type;
   function Value (Position : Cursor)
      return References.String.Slicing.Constant_Reference_Type;
   package Iterator_Interfaces is
--    new Ada.Iterator_Interfaces (Cursor, Has_Element);
      --  [gcc 4.6] Cursor is incomplete type
      type Forward_Iterator is limited interface;
      function First (Object : Forward_Iterator) return Cursor is abstract;
      function Next (Object : Forward_Iterator; Position : Cursor)
         return Cursor is abstract;
   end Iterator_Interfaces;
   type Iterator is new Iterator_Interfaces.Forward_Iterator
      with private;
   function Iterate return Iterator;

private

   type Cursor is new System.Address; -- C.char_ptr_ptr; -- [gcc-4.7] ???

   type Iterator is new Iterator_Interfaces.Forward_Iterator
      with null record;

   overriding function First (Object : Iterator) return Cursor;
   overriding function Next (Object : Iterator; Position : Cursor)
      return Cursor;

end Ada.Environment_Variables;
