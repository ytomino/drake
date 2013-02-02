pragma License (Unrestricted);
with Ada.Iterator_Interfaces;
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
   function Name (Position : Cursor) return String;
   function Value (Position : Cursor) return String;
   package Iterator_Interfaces is
      new Ada.Iterator_Interfaces (Cursor, Has_Element);
   type Iterator is new Iterator_Interfaces.Forward_Iterator
      with private;
   function Iterate return Iterator;

private

   type Cursor is new System.Address;

   type Iterator is new Iterator_Interfaces.Forward_Iterator
      with null record;

   overriding function First (Object : Iterator) return Cursor;
   overriding function Next (Object : Iterator; Position : Cursor)
      return Cursor;

end Ada.Environment_Variables;
