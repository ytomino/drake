pragma License (Unrestricted);
--  extended unit
with Ada.Iterator_Interfaces;
private with System;
package Ada.Bind_Time_Variables is
   --  This is similar to Ada.Environment_Variables,
   --    but reads bind-time variables set by gnatbind -Vkey=val.
   pragma Preelaborate;

   function Value (Name : String) return String;
   function Value (Name : String; Default : String) return String;

   function Exists (Name : String) return Boolean;

   procedure Iterate (
      Process : not null access procedure (Name, Value : String));

   --  Iterators:

   type Cursor is private;
   pragma Preelaborable_Initialization (Cursor);

   function Has_Element (Position : Cursor) return Boolean;
   pragma Inline (Has_Element);

   function Name (Position : Cursor) return String;
   function Value (Position : Cursor) return String;

   package Iterator_Interfaces is
      new Ada.Iterator_Interfaces (Cursor, Has_Element);

   function Iterate return Iterator_Interfaces.Forward_Iterator'Class;

private

   type Cursor is new System.Address;

   type Iterator is new Iterator_Interfaces.Forward_Iterator with null record;

   overriding function First (Object : Iterator) return Cursor;
   overriding function Next (Object : Iterator; Position : Cursor)
      return Cursor;

end Ada.Bind_Time_Variables;
