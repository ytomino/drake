pragma License (Unrestricted);
--  with Ada.Iterator_Interfaces; -- [gcc 4.6] can not instantiate it
private with Ada.Finalization;
private with System;
package Ada.Environment_Variables is
   pragma Preelaborate;

   function Value (Name : String) return String;
   function Value (Name : String; Default : String) return String;
   pragma Inline (Value);

   function Exists (Name : String) return Boolean;
   pragma Inline (Exists);

   procedure Set (Name : String; Value : String);
   pragma Inline (Set);

   procedure Clear (Name : String);
   procedure Clear;
   pragma Inline (Clear);

   procedure Iterate (
      Process : not null access procedure (Name, Value : String));

   --  extended
   --  There is an iterator for AI12-0009-1 (?)
   type Cursor is private;
   pragma Preelaborable_Initialization (Cursor);
   function Has_Element (Position : Cursor) return Boolean;
   pragma Inline (Has_Element);
   function Name (Position : Cursor) return String;
   pragma Inline (Name);
   function Value (Position : Cursor) return String;
   pragma Inline (Value);
   package Iterator_Interfaces is
--    new Ada.Iterator_Interfaces (Cursor, Has_Element);
      --  [gcc 4.6] Cursor is incomplete type
      type Forward_Iterator is limited interface;
      function First (Object : Forward_Iterator) return Cursor is abstract;
      function Next (Object : Forward_Iterator; Position : Cursor)
         return Cursor is abstract;
   end Iterator_Interfaces;
   function Iterate return Iterator_Interfaces.Forward_Iterator'Class;

private

   type Cursor is new System.Address;

   type Iterator is new Finalization.Limited_Controlled
      and Iterator_Interfaces.Forward_Iterator with
   record
      Block : System.Address := System.Null_Address;
   end record;

   overriding procedure Finalize (Object : in out Iterator);

   overriding function First (Object : Iterator) return Cursor;
   overriding function Next (Object : Iterator; Position : Cursor)
      return Cursor;

end Ada.Environment_Variables;
