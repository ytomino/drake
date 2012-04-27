pragma License (Unrestricted);
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
   type Iterator is limited private;
   function Iterate return Iterator;
   function First (Object : Iterator) return Cursor;
   function Next (Object : Iterator; Position : Cursor) return Cursor;

private

   type Iterator is null record;
   pragma Suppress_Initialization (Iterator);
   type Cursor is new System.Address; -- C.char_ptr_ptr; -- [gcc-4.7] ???

end Ada.Environment_Variables;
