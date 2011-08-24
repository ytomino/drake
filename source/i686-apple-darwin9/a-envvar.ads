pragma License (Unrestricted);
with System.Arrays;
private with C;
package Ada.Environment_Variables is
   pragma Preelaborate;

   function Value (Name : String) return String;

   function Exists (Name : String) return Boolean;

   procedure Set (Name : String; Value : String);

   procedure Clear (Name : String);
   procedure Clear;

   procedure Iterate (
      Process : not null access procedure (Name, Value : String));

   --  extended, iterator like AI05-0139-2 style
   package Slicing is new System.Arrays.Generic_Slicing (
      Positive,
      Character,
      String);
   type Iterator is limited private;
   type Cursor is private;
   pragma Preelaborable_Initialization (Cursor);
   function Has_Element (Position : Cursor) return Boolean;
   function Iterate return Iterator;
   function First (Object : Iterator) return Cursor;
   function Next (Object : Iterator; Position : Cursor) return Cursor;
   function Name (Position : Cursor) return Slicing.Constant_Reference_Type;
   function Value (Position : Cursor) return Slicing.Constant_Reference_Type;

private

   type Iterator is null record;
   pragma Suppress_Initialization (Iterator);
   type Cursor is new C.char_ptr_ptr;

end Ada.Environment_Variables;
