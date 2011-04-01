pragma License (Unrestricted);
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
   type Constant_Reference_Type (
      Name : not null access constant String;
      Value : not null access constant String) is limited private;
   type Iterator is limited private;
   type Cursor is private;
   pragma Preelaborable_Initialization (Cursor);
   No_Element : constant Cursor;
   function Iterate return Iterator;
   function First (Object : Iterator) return Cursor;
   function Next (Object : Iterator; Position : Cursor) return Cursor;
   function Constant_Reference (Position : Cursor)
      return Constant_Reference_Type;

private

   type Constant_Reference_Type (
      Name : not null access constant String;
      Value : not null access constant String) is limited
   record
      Name_First : Positive;
      Name_Last : Natural;
      Value_First : Positive;
      Value_Last : Natural;
   end record;

   type Iterator is null record;
   pragma Suppress_Initialization (Iterator);
   type Cursor is new C.char_ptr_ptr;
   No_Element : constant Cursor := null;

end Ada.Environment_Variables;
