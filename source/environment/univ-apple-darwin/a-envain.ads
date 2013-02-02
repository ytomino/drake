pragma License (Unrestricted);
--  implementation unit
package Ada.Environment_Variables.Inside is
   pragma Preelaborate;

   function Value (Name : String) return String;
   function Exists (Name : String) return Boolean;

   procedure Set (Name : String; Value : String; Error : out Boolean);
   procedure Clear (Name : String; Error : out Boolean);

   function Has_Element (Position : Cursor) return Boolean;
   pragma Inline (Has_Element);
   function Name (Position : Cursor) return String;
   function Value (Position : Cursor) return String;

   function First return Cursor;
   function "+" (Left : Cursor; Right : Integer) return Cursor;

end Ada.Environment_Variables.Inside;
