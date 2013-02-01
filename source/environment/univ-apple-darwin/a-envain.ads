pragma License (Unrestricted);
--  implementation unit
package Ada.Environment_Variables.Inside is
   pragma Preelaborate;

   type Character_Access is access all Character;

   function Reference (Name : String) return Character_Access;

   procedure Set (Name : String; Value : String; Error : out Boolean);
   procedure Clear (Name : String; Error : out Boolean);

   function Reference (Position : Cursor) return Character_Access;

   function First return Cursor;
   function "+" (Left : Cursor; Right : Integer) return Cursor;

end Ada.Environment_Variables.Inside;
