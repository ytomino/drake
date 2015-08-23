pragma License (Unrestricted);
--  implementation unit
with System;
package Ada.Environment_Variables.Inside is
   pragma Preelaborate;

   function Value (Name : String) return String;
   function Value (Name : String; Default : String) return String;
   function Exists (Name : String) return Boolean;

   procedure Set (Name : String; Value : String);
   procedure Clear (Name : String);

   procedure Clear;

   function Has_Element (Position : Cursor) return Boolean;
   pragma Inline (Has_Element);

   function Name (Position : Cursor) return String;
   function Value (Position : Cursor) return String;

   function Get_Block return System.Address;
   procedure Release_Block (Block : System.Address);

   function First (Block : System.Address) return Cursor;
   function Next (Block : System.Address; Position : Cursor) return Cursor;

   pragma Inline (First);
   pragma Inline (Next);

end Ada.Environment_Variables.Inside;
