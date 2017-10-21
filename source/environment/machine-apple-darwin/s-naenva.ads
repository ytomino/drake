pragma License (Unrestricted);
--  implementation unit specialized for POSIX (Darwin, FreeBSD, or Linux)
package System.Native_Environment_Variables is
   pragma Preelaborate;

   subtype Cursor is Address;

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

   function Get_Block return Address is (Null_Address);
   procedure Release_Block (Block : Address) is null;

   function First (Block : Address) return Cursor;
   function Next (Block : Address; Position : Cursor) return Cursor;

   pragma Inline (First);
   pragma Inline (Next);

end System.Native_Environment_Variables;
