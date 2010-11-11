pragma License (Unrestricted);
package Ada.Environment_Variables is
   pragma Preelaborate;

   function Value (Name : String) return String;

   function Exists (Name : String) return Boolean;

   procedure Set (Name : String; Value : String);

   procedure Clear (Name : String);
   procedure Clear;

   procedure Iterate (
      Process : not null access procedure (Name, Value : String));

end Ada.Environment_Variables;
