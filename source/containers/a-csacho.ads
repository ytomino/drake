pragma License (Unrestricted);
--  extended unit
private with Ada.Finalization;
generic
   type Name is private; -- it must have default value
   with procedure Free (X : in out Name) is <>;
package Ada.Containers.Scoped_Access_Holders is
   --  Access types with scope guard.
   pragma Preelaborate;

   type Holder is tagged limited private;

   function To_Holder (Source : Name) return Holder;
   function "+" (Right : Name) return Holder
      renames To_Holder;

   function Null_Holder return Holder;

   function Is_Null (Container : Holder) return Boolean;

   procedure Clear (Container : in out Holder);

   function Constant_Reference (Container : Holder) return Name;
   function Element (Container : Holder'Class) return Name;

   procedure Replace_Element (
      Target : in out Holder;
      Source : Name);

   procedure Move (
      Target : in out Holder;
      Source : in out Holder);

   procedure Swap (I, J : in out Holder);

   procedure Release (Container : in out Holder; Item : out Name);
   function Release (Container : not null access Holder) return Name;

private

   type Holder is new Finalization.Limited_Controlled with record
      Item : aliased Name;
   end record;

   overriding procedure Finalize (Object : in out Holder);

end Ada.Containers.Scoped_Access_Holders;
