pragma License (Unrestricted);
--  extended unit
with Ada.Containers.Access_Holders;
generic
   type Base (<>) is abstract tagged limited private;
   type Base_Name is access all Base'Class;
   with package Base_Holders is
      new Access_Holders (Base_Name, Free => <>);
   type Derived (<>) is abstract limited new Base with private;
   type Derived_Name is access all Derived'Class;
   with package Derived_Holders is
      new Access_Holders (Derived_Name, Free => <>);
package Ada.Containers.Access_Holders_Derivational_Conversions is
   --  It converts reference counted access types from derived to base.
   pragma Preelaborate;

   procedure Assign (
      Target : in out Base_Holders.Holder;
      Source : Derived_Holders.Holder);
   procedure Move (
      Target : in out Base_Holders.Holder;
      Source : in out Derived_Holders.Holder);

end Ada.Containers.Access_Holders_Derivational_Conversions;
