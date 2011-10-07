with Ada.Unchecked_Conversion;
package body Ada.Containers.Counted_Access_Holders_Derivational_Conversions is

   procedure Assign (
      Target : in out Base_Holders.Holder;
      Source : Derived_Holders.Holder)
   is
      type Base_Holder_Access is access all Base_Holders.Holder;
      type Derived_Holder_Access is access all Derived_Holders.Holder;
      function Cast is new Ada.Unchecked_Conversion (
         Derived_Holder_Access,
         Base_Holder_Access);
   begin
      Base_Holders.Assign (Target, Cast (Source'Unrestricted_Access).all);
   end Assign;

   procedure Move (
      Target : in out Base_Holders.Holder;
      Source : in out Derived_Holders.Holder)
   is
      type Base_Holder_Access is access all Base_Holders.Holder;
      type Derived_Holder_Access is access all Derived_Holders.Holder;
      function Cast is new Ada.Unchecked_Conversion (
         Derived_Holder_Access,
         Base_Holder_Access);
   begin
      Base_Holders.Move (Target, Cast (Source'Unrestricted_Access).all);
   end Move;

end Ada.Containers.Counted_Access_Holders_Derivational_Conversions;
