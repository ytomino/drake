pragma License (Unrestricted);
--  extended unit
private with Ada.Finalization;
generic
   type Name is private; -- it must have default value
   with procedure Retain (X : Name) is <>;
   with procedure Release (X : in out Name) is <>;
package Ada.Containers.Intrusive_Counted_Access_Holders is
   --  Reference counted access types, using the counter a part of the target.
   pragma Preelaborate;

   type Holder is tagged private;

   function "=" (Left, Right : Holder) return Boolean;

   function To_Holder (
      Source : Name;
      Retain : Boolean := True)
      return Holder;

   --  utility to make unary "+" for simple conversion
   --  example: function "+" is new Generic_To_Holder (Retain => True);
   generic
      Retain : Boolean := True;
   function Generic_To_Holder (Source : Name) return Holder;

   function Null_Holder return Holder;

   function Is_Null (Container : Holder) return Boolean;

   procedure Clear (Container : in out Holder);

   function Constant_Reference (Container : Holder) return Name;
   function Element (Container : Holder'Class) return Name;

   procedure Replace_Element (
      Target : in out Holder;
      Source : Name;
      Retain : Boolean := True);

   generic
      Retain : Boolean := True;
   procedure Generic_Replace_Element (
      Target : in out Holder;
      Source : Name);

   procedure Assign (
      Target : in out Holder;
      Source : Holder);

   procedure Move (
      Target : in out Holder;
      Source : in out Holder);

   procedure Swap (I, J : in out Holder);

private

   type Holder is new Finalization.Controlled with record
      Item : aliased Name;
   end record;

   overriding procedure Adjust (Object : in out Holder);
   overriding procedure Finalize (Object : in out Holder);

end Ada.Containers.Intrusive_Counted_Access_Holders;
