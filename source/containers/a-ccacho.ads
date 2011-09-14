pragma License (Unrestricted);
--  extended unit
private with Ada.Finalization;
private with System.Reference_Counting;
generic
   type Name is private; -- it must have default value
   with procedure Free (X : in out Name) is <>;
package Ada.Containers.Counted_Access_Holders is
   --  Reference counted access types.
   pragma Preelaborate;

   type Holder is tagged private;

   function "=" (Left, Right : Holder) return Boolean;

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
   procedure Assign (Target : in out Holder; Source : Holder);

   procedure Move (
      Target : in out Holder;
      Source : in out Holder);

   procedure Swap (I, J : in out Holder);

   type Data (<>) is limited private; -- for implementation of Weak

   package Weak is

      type Weak_Holder is tagged private;

      function "=" (Left, Right : Weak_Holder) return Boolean;

      function To_Weak_Holder (Source : Holder) return Weak_Holder;
      function "+" (Right : Holder) return Weak_Holder
         renames To_Weak_Holder;

      function Null_Weak_Holder return Weak_Holder;

      function To_Holder (Source : Weak_Holder) return Holder;
      function "+" (Right : Weak_Holder) return Holder
         renames To_Holder;

      function Is_Null (Container : Weak_Holder) return Boolean;

      procedure Clear (Container : in out Weak_Holder);

      procedure Assign (
         Target : in out Weak_Holder;
         Source : Holder);
      procedure Assign (
         Target : in out Holder;
         Source : Weak_Holder);

      procedure Clear (Item : Data); -- for implementation

   private

      type Data_Access is access all Data;

      type Weak_Holder_Access is access all Weak_Holder;

      type Weak_Holder is new Finalization.Controlled with record
         Data : Data_Access := null;
         Previous : Weak_Holder_Access := null;
         Next : Weak_Holder_Access := null;
      end record;

      overriding procedure Adjust (Object : in out Weak_Holder);
      overriding procedure Finalize (Object : in out Weak_Holder);

   end Weak;

private

   type Data_Access is access all Data;

   type Weak_Holder_Access is access all Weak.Weak_Holder;

   type Data is limited record
      Item : aliased Name;
      Reference_Count : aliased System.Reference_Counting.Counter;
      Weak_List : Weak_Holder_Access;
   end record;
   pragma Suppress_Initialization (Data);

   Default_Data : aliased Data := (
      Item => <>,
      Reference_Count => System.Reference_Counting.Static,
      Weak_List => null);

   type Holder is new Finalization.Controlled with record
      Data : aliased not null Data_Access := Default_Data'Access;
   end record;

   overriding procedure Adjust (Object : in out Holder);
   overriding procedure Finalize (Object : in out Holder);

end Ada.Containers.Counted_Access_Holders;
