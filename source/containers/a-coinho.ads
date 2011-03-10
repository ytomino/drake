pragma License (Unrestricted);
--  Ada 2012
private with Ada.Finalization;
private with Ada.Streams;
private with System.Reference_Counting;
generic
   type Element_Type (<>) is private;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
package Ada.Containers.Indefinite_Holders is
   pragma Preelaborate;
--  pragma Remote_Types; -- it defends to define Reference_Type...

   type Holder is tagged private;
   pragma Preelaborable_Initialization (Holder);

--  Empty_Holder : constant Holder;
   function Empty_Holder return Holder; -- extended

   function "=" (Left, Right : Holder) return Boolean;

   function To_Holder (New_Item : Element_Type) return Holder;

   function Is_Empty (Container : Holder) return Boolean;

   procedure Clear (Container : in out Holder);

   function Element (Container : Holder) return Element_Type;

   procedure Replace_Element (
      Container : in out Holder;
      New_Item : Element_Type);

   procedure Query_Element (
      Container : Holder;
      Process : not null access procedure (Element : Element_Type));

   procedure Update_Element (
      Container : in out Holder;
      Process : not null access procedure (Element : in out Element_Type));

   procedure Assign (Target : in out Holder; Source : Holder);

   function Copy (Source : Holder) return Holder;

   procedure Move (Target : in out Holder; Source : in out Holder);

   --  AI05-0212-1
   type Constant_Reference_Type (
      Element : not null access constant Element_Type) is limited private;
   type Reference_Type (
      Element : not null access Element_Type) is limited private;
   function Constant_Reference (Container : not null access constant Holder)
      return Constant_Reference_Type;
   function Reference (Container : not null access Holder)
      return Reference_Type;

private

   type Element_Access is access Element_Type;

   type Data is limited record
      Reference_Count : aliased System.Reference_Counting.Counter;
      Element : Element_Access;
   end record;
   pragma Suppress_Initialization (Data);

   type Data_Access is access all Data;

   Empty_Data : aliased constant Data := (
      Reference_Count => System.Reference_Counting.Static,
      Element => null);

   type Holder is new Finalization.Controlled with record
      Data : aliased not null Data_Access := Empty_Data'Unrestricted_Access;
   end record;

   overriding procedure Adjust (Object : in out Holder);
   overriding procedure Finalize (Object : in out Holder)
      renames Clear;

   package No_Primitives is
      procedure Read (
         Stream : not null access Streams.Root_Stream_Type'Class;
         Container : out Holder);
      procedure Write (
         Stream : not null access Streams.Root_Stream_Type'Class;
         Container : Holder);
   end No_Primitives;

   for Holder'Read use No_Primitives.Read;
   for Holder'Write use No_Primitives.Write;

   type Constant_Reference_Type (
      Element : not null access constant Element_Type) is limited null record;
   type Reference_Type (
      Element : not null access Element_Type) is limited null record;

end Ada.Containers.Indefinite_Holders;
