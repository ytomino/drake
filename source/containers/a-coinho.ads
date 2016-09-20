pragma License (Unrestricted);
--  Ada 2012
private with Ada.Containers.Copy_On_Write;
private with Ada.Finalization;
private with Ada.Streams;
generic
   type Element_Type (<>) is private;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
package Ada.Containers.Indefinite_Holders is
   pragma Preelaborate;
   pragma Remote_Types;

   type Holder is tagged private;
   pragma Preelaborable_Initialization (Holder);

   --  modified
--  Empty_Holder : constant Holder;
   function Empty_Holder return Holder;

   overriding function "=" (Left, Right : Holder) return Boolean;

   function To_Holder (New_Item : Element_Type) return Holder;

   function Is_Empty (Container : Holder) return Boolean;

   procedure Clear (Container : in out Holder);

   --  modified
   function Element (Container : Holder'Class) -- not primitive
      return Element_Type;

   procedure Replace_Element (
      Container : in out Holder;
      New_Item : Element_Type);

   --  modified
   procedure Query_Element (
      Container : Holder'Class; -- not primitive
      Process : not null access procedure (Element : Element_Type));

   --  modified
   procedure Update_Element (
      Container : in out Holder'Class; -- not primitive
      Process : not null access procedure (Element : in out Element_Type));

   type Constant_Reference_Type (
      Element : not null access constant Element_Type) is private
      with Implicit_Dereference => Element;

   type Reference_Type (Element : not null access Element_Type) is private
      with Implicit_Dereference => Element;

   function Constant_Reference (Container : aliased Holder)
      return Constant_Reference_Type;

   function Reference (Container : aliased in out Holder)
      return Reference_Type;

   procedure Assign (Target : in out Holder; Source : Holder);

   function Copy (Source : Holder) return Holder;

   procedure Move (Target : in out Holder; Source : in out Holder);

private

   type Element_Access is access Element_Type;

   type Data is limited record
      Super : aliased Copy_On_Write.Data;
      Element : Element_Access;
   end record;
   pragma Suppress_Initialization (Data);

   type Data_Access is access all Data;

   type Holder is new Finalization.Controlled with record
      Super : aliased Copy_On_Write.Container;
   end record;

   overriding procedure Adjust (Object : in out Holder);
   overriding procedure Finalize (Object : in out Holder)
      renames Clear;

   type Constant_Reference_Type (
      Element : not null access constant Element_Type) is null record;

   type Reference_Type (Element : not null access Element_Type) is null record;

   package Streaming is

      procedure Read (
         Stream : not null access Streams.Root_Stream_Type'Class;
         Item : out Holder);
      procedure Write (
         Stream : not null access Streams.Root_Stream_Type'Class;
         Item : Holder);

      procedure Missing_Read (
         Stream : access Streams.Root_Stream_Type'Class;
         Item : out Constant_Reference_Type)
         with Import,
            Convention => Ada, External_Name => "__drake_program_error";
      procedure Missing_Write (
         Stream : access Streams.Root_Stream_Type'Class;
         Item : Constant_Reference_Type)
         with Import,
            Convention => Ada, External_Name => "__drake_program_error";

      procedure Missing_Read (
         Stream : access Streams.Root_Stream_Type'Class;
         Item : out Reference_Type)
         with Import,
            Convention => Ada, External_Name => "__drake_program_error";
      procedure Missing_Write (
         Stream : access Streams.Root_Stream_Type'Class;
         Item : Reference_Type)
         with Import,
            Convention => Ada, External_Name => "__drake_program_error";

   end Streaming;

   for Holder'Read use Streaming.Read;
   for Holder'Write use Streaming.Write;

   for Constant_Reference_Type'Read use Streaming.Missing_Read;
   for Constant_Reference_Type'Write use Streaming.Missing_Write;

   for Reference_Type'Read use Streaming.Missing_Read;
   for Reference_Type'Write use Streaming.Missing_Write;

end Ada.Containers.Indefinite_Holders;
