with Ada.Exceptions.Finally;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
package body Ada.Containers.Indefinite_Holders is
   use type Copy_On_Write.Data_Access;

   function Upcast is
      new Unchecked_Conversion (Data_Access, Copy_On_Write.Data_Access);
   function Downcast is
      new Unchecked_Conversion (Copy_On_Write.Data_Access, Data_Access);

   procedure Free is new Unchecked_Deallocation (Element_Type, Element_Access);
   procedure Free is new Unchecked_Deallocation (Data, Data_Access);

   procedure Allocate_Element (
      Item : out Element_Access;
      New_Item : Element_Type);
   procedure Allocate_Element (
      Item : out Element_Access;
      New_Item : Element_Type) is
   begin
      Item := new Element_Type'(New_Item);
   end Allocate_Element;

   procedure Free_Data (Data : in out Copy_On_Write.Data_Access);
   procedure Free_Data (Data : in out Copy_On_Write.Data_Access) is
      X : Data_Access := Downcast (Data);
   begin
      Free (X.Element);
      Free (X);
   end Free_Data;

   procedure Allocate_Data (
      Target : out not null Copy_On_Write.Data_Access;
      Max_Length : Count_Type;
      Capacity : Count_Type);
   procedure Allocate_Data (
      Target : out not null Copy_On_Write.Data_Access;
      Max_Length : Count_Type;
      Capacity : Count_Type)
   is
      pragma Unreferenced (Max_Length);
      pragma Unreferenced (Capacity);
      New_Data : constant Data_Access := new Data'(
         Super => <>,
         Element => null);
   begin
      Target := Upcast (New_Data);
   end Allocate_Data;

   procedure Copy_Data (
      Target : out not null Copy_On_Write.Data_Access;
      Source : not null Copy_On_Write.Data_Access;
      Length : Count_Type;
      Max_Length : Count_Type;
      Capacity : Count_Type);
   procedure Copy_Data (
      Target : out not null Copy_On_Write.Data_Access;
      Source : not null Copy_On_Write.Data_Access;
      Length : Count_Type;
      Max_Length : Count_Type;
      Capacity : Count_Type)
   is
      pragma Unreferenced (Length);
      pragma Unreferenced (Max_Length);
      pragma Unreferenced (Capacity);
      Aliased_Target : aliased Copy_On_Write.Data_Access;
   begin
      Allocate_Data (Aliased_Target, 0, 0);
      declare
         procedure Finally (X : not null access Copy_On_Write.Data_Access);
         procedure Finally (X : not null access Copy_On_Write.Data_Access) is
         begin
            Free_Data (X.all);
         end Finally;
         package Holder is
            new Exceptions.Finally.Scoped_Holder (
               Copy_On_Write.Data_Access,
               Finally);
      begin
         Holder.Assign (Aliased_Target'Access);
         Allocate_Element (
            Downcast (Aliased_Target).Element,
            Downcast (Source).Element.all);
         Target := Aliased_Target;
         Holder.Clear;
      end;
   end Copy_Data;

   procedure Reallocate (Container : in out Holder; To_Update : Boolean);
   procedure Reallocate (Container : in out Holder; To_Update : Boolean) is
   begin
      Copy_On_Write.Unique (
         Target => Container.Super'Access,
         Target_Length => 0, -- Length is unused
         Target_Capacity => 0, -- Capacity is unused
         New_Length => 0,
         New_Capacity => 0,
         To_Update => To_Update,
         Allocate => Allocate_Data'Access,
         Move => Copy_Data'Access,
         Copy => Copy_Data'Access,
         Free => Free_Data'Access);
   end Reallocate;

   procedure Unique (Container : in out Holder; To_Update : Boolean);
   procedure Unique (Container : in out Holder; To_Update : Boolean) is
   begin
      if Copy_On_Write.Shared (Container.Super.Data) then
         Reallocate (Container, To_Update);
      end if;
   end Unique;

   --  implementation

   function Empty_Holder return Holder is
   begin
      return (Finalization.Controlled with Super => (null, null));
   end Empty_Holder;

   overriding function "=" (Left, Right : Holder) return Boolean is
   begin
      if Left.Super.Data = Right.Super.Data then
         return True;
      elsif Is_Empty (Left) or else Is_Empty (Right) then
         return Is_Empty (Left) and then Is_Empty (Right);
      else
         return Downcast (Left.Super.Data).Element.all =
            Downcast (Right.Super.Data).Element.all;
      end if;
   end "=";

   function To_Holder (New_Item : Element_Type) return Holder is
   begin
      return Result : Holder do
         Replace_Element (Result, New_Item);
      end return;
   end To_Holder;

   function Is_Empty (Container : Holder) return Boolean is
   begin
      return Container.Super.Data = null
         or else Downcast (Container.Super.Data).Element = null;
   end Is_Empty;

   procedure Clear (Container : in out Holder) is
   begin
      Copy_On_Write.Clear (
         Container.Super'Access,
         Free => Free_Data'Access);
   end Clear;

   function Element (Container : Holder'Class) return Element_Type is
   begin
      return Constant_Reference (Holder (Container)).Element.all;
   end Element;

   procedure Replace_Element (
      Container : in out Holder;
      New_Item : Element_Type) is
   begin
      Clear (Container);
      Reallocate (Container, True);
      Allocate_Element (Downcast (Container.Super.Data).Element, New_Item);
   end Replace_Element;

   procedure Query_Element (
      Container : Holder'Class;
      Process : not null access procedure (Element : Element_Type)) is
   begin
      Process (Constant_Reference (Holder (Container)).Element.all);
   end Query_Element;

   procedure Update_Element (
      Container : in out Holder'Class;
      Process : not null access procedure (Element : in out Element_Type)) is
   begin
      Process (Reference (Holder (Container)).Element.all);
   end Update_Element;

   function Constant_Reference (
      Container : aliased Holder)
      return Constant_Reference_Type is
   begin
      Unique (Container'Unrestricted_Access.all, False);
      return (Element =>
         Downcast (Container.Super.Data).Element.all'Access);
   end Constant_Reference;

   function Reference (
      Container : aliased in out Holder)
      return Reference_Type is
   begin
      Unique (Container, True);
      return (Element =>
         Downcast (Container.Super.Data).Element.all'Access);
   end Reference;

   procedure Assign (Target : in out Holder; Source : Holder) is
   begin
      Copy_On_Write.Assign (
         Target.Super'Access,
         Source.Super'Access,
         Free => Free_Data'Access);
   end Assign;

   function Copy (Source : Holder) return Holder is
   begin
      return (Finalization.Controlled with
         Super => Copy_On_Write.Copy (
            Source.Super'Access,
            0, -- Length is unused
            0, -- Capacity is unused
            Allocate => Allocate_Data'Access,
            Copy => Copy_Data'Access));
   end Copy;

   procedure Move (Target : in out Holder; Source : in out Holder) is
   begin
      Copy_On_Write.Move (
         Target.Super'Access,
         Source.Super'Access,
         Free => Free_Data'Access);
   end Move;

   overriding procedure Adjust (Object : in out Holder) is
   begin
      Copy_On_Write.Adjust (Object.Super'Access);
   end Adjust;

   package body Streaming is

      procedure Read (
         Stream : not null access Streams.Root_Stream_Type'Class;
         Item : out Holder) is
      begin
         Clear (Item);
         Reallocate (Item, True);
         Allocate_Element (
            Downcast (Item.Super.Data).Element,
            Element_Type'Input (Stream));
      end Read;

      procedure Write (
         Stream : not null access Streams.Root_Stream_Type'Class;
         Item : Holder) is
      begin
         Element_Type'Output (
            Stream,
            Downcast (Item.Super.Data).Element.all);
      end Write;

   end Streaming;

end Ada.Containers.Indefinite_Holders;
