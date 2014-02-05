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

   procedure Free_Data (Data : in out Copy_On_Write.Data_Access);
   procedure Free_Data (Data : in out Copy_On_Write.Data_Access) is
      X : Data_Access := Downcast (Data);
   begin
      Free (X.Element);
      Free (X);
   end Free_Data;

   procedure Allocate_Data (
      Target : out Copy_On_Write.Data_Access;
      Capacity : Count_Type);
   procedure Allocate_Data (
      Target : out Copy_On_Write.Data_Access;
      Capacity : Count_Type)
   is
      pragma Unreferenced (Capacity);
      New_Data : constant Data_Access := new Data'(
         Super => <>,
         Element => null);
   begin
      Target := Upcast (New_Data);
   end Allocate_Data;

   procedure Copy_Data (
      Target : out Copy_On_Write.Data_Access;
      Source : not null Copy_On_Write.Data_Access;
      Length : Count_Type;
      Capacity : Count_Type);
   procedure Copy_Data (
      Target : out Copy_On_Write.Data_Access;
      Source : not null Copy_On_Write.Data_Access;
      Length : Count_Type;
      Capacity : Count_Type) is
   begin
      pragma Unreferenced (Length);
      pragma Unreferenced (Capacity);
      Allocate_Data (Target, 0);
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
         Holder.Assign (Target'Unrestricted_Access);
         Downcast (Target).Element :=
            new Element_Type'(Downcast (Source).Element.all);
         Holder.Clear;
      end;
   end Copy_Data;

   procedure Unique (Container : in out Holder; To_Update : Boolean);
   procedure Unique (Container : in out Holder; To_Update : Boolean) is
   begin
      Copy_On_Write.Unique (
         Container.Super'Access,
         To_Update,
         0, -- Length is unused
         0, -- Capacity is unused
         Allocate => Allocate_Data'Access,
         Copy => Copy_Data'Access,
         Free => Free_Data'Access);
   end Unique;

   --  implementation

   overriding procedure Adjust (Object : in out Holder) is
   begin
      Copy_On_Write.Adjust (Object.Super'Access);
   end Adjust;

   procedure Assign (Target : in out Holder; Source : Holder) is
   begin
      Copy_On_Write.Assign (
         Target.Super'Access,
         Source.Super'Access,
         Free => Free_Data'Access);
   end Assign;

   procedure Clear (Container : in out Holder) is
   begin
      Copy_On_Write.Clear (
         Container.Super'Access,
         Free => Free_Data'Access);
   end Clear;

   function Constant_Reference (
      Container : aliased Holder)
      return Constant_Reference_Type is
   begin
      Unique (Container'Unrestricted_Access.all, False);
      return (Element =>
         Downcast (Container.Super.Data).Element.all'Unrestricted_Access);
   end Constant_Reference;

   function Copy (Source : Holder) return Holder is
   begin
      return (Finalization.Controlled with Super => Copy_On_Write.Copy (
         Source.Super'Access,
         0, -- Length is unused
         0, -- Capacity is unused
         Allocate => Allocate_Data'Access,
         Copy => Copy_Data'Access));
   end Copy;

   function Element (Container : Holder'Class) return Element_Type is
   begin
      return Container.Constant_Reference.Element.all;
   end Element;

   function Empty_Holder return Holder is
   begin
      return (Finalization.Controlled with Super => (null, null));
   end Empty_Holder;

   procedure Move (Target : in out Holder; Source : in out Holder) is
   begin
      Copy_On_Write.Move (
         Target.Super'Access,
         Source.Super'Access,
         Free => Free_Data'Access);
   end Move;

   function Is_Empty (Container : Holder) return Boolean is
   begin
      return Container.Super.Data = null
         or else Downcast (Container.Super.Data).Element = null;
   end Is_Empty;

   procedure Query_Element (
      Container : Holder'Class;
      Process : not null access procedure (Element : Element_Type)) is
   begin
      Process (Container.Constant_Reference.Element.all);
   end Query_Element;

   function Reference (
      Container : aliased in out Holder)
      return Reference_Type is
   begin
      Unique (Container, True);
      return (Element =>
         Downcast (Container.Super.Data).Element.all'Unrestricted_Access);
   end Reference;

   procedure Replace_Element (
      Container : in out Holder;
      New_Item : Element_Type) is
   begin
      Clear (Container);
      Unique (Container, True);
      Downcast (Container.Super.Data).Element := new Element_Type'(New_Item);
   end Replace_Element;

   function To_Holder (New_Item : Element_Type) return Holder is
   begin
      return Result : Holder do
         Unique (Result, True);
         Downcast (Result.Super.Data).Element := new Element_Type'(New_Item);
      end return;
   end To_Holder;

   procedure Update_Element (
      Container : in out Holder'Class;
      Process : not null access procedure (Element : in out Element_Type)) is
   begin
      Process (Container.Reference.Element.all);
   end Update_Element;

   function "=" (Left, Right : Holder) return Boolean is
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

   package body Streaming is

      procedure Read (
         Stream : not null access Streams.Root_Stream_Type'Class;
         Item : out Holder) is
      begin
         Clear (Item);
         Unique (Item, True);
         Downcast (Item.Super.Data).Element :=
            new Element_Type'(Element_Type'Input (Stream));
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
