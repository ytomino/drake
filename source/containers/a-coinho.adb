with Ada.Exceptions.Finally;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System.Address_To_Named_Access_Conversions;
package body Ada.Containers.Indefinite_Holders is

   package Data_Cast is
      new System.Address_To_Named_Access_Conversions (Data, Data_Access);

   subtype Not_Null_Data_Access is not null Data_Access;
   type Data_Access_Access is access all Not_Null_Data_Access;
   type System_Address_Access is access all System.Address;
   function Upcast is new Unchecked_Conversion (
      Data_Access_Access,
      System_Address_Access);

   procedure Free is new Unchecked_Deallocation (Element_Type, Element_Access);
   procedure Free is new Unchecked_Deallocation (Data, Data_Access);

   function Hold (New_Item : Element_Access) return not null Data_Access;
   function Hold (New_Item : Element_Access) return not null Data_Access is
      Result : Data_Access;
      New_Item_Var : aliased Element_Access := New_Item;
      procedure Finally (X : not null access Element_Access);
      procedure Finally (X : not null access Element_Access) is
      begin
         Free (X.all);
      end Finally;
      package Holder is new Exceptions.Finally.Scoped_Holder (
         Element_Access,
         Finally);
   begin
      Holder.Assign (New_Item_Var'Access);
      Result := new Indefinite_Holders.Data'(
         Reference_Count => 1,
         Element => New_Item);
      Holder.Clear;
      return Result;
   end Hold;

   procedure Free_Data (Data : System.Address);
   procedure Free_Data (Data : System.Address) is
      X : Data_Access := Data_Cast.To_Pointer (Data);
   begin
      Free (X.Element);
      Free (X);
   end Free_Data;

   procedure Copy_Data (
      Target : out System.Address;
      Source : System.Address;
      Length : Natural;
      Max_Length : Natural;
      Capacity : Count_Type);
   procedure Copy_Data (
      Target : out System.Address;
      Source : System.Address;
      Length : Natural;
      Max_Length : Natural;
      Capacity : Count_Type)
   is
      pragma Unreferenced (Length);
      pragma Unreferenced (Max_Length);
      pragma Unreferenced (Capacity);
   begin
      Target := Data_Cast.To_Address (Hold (
         new Element_Type'(Data_Cast.To_Pointer (Source).Element.all)));
   end Copy_Data;

   procedure Unique (Container : in out Holder);
   procedure Unique (Container : in out Holder) is
   begin
      System.Reference_Counting.Unique (
         Target => Upcast (Container.Data'Unchecked_Access),
         Target_Reference_Count => Container.Data.Reference_Count'Access,
         Target_Length => 0,
         Target_Capacity => 0,
         Max_Length => 0,
         Capacity => 0,
         Sentinel => Empty_Data'Address,
         Copy => Copy_Data'Access,
         Free => Free_Data'Access);
   end Unique;

   --  implementation

   overriding procedure Adjust (Object : in out Holder) is
   begin
      System.Reference_Counting.Adjust (Object.Data.Reference_Count'Access);
   end Adjust;

   procedure Assign (Target : in out Holder; Source : Holder) is
   begin
      System.Reference_Counting.Assign (
         Target => Upcast (Target.Data'Unchecked_Access),
         Target_Reference_Count => Target.Data.Reference_Count'Access,
         Source => Upcast (Source.Data'Unrestricted_Access),
         Source_Reference_Count => Source.Data.Reference_Count'Access,
         Free => Free_Data'Access);
   end Assign;

   procedure Clear (Container : in out Holder) is
   begin
      System.Reference_Counting.Clear (
         Target => Upcast (Container.Data'Unchecked_Access),
         Reference_Count => Container.Data.Reference_Count'Access,
         Free => Free_Data'Access);
      Container.Data := Empty_Data'Unrestricted_Access;
   end Clear;

   function Constant_Reference (Container : not null access constant Holder)
      return Constant_Reference_Type is
   begin
      return (Element => Container.Data.Element.all'Unrestricted_Access);
   end Constant_Reference;

   function Copy (Source : Holder) return Holder is
   begin
      return Source; -- Adjust called by the auto
   end Copy;

   function Element (Container : Holder'Class) return Element_Type is
   begin
      return Constant_Reference (Container'Unrestricted_Access).Element.all;
   end Element;

   function Empty_Holder return Holder is
   begin
      return (Finalization.Controlled with
         Data => Empty_Data'Unrestricted_Access);
   end Empty_Holder;

   procedure Move (Target : in out Holder; Source : in out Holder) is
   begin
      System.Reference_Counting.Move (
         Target => Upcast (Target.Data'Unchecked_Access),
         Target_Reference_Count => Target.Data.Reference_Count'Access,
         Source => Upcast (Source.Data'Unchecked_Access),
         Sentinel => Empty_Data'Address,
         Free => Free_Data'Access);
   end Move;

   function Is_Empty (Container : Holder) return Boolean is
   begin
      return Container.Data.Element = null;
   end Is_Empty;

   procedure Query_Element (
      Container : Holder'Class;
      Process : not null access procedure (Element : Element_Type)) is
   begin
      Process (Constant_Reference (Container'Unrestricted_Access).Element.all);
   end Query_Element;

   function Reference (Container : not null access Holder)
      return Reference_Type is
   begin
      Unique (Container.all);
      return (Element => Container.Data.Element.all'Unrestricted_Access);
   end Reference;

   procedure Replace_Element (
      Container : in out Holder;
      New_Item : Element_Type) is
   begin
      Clear (Container);
      Container.Data := Hold (new Element_Type'(New_Item));
   end Replace_Element;

   function To_Holder (New_Item : Element_Type) return Holder is
   begin
      return Result : Holder do
         Result.Data := Hold (new Element_Type'(New_Item));
      end return;
   end To_Holder;

   procedure Update_Element (
      Container : in out Holder'Class;
      Process : not null access procedure (Element : in out Element_Type)) is
   begin
      Process (Reference (Container'Unrestricted_Access).Element.all);
   end Update_Element;

   function "=" (Left, Right : Holder) return Boolean is
   begin
      if Left.Data = Right.Data then
         return True;
      elsif Left.Data = null or else Right.Data = null then
         return False;
      else
         return Left.Data.Element.all = Right.Data.Element.all;
      end if;
   end "=";

   package body No_Primitives is

      procedure Read (
         Stream : not null access Streams.Root_Stream_Type'Class;
         Container : out Holder) is
      begin
         Clear (Container);
         Container.Data := Hold (
            new Element_Type'(Element_Type'Input (Stream)));
      end Read;

      procedure Write (
         Stream : not null access Streams.Root_Stream_Type'Class;
         Container : Holder) is
      begin
         Element_Type'Output (Stream, Container.Data.Element.all);
      end Write;

   end No_Primitives;

end Ada.Containers.Indefinite_Holders;
