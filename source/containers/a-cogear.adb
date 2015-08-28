with Ada.Containers.Array_Sorting;
with System;
package body Ada.Containers.Generic_Arrays is

   --  implementation

   procedure Append (
      Container : in out Array_Access;
      New_Item : Array_Type) is
   begin
      Insert (Container, Container'Last + 1, New_Item);
   end Append;

   procedure Append (
      Container : in out Array_Access;
      New_Item : Array_Access) is
   begin
      Insert (Container, Container'Last + 1, New_Item);
   end Append;

   procedure Append (
      Container : in out Array_Access;
      New_Item : Element_Type;
      Count : Count_Type := 1) is
   begin
      Insert (Container, Container'Last + 1, New_Item, Count);
   end Append;

   procedure Append (
      Container : in out Array_Access;
      Count : Count_Type := 1) is
   begin
      Insert (Container, Container'Last + 1, Count);
   end Append;

   procedure Assign (Target : in out Array_Access; Source : Array_Access) is
   begin
      if Target /= Source then
         Free (Target);
         if Source /= null and then Source'Length > 0 then
            Target := new Array_Type'(Source.all);
         end if;
      end if;
   end Assign;

   procedure Assign (Target : in out Array_Access; Source : New_Array) is
   begin
      Free (Target);
      Target := Array_Access (Source);
   end Assign;

   procedure Delete (
      Container : in out Array_Access;
      Index : Extended_Index;
      Count : Count_Type := 1) is
   begin
      if Container = null then
         null;
      elsif Index = Container'First and then Count = Container'Length then
         Free (Container);
      else
         declare
            S : Array_Access := Container;
         begin
            if Index = Container'First then
               Container := new Array_Type'(
                  Container (
                     Index + Index_Type'Base (Count) ..
                     Container'Last));
            elsif Index + Index_Type'Base (Count) - 1 = Container'Last then
               Container := new Array_Type'(
                  Container (
                     Container'First ..
                     Index_Type'Base'Pred (Index)));
            else
               Container := new Array_Type'(
                  Container (
                     Container'First ..
                     Index_Type'Base'Pred (Index))
                  & Container (
                     Index + Index_Type'Base (Count) ..
                     Container'Last));
            end if;
            Free (S);
         end;
      end if;
   end Delete;

   procedure Delete_First (
      Container : in out Array_Access;
      Count : Count_Type := 1) is
   begin
      Delete (Container, Container'First, Count);
   end Delete_First;

   procedure Delete_Last (
      Container : in out Array_Access;
      Count : Count_Type := 1) is
   begin
      Delete (Container, Container'Last - Index_Type'Base (Count) + 1, Count);
   end Delete_Last;

   procedure Insert (
      Container : in out Array_Access;
      Before : Extended_Index;
      New_Item : Array_Type) is
   begin
      if New_Item'Length = 0 then
         null;
      elsif Container = null then
         Container := new Array_Type'(New_Item);
      else
         declare
            S : Array_Access := Container;
         begin
            if Before = Container'First then
               Container := new Array_Type'(
                  New_Item
                  & S (Before .. S'Last));
            elsif Before > Container'Last then
               Container := new Array_Type'(
                  S (S'First .. Before - 1)
                  & New_Item);
            else
               Container := new Array_Type'(
                  S (S'First .. Before - 1)
                  & New_Item
                  & S (Before .. S'Last));
            end if;
            Free (S);
         end;
      end if;
   end Insert;

   procedure Insert (
      Container : in out Array_Access;
      Before : Extended_Index;
      New_Item : Array_Access) is
   begin
      if New_Item /= null then
         Insert (Container, Before, New_Item.all);
      end if;
   end Insert;

   procedure Insert (
      Container : in out Array_Access;
      Before : Extended_Index;
      New_Item : Element_Type;
      Count : Count_Type := 1) is
   begin
      Insert (Container, Before, Count);
      for I in Before .. Before + Index_Type'Base (Count) - 1 loop
         Container (I) := New_Item;
      end loop;
   end Insert;

   procedure Insert (
      Container : in out Array_Access;
      Before : Extended_Index;
      Count : Count_Type := 1) is
   begin
      if Count = 0 then
         null;
      elsif Container = null then
         Container := new Array_Type (
            Before ..
            Before + Index_Type'Base (Count) - 1);
      else
         declare
            S : Array_Access := Container;
         begin
            if Before = Container'First then
               Container := new Array_Type'(
                  Array_Type'(
                     Index_Type'First ..
                     Index_Type'First + Index_Type'Base (Count) - 1 => <>)
                  & S (Before .. S'Last));
            elsif Before > Container'Last then
               Container := new Array_Type'(
                  S (S'First .. Before - 1)
                  & Array_Type'(
                     Index_Type'First ..
                     Index_Type'First + Index_Type'Base (Count) - 1 => <>));
            else
               Container := new Array_Type'(
                  S (S'First .. Before - 1)
                  & Array_Type'(
                     Index_Type'First ..
                     Index_Type'First + Index_Type'Base (Count) - 1 => <>)
                  & S (Before .. S'Last));
            end if;
            Free (S);
         end;
      end if;
   end Insert;

   function Length (Container : Array_Access) return Count_Type is
   begin
      if Container = null then
         return 0;
      else
         return Container'Length;
      end if;
   end Length;

   procedure Move (
      Target : in out Array_Access;
      Source : in out Array_Access) is
   begin
      if Target /= Source then
         Free (Target);
         Target := Source;
         Source := null;
      end if;
   end Move;

   procedure Prepend (
      Container : in out Array_Access;
      New_Item : Array_Type) is
   begin
      Insert (Container, Container'First, New_Item);
   end Prepend;

   procedure Prepend (
      Container : in out Array_Access;
      New_Item : Array_Access) is
   begin
      Insert (Container, Container'First, New_Item);
   end Prepend;

   procedure Prepend (
      Container : in out Array_Access;
      New_Item : Element_Type;
      Count : Count_Type := 1) is
   begin
      Insert (Container, Container'First, New_Item, Count);
   end Prepend;

   procedure Prepend (
      Container : in out Array_Access;
      Count : Count_Type := 1) is
   begin
      Insert (Container, Container'First, Count);
   end Prepend;

   procedure Set_Length (
      Container : in out Array_Access;
      Length : Count_Type) is
   begin
      if Container = null then
         if Length > 0 then
            Container := new Array_Type (
               Index_Type'First ..
               Index_Type'First + Index_Type'Base (Length) - 1);
         end if;
      elsif Length = 0 then
         Free (Container);
      elsif Length < Container'Length then
         declare
            S : Array_Access := Container;
         begin
            Container := new Array_Type'(
               Container (
                  Container'First ..
                  Container'First + Index_Type'Base (Length) - 1));
            Free (S);
         end;
      elsif Length > Container'Length then
         declare
            S : Array_Access := Container;
         begin
            Container := new Array_Type'(
               Container.all
               & Array_Type'(
                  Index_Type'First ..
                  Index_Type'First
                     + Index_Type'Base (Length - Container'Length) - 1 => <>));
            Free (S);
         end;
      end if;
   end Set_Length;

   procedure Swap (Container : in out Array_Access; I, J : Index_Type) is
      pragma Unmodified (Container);
   begin
      if I /= J then
         declare
            Temp : constant Element_Type := Container (I);
         begin
            Container.all (I) := Container (J);
            Container.all (J) := Temp;
         end;
      end if;
   end Swap;

   package body Operators is

      function Start (
         Left : Array_Access;
         Right : Element_Type;
         Space : Count_Type)
         return New_Array_1;
      function Start (
         Left : Array_Access;
         Right : Element_Type;
         Space : Count_Type)
         return New_Array_1
      is
         subtype Space_Range is
            Index_Type range
               Index_Type'First ..
               Extended_Index'Val (
                  Index_Type'Pos (Index_Type'First) + Space - 1);
      begin
         if Left = null then
            return (
               Data => new Array_Type'(
                  Right
                  & Array_Type'(Space_Range => <>)),
               Last => Index_Type'First);
         else
            return (
               Data => new Array_Type'(
                  Left.all
                  & Right
                  & Array_Type'(Space_Range => <>)),
               Last => Left'Last + 1);
         end if;
      end Start;

      function Step (
         Left : New_Array_1;
         Right : Element_Type;
         Space : Count_Type)
         return New_Array_1;
      function Step (
         Left : New_Array_1;
         Right : Element_Type;
         Space : Count_Type)
         return New_Array_1 is
      begin
         if Left.Last + 1 <= Left.Data'Last then
            Left.Data (Left.Last + 1) := Right;
            return (Data => Left.Data, Last => Left.Last + 1);
         else
            declare
               subtype Space_Range is
                  Index_Type range
                     Index_Type'First ..
                     Extended_Index'Val (
                        Index_Type'Pos (Index_Type'First) + Space - 1);
            begin
               return Result : constant New_Array_1 := (
                  Data => new Array_Type'(
                     Left.Data (Left.Data'First .. Left.Last)
                     & Right
                     & Array_Type'(Space_Range => <>)),
                  Last => Left.Last + 1)
               do
                  declare
                     Data : Array_Access := Left.Data;
                  begin
                     Free (Data);
                  end;
               end return;
            end;
         end if;
      end Step;

      --  implementation

      function "&" (Left : Array_Access; Right : Element_Type)
         return New_Array is
      begin
         return New_Array (Start (Left, Right, Space => 0).Data);
      end "&";

      function "&" (Left : New_Array_1; Right : Element_Type)
         return New_Array
      is
         Data : Array_Access := Left.Data;
      begin
         if Left.Last + 1 = Data'Last then
            Data (Data'Last) := Right;
            return New_Array (Data);
         else
            return Result : constant New_Array :=
               new Array_Type'(Data (Data'First .. Data'Last) & Right)
            do
               Free (Data);
            end return;
         end if;
      end "&";

      function "&" (Left : Array_Access; Right : Element_Type)
         return New_Array_1 is
      begin
         return Start (Left, Right, Space => 1);
      end "&";

      function "&" (Left : New_Array_2; Right : Element_Type)
         return New_Array_1 is
      begin
         return Step (New_Array_1 (Left), Right, Space => 1);
      end "&";

      function "&" (Left : Array_Access; Right : Element_Type)
         return New_Array_2 is
      begin
         return New_Array_2 (Start (Left, Right, Space => 2));
      end "&";

   end Operators;

   package body Generic_Reversing is

      type Context_Type is limited record
         Container : Array_Access;
      end record;
      pragma Suppress_Initialization (Context_Type);

      procedure Swap (I, J : Integer; Params : System.Address);
      procedure Swap (I, J : Integer; Params : System.Address) is
         Context : Context_Type;
         for Context'Address use Params;
      begin
         Swap (Context.Container, Index_Type'Val (I), Index_Type'Val (J));
      end Swap;

      --  implementation

      procedure Reverse_Elements (Container : in out Array_Access) is
         pragma Unmodified (Container);
         Context : Context_Type := (Container => Container);
      begin
         Array_Sorting.In_Place_Reverse (
            Index_Type'Pos (Container'First),
            Index_Type'Pos (Container'Last),
            Context'Address,
            Swap => Swap'Access);
      end Reverse_Elements;

      procedure Reverse_Rotate_Elements (
         Container : in out Array_Access;
         Before : Extended_Index)
      is
         pragma Unmodified (Container);
         Context : Context_Type := (Container => Container);
      begin
         Array_Sorting.Reverse_Rotate (
            Index_Type'Pos (Container'First),
            Index_Type'Pos (Before) - 1,
            Index_Type'Pos (Container'Last),
            Context'Address,
            Swap => Swap'Access);
      end Reverse_Rotate_Elements;

      procedure Juggling_Rotate_Elements (
         Container : in out Array_Access;
         Before : Extended_Index)
      is
         pragma Unmodified (Container);
         Context : Context_Type := (Container => Container);
      begin
         Array_Sorting.Juggling_Rotate (
            Index_Type'Pos (Container'First),
            Index_Type'Pos (Before) - 1,
            Index_Type'Pos (Container'Last),
            Context'Address,
            Swap => Swap'Access);
      end Juggling_Rotate_Elements;

   end Generic_Reversing;

   package body Generic_Sorting is

      type Context_Type is limited record
         Container : Array_Access;
      end record;
      pragma Suppress_Initialization (Context_Type);

      function LT (Left, Right : Integer; Params : System.Address)
         return Boolean;
      function LT (Left, Right : Integer; Params : System.Address)
         return Boolean
      is
         Context : Context_Type;
         for Context'Address use Params;
      begin
         return Context.Container (Index_Type'Val (Left)) <
            Context.Container (Index_Type'Val (Right));
      end LT;

      procedure Swap (I, J : Integer; Params : System.Address);
      procedure Swap (I, J : Integer; Params : System.Address) is
         Context : Context_Type;
         for Context'Address use Params;
      begin
         Swap (Context.Container, Index_Type'Val (I), Index_Type'Val (J));
      end Swap;

      --  implementation

      function Is_Sorted (Container : Array_Access) return Boolean is
         Context : Context_Type := (Container => Container);
      begin
         return Container = null
            or else
               Array_Sorting.Is_Sorted (
                  Index_Type'Pos (Container'First),
                  Index_Type'Pos (Container'Last),
                  Context'Address,
                  LT => LT'Access);
      end Is_Sorted;

      procedure Insertion_Sort (Container : in out Array_Access) is
         pragma Unmodified (Container);
         Context : Context_Type := (Container => Container);
      begin
         if Container /= null then
            Array_Sorting.Insertion_Sort (
               Index_Type'Pos (Container'First),
               Index_Type'Pos (Container'Last),
               Context'Address,
               LT => LT'Access,
               Swap => Swap'Access);
         end if;
      end Insertion_Sort;

      procedure Merge_Sort (Container : in out Array_Access) is
         pragma Unmodified (Container);
         Context : Context_Type := (Container => Container);
      begin
         if Container /= null then
            Array_Sorting.In_Place_Merge_Sort (
               Index_Type'Pos (Container'First),
               Index_Type'Pos (Container'Last),
               Context'Address,
               LT => LT'Access,
               Swap => Swap'Access);
         end if;
      end Merge_Sort;

      procedure Merge (
         Target : in out Array_Access;
         Source : in out Array_Access)
      is
         Context : Context_Type := (Container => Target);
      begin
         if Target = null then
            Move (Target, Source);
         else
            declare
               Old_Length : constant Count_Type := Target'Length;
            begin
               Append (Target, Source);
               Free (Source);
               Array_Sorting.In_Place_Merge (
                  Index_Type'Pos (Target'First),
                  Integer (Target'First + Index_Type'Base (Old_Length) - 1),
                  Index_Type'Pos (Target'Last),
                  Context'Address,
                  LT => LT'Access,
                  Swap => Swap'Access);
            end;
         end if;
      end Merge;

   end Generic_Sorting;

end Ada.Containers.Generic_Arrays;
