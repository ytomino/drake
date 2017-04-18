with Ada.Containers.Array_Sorting;
with System;
package body Ada.Containers.Generic_Array_Access_Types is

   --  implementation

   function Length (Container : Array_Access) return Count_Type is
   begin
      if Container = null then
         return 0;
      else
         return Container'Length;
      end if;
   end Length;

   procedure Set_Length (
      Container : in out Array_Access;
      Length : Count_Type) is
   begin
      if Container = null then
         if Length > 0 then
            Container :=
               new Array_Type (
                  Index_Type'First ..
                  Index_Type'First + Index_Type'Base (Length) - 1);
         end if;
      elsif Length = 0 then
         Free (Container);
      else
         declare
            Old_Length : constant Count_Type := Container'Length;
         begin
            if Length < Old_Length then
               declare
                  S : Array_Access := Container;
               begin
                  Container :=
                     new Array_Type'(
                        S (S'First .. S'First + Index_Type'Base (Length) - 1));
                  Free (S);
               end;
            elsif Length > Old_Length then
               declare
                  S : Array_Access := Container;
               begin
                  Container :=
                     new Array_Type (
                        S'First .. S'First + Index_Type'Base (Length) - 1);
                  Container (S'Range) := S.all;
                  Free (S);
               end;
            end if;
         end;
      end if;
   end Set_Length;

   procedure Assign (Target : in out Array_Access; Source : Array_Access) is
   begin
      if Target /= Source then
         Free (Target);
         if Source /= null and then Source'Length > 0 then
            Target := new Array_Type'(Source.all);
         end if;
      end if;
   end Assign;

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

   procedure Insert (
      Container : in out Array_Access;
      Before : Extended_Index;
      New_Item : Array_Type)
   is
      pragma Check (Pre,
         Check =>
            Before in First_Index (Container) .. Last_Index (Container) + 1
            or else raise Constraint_Error);
   begin
      if New_Item'Length > 0 then
         if Container = null then
            declare
               subtype T is
                  Array_Type (Before .. Before + (New_Item'Length - 1));
            begin
               Container := new Array_Type'(T (New_Item));
            end;
         else
            declare
               New_Item_Length : constant Index_Type'Base := New_Item'Length;
               Following : constant Index_Type := Before + New_Item_Length;
               S : Array_Access := Container;
            begin
               Container :=
                  new Array_Type (S'First .. S'Last + New_Item_Length);
               Container (S'First .. Before - 1) := S (S'First .. Before - 1);
               Container (Before .. Following - 1) := New_Item;
               Container (Following .. Container'Last) := S (Before .. S'Last);
               Free (S);
            end;
         end if;
      end if;
   end Insert;

   procedure Insert (
      Container : in out Array_Access;
      Before : Extended_Index;
      New_Item : Array_Access)
   is
      pragma Check (Pre,
         Check =>
            Before in First_Index (Container) .. Last_Index (Container) + 1
            or else raise Constraint_Error);
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
      Insert (
         Container,
         Before, -- checking Constraint_Error
         Count);
      for I in Before .. Before + Index_Type'Base (Count) - 1 loop
         Container (I) := New_Item;
      end loop;
   end Insert;

   procedure Insert (
      Container : in out Array_Access;
      Before : Extended_Index;
      Count : Count_Type := 1)
   is
      pragma Check (Pre,
         Check =>
            Before in First_Index (Container) .. Last_Index (Container) + 1
            or else raise Constraint_Error);
   begin
      if Count > 0 then
         if Container = null then
            Container :=
               new Array_Type (Before .. Before + Index_Type'Base (Count) - 1);
         else
            declare
               Following : constant Index_Type :=
                  Before + Index_Type'Base (Count);
               S : Array_Access := Container;
            begin
               Container :=
                  new Array_Type (S'First .. S'Last + Index_Type'Base (Count));
               Container (S'First .. Before - 1) := S (S'First .. Before - 1);
               Container (Following .. Container'Last) := S (Before .. S'Last);
               Free (S);
            end;
         end if;
      end if;
   end Insert;

   procedure Prepend (
      Container : in out Array_Access;
      New_Item : Array_Type) is
   begin
      Insert (Container, First_Index (Container), New_Item);
   end Prepend;

   procedure Prepend (
      Container : in out Array_Access;
      New_Item : Array_Access) is
   begin
      Insert (Container, First_Index (Container), New_Item);
   end Prepend;

   procedure Prepend (
      Container : in out Array_Access;
      New_Item : Element_Type;
      Count : Count_Type := 1) is
   begin
      Insert (Container, First_Index (Container), New_Item, Count);
   end Prepend;

   procedure Prepend (
      Container : in out Array_Access;
      Count : Count_Type := 1) is
   begin
      Insert (Container, First_Index (Container), Count);
   end Prepend;

   procedure Append (
      Container : in out Array_Access;
      New_Item : Array_Type) is
   begin
      Insert (Container, Last_Index (Container) + 1, New_Item);
   end Append;

   procedure Append (
      Container : in out Array_Access;
      New_Item : Array_Access) is
   begin
      Insert (Container, Last_Index (Container) + 1, New_Item);
   end Append;

   procedure Append (
      Container : in out Array_Access;
      New_Item : Element_Type;
      Count : Count_Type := 1) is
   begin
      Insert (Container, Last_Index (Container) + 1, New_Item, Count);
   end Append;

   procedure Append (
      Container : in out Array_Access;
      Count : Count_Type := 1) is
   begin
      Insert (Container, Last_Index (Container) + 1, Count);
   end Append;

   procedure Delete (
      Container : in out Array_Access;
      Index : Extended_Index;
      Count : Count_Type := 1)
   is
      pragma Check (Pre,
         Check =>
            (Index >= First_Index (Container)
               and then Index + Index_Type'Base (Count) - 1 <=
                  Last_Index (Container))
            or else raise Constraint_Error);
   begin
      if Count > 0 then
         if Index = Container'First and then Count = Container'Length then
            Free (Container);
         else
            declare
               Following : constant Index_Type :=
                  Index + Index_Type'Base (Count);
               S : Array_Access := Container;
            begin
               Container :=
                  new Array_Type (S'First .. S'Last - Index_Type'Base (Count));
               Container (S'First .. Index - 1) := S (S'First .. Index - 1);
               Container (Index .. Container'Last) := S (Following .. S'Last);
               Free (S);
            end;
         end if;
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

   procedure Swap (Container : in out Array_Access; I, J : Index_Type) is
      pragma Check (Pre,
         Check =>
            (I in First_Index (Container) .. Last_Index (Container)
               and then J in First_Index (Container) .. Last_Index (Container))
            or else raise Constraint_Error);
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

   function First_Index (Container : Array_Access) return Index_Type is
   begin
      if Container = null then
         return Index_Type'First;
      else
         return Container'First;
      end if;
   end First_Index;

   function Last_Index (Container : Array_Access) return Extended_Index is
   begin
      if Container = null then
         return Index_Type'First - 1;
      else
         return Container'Last;
      end if;
   end Last_Index;

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
         Context : Context_Type :=
            (Container => Container);
      begin
         if Container /= null then
            Array_Sorting.In_Place_Reverse (
               Index_Type'Pos (Container'First),
               Index_Type'Pos (Container'Last),
               Context'Address,
               Swap => Swap'Access);
         end if;
      end Reverse_Elements;

      procedure Reverse_Rotate_Elements (
         Container : in out Array_Access;
         Before : Extended_Index)
      is
         pragma Check (Pre,
            Check =>
               Before in First_Index (Container) .. Last_Index (Container) + 1
               or else raise Constraint_Error);
         pragma Unmodified (Container);
         Context : Context_Type :=
            (Container => Container);
      begin
         if Container /= null then
            Array_Sorting.Reverse_Rotate (
               Index_Type'Pos (Container'First),
               Index_Type'Pos (Before),
               Index_Type'Pos (Container'Last),
               Context'Address,
               Swap => Swap'Access);
         end if;
      end Reverse_Rotate_Elements;

      procedure Juggling_Rotate_Elements (
         Container : in out Array_Access;
         Before : Extended_Index)
      is
         pragma Check (Pre,
            Check =>
               Before in First_Index (Container) .. Last_Index (Container) + 1
               or else raise Constraint_Error);
         pragma Unmodified (Container);
         Context : Context_Type :=
            (Container => Container);
      begin
         if Container /= null then
            Array_Sorting.Juggling_Rotate (
               Index_Type'Pos (Container'First),
               Index_Type'Pos (Before),
               Index_Type'Pos (Container'Last),
               Context'Address,
               Swap => Swap'Access);
         end if;
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
         Context : Context_Type :=
            (Container => Container);
      begin
         return Container = null
            or else Array_Sorting.Is_Sorted (
               Index_Type'Pos (Container'First),
               Index_Type'Pos (Container'Last),
               Context'Address,
               LT => LT'Access);
      end Is_Sorted;

      procedure Insertion_Sort (Container : in out Array_Access) is
         pragma Unmodified (Container);
         Context : Context_Type :=
            (Container => Container);
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
         Context : Context_Type :=
            (Container => Container);
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
         Source : in out Array_Access) is
      begin
         if Target = null then
            Move (Target, Source);
         else
            declare
               Before : constant Index_Type'Base := Target'Last + 1;
            begin
               Insert (Target, Before, Source);
               Free (Source);
               declare
                  Context : Context_Type := (Container => Target);
               begin
                  Array_Sorting.In_Place_Merge (
                     Index_Type'Pos (Target'First),
                     Index_Type'Pos (Before),
                     Index_Type'Pos (Target'Last),
                     Context'Address,
                     LT => LT'Access,
                     Swap => Swap'Access);
               end;
            end;
         end if;
      end Merge;

   end Generic_Sorting;

   procedure Assign (Target : in out Array_Access; Source : New_Array) is
   begin
      Free (Target);
      Target := Array_Access (Source);
   end Assign;

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
         Data : Array_Access;
         Last : Extended_Index;
      begin
         if Left = null then
            Data :=
               new Array_Type (
                  Index_Type'First ..
                  Index_Type'First + Index_Type'Base (Space));
            Last := Index_Type'First;
            Data (Last) := Right;
         else
            Data :=
               new Array_Type (
                  Left'First .. Left'Last + 1 + Index_Type'Base (Space));
            Last := Left'Last + 1;
            Data (Left'Range) := Left.all;
            Data (Last) := Right;
         end if;
         return (Data => Data, Last => Last);
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
         return New_Array_1
      is
         Data : Array_Access;
         Last : Extended_Index;
      begin
         if Left.Last + 1 <= Left.Data'Last then
            Data := Left.Data;
            Last := Left.Last + 1;
            Left.Data (Last) := Right;
         else
            Data := new Array_Type (
               Left.Data'First .. Left.Last + 1 + Index_Type'Base (Space));
            Last := Left.Last + 1;
            Data (Left.Data'Range) := Left.Data.all;
            Data (Last) := Right;
            declare
               Left_Data : Array_Access := Left.Data;
            begin
               Free (Left_Data);
            end;
         end if;
         return (Data => Data, Last => Last);
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
            declare
               Result : constant New_Array :=
                  new Array_Type (Data'First .. Data'Last + 1);
            begin
               Result (Data'Range) := Data.all;
               Result (Data'Last + 1) := Right;
               Free (Data);
               return Result;
            end;
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

end Ada.Containers.Generic_Array_Access_Types;
