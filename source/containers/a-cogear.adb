with Ada.Containers.Inside.Array_Sorting;
package body Ada.Containers.Generic_Arrays is

   procedure Append (Container : in out Array_Access;
                     New_Item  : Array_Type) is
   begin
      Insert (Container, Container'Last + 1, New_Item);
   end Append;

   procedure Append (Container : in out Array_Access;
                     New_Item  : Array_Access) is
   begin
      Insert (Container, Container'Last + 1, New_Item);
   end Append;

   procedure Append (Container : in out Array_Access;
                     New_Item  : Element_Type;
                     Count     : Count_Type := 1) is
   begin
      Insert (Container, Container'Last + 1, New_Item, Count);
   end Append;

   procedure Append (Container : in out Array_Access;
                     Count     : Count_Type := 1) is
   begin
      Insert (Container, Container'Last + 1, Count);
   end Append;

   procedure Delete (Container : in out Array_Access;
                     Index     : Extended_Index;
                     Count     : Count_Type := 1) is
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
                  Container (Index + Index_Type'Base (Count) ..
                             Container'Last));
            elsif Index + Index_Type'Base (Count) - 1 = Container'Last then
               Container := new Array_Type'(
                  Container (Container'First ..
                             Index_Type'Base'Pred (Index)));
            else
               Container := new Array_Type'(
                  Container (Container'First ..
                             Index_Type'Base'Pred (Index)) &
                  Container (Index + Index_Type'Base (Count) ..
                             Container'Last));
            end if;
            Free (S);
         end;
      end if;
   end Delete;

   procedure Delete_First (Container : in out Array_Access;
                           Count     : Count_Type := 1) is
   begin
      Delete (Container, Container'First, Count);
   end Delete_First;

   procedure Delete_Last (Container : in out Array_Access;
                          Count     : Count_Type := 1) is
   begin
      Delete (Container, Container'Last - Index_Type'Base (Count) + 1, Count);
   end Delete_Last;

   procedure Insert (Container : in out Array_Access;
                     Before    : Extended_Index;
                     New_Item  : Array_Type) is
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
                  New_Item &
                  S (Before .. S'Last));
            elsif Before > Container'Last then
               Container := new Array_Type'(
                  S (S'First .. Before - 1) &
                  New_Item);
            else
               Container := new Array_Type'(
                  S (S'First .. Before - 1) &
                  New_Item &
                  S (Before .. S'Last));
            end if;
            Free (S);
         end;
      end if;
   end Insert;

   procedure Insert (Container : in out Array_Access;
                     Before    : Extended_Index;
                     New_Item  : Array_Access) is
   begin
      if New_Item /= null then
         Insert (Container, Before, New_Item.all);
      end if;
   end Insert;

   procedure Insert (Container : in out Array_Access;
                     Before    : Extended_Index;
                     New_Item  : Element_Type;
                     Count     : Count_Type := 1) is
   begin
      Insert (Container, Before, Count);
      for I in Before .. Before + Index_Type'Base (Count) - 1 loop
         Container (I) := New_Item;
      end loop;
   end Insert;

   procedure Insert (Container : in out Array_Access;
                     Before    : Extended_Index;
                     Count     : Count_Type := 1) is
   begin
      if Count = 0 then
         null;
      elsif Container = null then
         Container := new Array_Type (Before ..
                                      Before + Index_Type'Base (Count) - 1);
      else
         declare
            S : Array_Access := Container;
         begin
            if Before = Container'First then
               Container := new Array_Type'(
                  Array_Type'(
                     Index_Type'First ..
                     Index_Type'First + Index_Type'Base (Count) - 1 => <>) &
                  S (Before .. S'Last));
            elsif Before > Container'Last then
               Container := new Array_Type'(
                  S (S'First .. Before - 1) &
                  Array_Type'(
                     Index_Type'First ..
                     Index_Type'First + Index_Type'Base (Count) - 1 => <>));
            else
               Container := new Array_Type'(
                  S (S'First .. Before - 1) &
                  Array_Type'(
                     Index_Type'First ..
                     Index_Type'First + Index_Type'Base (Count) - 1 => <>) &
                  S (Before .. S'Last));
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

   procedure Move (Target : in out Array_Access;
                   Source : in out Array_Access) is
   begin
      if Target /= Source then
         Free (Target);
         Target := Source;
         Source := null;
      end if;
   end Move;

   procedure Prepend (Container : in out Array_Access;
                      New_Item  : Array_Type) is
   begin
      Insert (Container, Container'First, New_Item);
   end Prepend;

   procedure Prepend (Container : in out Array_Access;
                      New_Item  : Array_Access) is
   begin
      Insert (Container, Container'First, New_Item);
   end Prepend;

   procedure Prepend (Container : in out Array_Access;
                      New_Item  : Element_Type;
                      Count     : Count_Type := 1) is
   begin
      Insert (Container, Container'First, New_Item, Count);
   end Prepend;

   procedure Prepend (Container : in out Array_Access;
                      Count     : Count_Type := 1) is
   begin
      Insert (Container, Container'First, Count);
   end Prepend;

   procedure Replace (Target : in out Array_Access; Source : Array_Access) is
   begin
      if Target /= Source then
         Free (Target);
         if Source /= null and then Source'Length > 0 then
            Target := new Array_Type'(Source.all);
         end if;
      end if;
   end Replace;

   procedure Replace (Target : in out Array_Access; Source : New_Array) is
   begin
      Free (Target);
      Target := Array_Access (Source);
   end Replace;

   procedure Reverse_Elements (Container : in out Array_Access) is
      procedure Swap (I, J : Integer);
      procedure Swap (I, J : Integer) is
      begin
         Swap (Container, Index_Type'Val (I), Index_Type'Val (J));
      end Swap;
   begin
      Inside.Array_Sorting.In_Place_Reverse (
         Index_Type'Pos (Container'First),
         Index_Type'Pos (Container'Last),
         Swap => Swap'Access);
   end Reverse_Elements;

   procedure Set_Length (Container : in out Array_Access;
                         Length    : Count_Type) is
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
            Container := new Array_Type'(Container (
               Container'First ..
               Container'First + Index_Type'Base (Length) - 1));
            Free (S);
         end;
      elsif Length > Container'Length then
         declare
            S : Array_Access := Container;
         begin
            Container := new Array_Type'(Container.all & Array_Type'(
               Index_Type'First ..
               Index_Type'First +
               Index_Type'Base (Length - Container'Length) - 1 => <>));
            Free (S);
         end;
      end if;
   end Set_Length;

   procedure Swap (Container : in out Array_Access; I, J : Index_Type) is
   begin
      if I /= J then
         declare
            Temp : constant Element_Type := Container (I);
         begin
            Container (I) := Container (J);
            Container (J) := Temp;
         end;
      end if;
   end Swap;

   function "&" (Left : Array_Access; Right : Element_Type) return New_Array is
   begin
      if Left = null then
         return new Array_Type'(Index_Type'First => Right);
      else
         return new Array_Type'(Left.all & Right);
      end if;
   end "&";

   package body Generic_Sorting is

      function Is_Sorted (Container : Array_Access) return Boolean is
         function LT (Left, Right : Integer) return Boolean;
         function LT (Left, Right : Integer) return Boolean is
         begin
            return Container (Index_Type'Val (Left)) <
                   Container (Index_Type'Val (Right));
         end LT;
      begin
         return Container = null or else
            Inside.Array_Sorting.Is_Sorted (
               Index_Type'Pos (Container'First),
               Index_Type'Pos (Container'Last),
               LT => LT'Access);
      end Is_Sorted;

      procedure Sort (Container : in out Array_Access) is
         function LT (Left, Right : Integer) return Boolean;
         function LT (Left, Right : Integer) return Boolean is
         begin
            return Container (Index_Type'Val (Left)) <
                   Container (Index_Type'Val (Right));
         end LT;
         procedure Swap (I, J : Integer);
         procedure Swap (I, J : Integer) is
         begin
            Swap (Container, Index_Type'Val (I), Index_Type'Val (J));
         end Swap;
      begin
         if Container /= null then
            Inside.Array_Sorting.In_Place_Merge_Sort (
               Index_Type'Pos (Container'First),
               Index_Type'Pos (Container'Last),
               LT => LT'Access,
               Swap => Swap'Access);
         end if;
      end Sort;

      procedure Merge (Target : in out Array_Access;
                       Source : in out Array_Access)
      is
         function LT (Left, Right : Integer) return Boolean;
         function LT (Left, Right : Integer) return Boolean is
         begin
            return Target (Index_Type'Val (Left)) <
                   Target (Index_Type'Val (Right));
         end LT;
         procedure Swap (I, J : Integer);
         procedure Swap (I, J : Integer) is
         begin
            Swap (Target, Index_Type'Val (I), Index_Type'Val (J));
         end Swap;
      begin
         if Target = null then
            Move (Target, Source);
         else
            declare
               Old_Length : constant Count_Type := Target'Length;
            begin
               Append (Target, Source);
               Free (Source);
               Inside.Array_Sorting.In_Place_Merge (
                  Index_Type'Pos (Target'First),
                  Integer (Target'First + Index_Type'Base (Old_Length) - 1),
                  Index_Type'Pos (Target'Last),
                  LT => LT'Access,
                  Swap => Swap'Access);
            end;
         end if;
      end Merge;

   end Generic_Sorting;

end Ada.Containers.Generic_Arrays;
