with Ada.Unchecked_Deallocation;
with Ada.Containers.Inside.Array_Sorting;
package body Ada.Containers.Limited_Vectors is
--  diff

   procedure Free is new Unchecked_Deallocation (Element_Type, Element_Access);
   procedure Free is new Unchecked_Deallocation (Data, Data_Access);

--  diff (Replace_Element)
--
--
--
--
--
--
--
--
--
--
--
--
--

   procedure Release (Data : in out Data_Access);
   procedure Release (Data : in out Data_Access) is
   begin
      if Data /= Empty_Data'Unrestricted_Access then
--  diff
--  diff
--  diff
--  diff
         for I in Data.Items'Range loop
            Free (Data.Items (I));
         end loop;
         Free (Data);
--  diff
      end if;
   end Release;

--  diff (Unique)
--
--
--
--

--  diff (Array_To_Vector)
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--

--  diff (Adjust)
--
--
--
--
--

--  diff (Assign)
--
--
--
--
--
--
--
--

--  diff (Append)
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--

--  diff (Append)
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--

   function Capacity (Container : Vector) return Count_Type is
   begin
      return Container.Data.Items'Length;
   end Capacity;

   procedure Clear (Container : in out Vector) is
   begin
      Release (Container.Data);
      Container.Data := Empty_Data'Unrestricted_Access;
      Container.Length := 0;
   end Clear;

   function Constant_Reference (
      Container : not null access constant Vector;
      Index : Index_Type)
      return Constant_Reference_Type is
   begin
      return (Element => Container.Data.Items (Index).all'Access);
   end Constant_Reference;

--  diff (Contains)
--
--
--
--

--  diff (Copy)
--
--
--
--
--

   procedure Delete (
      Container : in out Vector;
      Index : Extended_Index;
      Count : Count_Type := 1) is
   begin
      if Index + Index_Type'Base (Count) =
         Index_Type'First + Index_Type'Base (Container.Length)
      then
         Delete_Last (Container, Count);
      else
--  diff
         declare
            Old_Length : constant Count_Type := Container.Length;
            Moving : constant Index_Type'Base :=
               (Index_Type'First + Index_Type'Base (Old_Length)) -
               (Index + Index_Type'Base (Count)) - 1;
            Before : constant Index_Type := Index + Index_Type'Base (Count);
            After : constant Index_Type := Index;
         begin
            Container.Length := Container.Length - Count;
            for I in After .. After + Index_Type'Base (Count) - 1 loop
               Free (Container.Data.Items (I));
            end loop;
            Container.Data.Items (After .. After + Moving) :=
               Container.Data.Items (Before .. Before + Moving);
            for I in
               Index_Type'First + Index_Type'Base (Container.Length) ..
               Index_Type'First - 1 + Index_Type'Base (Old_Length)
            loop
               Container.Data.Items (I) := null;
            end loop;
         end;
      end if;
   end Delete;

   procedure Delete_First (
      Container : in out Vector;
      Count : Count_Type := 1) is
   begin
      Delete (Container, Index_Type'First, Count => Count);
   end Delete_First;

   procedure Delete_Last (
      Container : in out Vector;
      Count : Count_Type := 1) is
   begin
      Container.Length := Container.Length - Count;
   end Delete_Last;

--  diff (Element)
--
--
--
--

   function Empty_Vector return Vector is
   begin
      return (Finalization.Limited_Controlled with
         Data => Empty_Data'Unrestricted_Access,
         Length => 0);
   end Empty_Vector;

--  diff (Find)
--
--
--

--  diff (Find)
--
--
--
--
--
--
--
--
--
--
--
--

--  diff (Find_Index)
--
--
--
--
--
--
--
--
--
--
--
--
--
--

--  diff (First_Element)
--
--
--

   function First_Index (Container : Vector) return Index_Type is
      pragma Unreferenced (Container);
   begin
      return Index_Type'First;
   end First_Index;

--  diff (Generic_Array_To_Vector)
--
--
--

--  diff (Insert)
--
--
--
--
--
--
--
--

--  diff (Insert)
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--

--  diff (Insert)
--
--
--
--
--
--
--
--
--

   procedure Insert (
      Container : in out Vector;
      Before : Cursor;
      New_Item : not null access function (C : Vector) return Element_Type;
      Position : out Cursor;
      Count : Count_Type := 1) is
   begin
      Insert_Space (Container, Before, Position, Count);
      for I in Before .. Before + Index_Type'Base (Count) - 1 loop
         pragma Assert (Container.Data.Items (I) = null);
         Container.Data.Items (I) := new Element_Type'(New_Item (Container));
--  diff
--  diff
      end loop;
   end Insert;

--  diff (Insert)
--
--
--
--
--
--
--
--

--  diff (Insert)
--
--
--
--
--
--
--

   procedure Insert_Space (
      Container : in out Vector;
      Before : Extended_Index;
      Count : Count_Type := 1)
   is
      Position : Cursor;
   begin
      Insert_Space (Container, Before, Position, Count);
   end Insert_Space;

   procedure Insert_Space (
      Container : in out Vector;
      Before : Cursor;
      Position : out Cursor;
      Count : Count_Type := 1) is
   begin
      Position := Before;
      if Before = Index_Type'First + Index_Type'Base (Container.Length) then
         Set_Length (Container, Container.Length + Count);
      else
         declare
            Old_Length : constant Count_Type := Container.Length;
            Moving : constant Index_Type'Base :=
               Last_Index (Container) - Before;
            After : constant Index_Type := Before + Index_Type'Base (Count);
         begin
            Set_Length (Container, Old_Length + Count);
            for I in
               Index_Type'First + Index_Type'Base (Old_Length) ..
               Last_Index (Container)
            loop
               Free (Container.Data.Items (I));
            end loop;
            Container.Data.Items (After .. After + Moving) :=
               Container.Data.Items (Before .. Before + Moving);
            for I in Before .. After - 1 loop
               Container.Data.Items (I) := null;
            end loop;
         end;
      end if;
   end Insert_Space;

   function Is_Empty (Container : Vector) return Boolean is
   begin
      return Container.Length = 0;
   end Is_Empty;

   procedure Iterate (
      Container : Vector;
      Process : not null access procedure (Position : Cursor)) is
   begin
      for I in Index_Type'First .. Last_Index (Container) loop
         Process (I);
      end loop;
   end Iterate;

--  diff (Last_Element)
--
--
--

   function Last_Index (Container : Vector) return Extended_Index is
   begin
      return Index_Type'First - 1 + Index_Type'Base (Container.Length);
   end Last_Index;

   function Length (Container : Vector) return Count_Type is
   begin
      return Container.Length;
   end Length;

   procedure Move (Target : in out Vector; Source : in out Vector) is
   begin
      Clear (Target);
      Target.Data := Source.Data;
      Target.Length := Source.Length;
      Source.Data := Empty_Data'Unrestricted_Access;
      Source.Length := 0;
   end Move;

   function Next (Position : Cursor) return Cursor is
   begin
      return Position + 1;
   end Next;

   procedure Next (Position : in out Cursor) is
   begin
      Position := Position + 1;
   end Next;

--  diff (Prepend)
--
--
--

--  diff (Prepend)
--
--
--
--
--
--

   function Previous (Position : Cursor) return Cursor is
   begin
      return Position - 1;
   end Previous;

   procedure Previous (Position : in out Cursor) is
   begin
      Position := Position - 1;
   end Previous;

   procedure Query_Element (
      Container : Vector;
      Index : Index_Type;
      Process  : not null access procedure (Element : Element_Type)) is
   begin
      Process (Container.Data.Items (Index).all);
   end Query_Element;

   function Reference (
      Container : not null access Vector;
      Index : Index_Type)
      return Reference_Type is
   begin
--  diff
      return (Element => Container.Data.Items (Index).all'Access);
   end Reference;

--  diff (Reference)
--
--
--
--
--
--
--

--  diff (Reference)
--
--
--
--
--
--
--
--
--
--
--

   procedure Replace_Element (
      Container : in out Vector;
      Position  : Cursor;
      New_Item : not null access function (C : Vector) return Element_Type) is
   begin
      Free (Container.Data.Items (Position));
      Container.Data.Items (Position) :=
         new Element_Type'(New_Item (Container));
--  diff
--  diff
   end Replace_Element;

   procedure Reserve_Capacity (
      Container : in out Vector;
      Capacity : Count_Type) is
   begin
      if Capacity /= Limited_Vectors.Capacity (Container) then
--  diff
--  diff
--  diff
         declare
            New_Capacity : constant Count_Type := Count_Type'Max (
               Capacity,
               Container.Length);
            Old_Data : Data_Access := Container.Data;
         begin
            if New_Capacity = 0 then
               Container.Data := Empty_Data'Unrestricted_Access;
            else
               declare
                  Capacity_Last : constant Extended_Index :=
                     Index_Type'First - 1 + Index_Type'Base (New_Capacity);
               begin
                  Container.Data := new Data'(
                     Capacity_Last => Capacity_Last,
--  diff
--  diff
                     Items => <>);
                  declare
                     Last : constant Extended_Index := Last_Index (Container);
                     subtype R is Index_Type range Index_Type'First .. Last;
                  begin
                     for I in R loop
--  diff
--  diff
--  diff
--  diff
--  diff
--  diff
                        Container.Data.Items (I) := Old_Data.Items (I);
                        Old_Data.Items (I) := null;
--  diff
                     end loop;
                  end;
               end;
            end if;
            Release (Old_Data);
         end;
      end if;
   end Reserve_Capacity;

   procedure Reverse_Elements (Container : in out Vector) is
      procedure Swap (I, J : Integer);
      procedure Swap (I, J : Integer) is
      begin
         Swap (Container, Index_Type'Val (I), Index_Type'Val (J));
      end Swap;
   begin
--  diff
      Inside.Array_Sorting.In_Place_Reverse (Index_Type'Pos (Index_Type'First),
         Index_Type'Pos (Last_Index (Container)),
         Swap => Swap'Access);
   end Reverse_Elements;

--  diff (Reverse_Find)
--
--
--
--

--  diff (Reverse_Find)
--
--
--
--
--
--
--

--  diff (Reverse_Find_Index)
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--

   procedure Reverse_Iterate (
      Container : Vector;
      Process : not null access procedure (Position : Cursor)) is
   begin
      for I in reverse Index_Type'First .. Last_Index (Container) loop
         Process (I);
      end loop;
   end Reverse_Iterate;

   procedure Set_Length (Container : in out Vector; Length : Count_Type) is
--  diff
--  diff
--  diff
      Old_Capacity : constant Count_Type := Capacity (Container);
   begin
      if Length > Old_Capacity then
         declare
            New_Capacity : constant Count_Type :=
               Count_Type'Max (Old_Capacity * 2, Length);
         begin
            Reserve_Capacity (Container, New_Capacity);
--  diff
         end;
--  diff
--  diff
--  diff
--  diff
--  diff
--  diff
--  diff
--  diff
--  diff
--  diff
--  diff
      end if;
--  diff
--  diff
      Container.Length := Length;
   end Set_Length;

   procedure Swap (Container : in out Vector; I, J : Index_Type) is
   begin
--  diff
      declare
         Temp : constant Element_Access := Container.Data.Items (I);
      begin
         Container.Data.Items (I) := Container.Data.Items (J);
         Container.Data.Items (J) := Temp;
      end;
   end Swap;

   function To_Cursor (Container : Vector; Index : Extended_Index)
      return Cursor
   is
      pragma Unreferenced (Container);
   begin
      return Cursor (Index);
   end To_Cursor;

   function To_Index (Position : Cursor) return Extended_Index is
   begin
      return Position;
   end To_Index;

   function To_Vector (Length : Count_Type) return Vector is
   begin
      return Result : Vector do
         Insert_Space (Result, Index_Type'First, Length);
      end return;
   end To_Vector;

--  diff (To_Vector)
--
--
--
--
--
--

   procedure Update_Element (
      Container : in out Vector;
      Index : Index_Type;
      Process : not null access procedure (Element : in out Element_Type)) is
   begin
--  diff
      Process (Container.Data.Items (Index).all);
   end Update_Element;

--  diff ("=")
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--

--  diff ("&")
--
--
--
--
--

--  diff ("&")
--
--
--
--
--

--  diff ("&")
--
--
--
--
--
--
--

--  diff ("&")
--
--
--
--
--
--
--

   package body Generic_Sorting is

      function Is_Sorted (Container : Vector) return Boolean is
         function LT (Left, Right : Integer) return Boolean;
         function LT (Left, Right : Integer) return Boolean is
         begin
            return Container.Data.Items (Index_Type'Val (Left)).all <
               Container.Data.Items (Index_Type'Val (Right)).all;
         end LT;
      begin
         return Inside.Array_Sorting.Is_Sorted (
            Index_Type'Pos (Index_Type'First),
            Index_Type'Pos (Last_Index (Container)),
            LT => LT'Access);
      end Is_Sorted;

      procedure Sort (Container : in out Vector) is
         function LT (Left, Right : Integer) return Boolean;
         function LT (Left, Right : Integer) return Boolean is
         begin
            return Container.Data.Items (Index_Type'Val (Left)).all <
               Container.Data.Items (Index_Type'Val (Right)).all;
         end LT;
         procedure Swap (I, J : Integer);
         procedure Swap (I, J : Integer) is
         begin
            Swap (Container, Index_Type'Val (I), Index_Type'Val (J));
         end Swap;
      begin
--  diff
         Inside.Array_Sorting.In_Place_Merge_Sort (
            Index_Type'Pos (Index_Type'First),
            Index_Type'Pos (Last_Index (Container)),
            LT => LT'Access,
            Swap => Swap'Access);
      end Sort;

      procedure Merge (Target : in out Vector; Source : in out Vector) is
         function LT (Left, Right : Integer) return Boolean;
         function LT (Left, Right : Integer) return Boolean is
         begin
            return Target.Data.Items (Index_Type'Val (Left)).all <
               Target.Data.Items (Index_Type'Val (Right)).all;
         end LT;
         procedure Swap (I, J : Integer);
         procedure Swap (I, J : Integer) is
         begin
            Swap (Target, Index_Type'Val (I), Index_Type'Val (J));
         end Swap;
         Old_Length : constant Count_Type := Target.Length;
      begin
         if Old_Length = 0 then
            Move (Target, Source);
         else
            Set_Length (Target, Old_Length + Source.Length);
            for I in Index_Type'First .. Last_Index (Source) loop
               Target.Data.Items (I + Index_Type'Base (Old_Length)) :=
                  Source.Data.Items (I);
               Source.Data.Items (I) := null;
            end loop;
            Source.Length := 0;
         end if;
--  diff
         Inside.Array_Sorting.In_Place_Merge (
            Index_Type'Pos (Index_Type'First),
            Integer (Index_Type'First) - 1 + Integer (Old_Length),
            Index_Type'Pos (Last_Index (Target)),
            LT => LT'Access,
            Swap => Swap'Access);
      end Merge;

   end Generic_Sorting;

   package body Equivalents is

      function Contains (Container : Vector; Item : Element_Type)
         return Boolean is
      begin
         return Reverse_Find (Container, Item) >= Index_Type'First;
      end Contains;

      function Find (Container : Vector; Item : Element_Type) return Cursor is
      begin
         return Find (Container, Item, Index_Type'First);
      end Find;

      function Find (
         Container : Vector;
         Item : Element_Type;
         Position : Cursor) return Cursor is
      begin
         for I in Position .. Last_Index (Container) loop
            if Container.Data.Items (I) /= null
               and then Container.Data.Items (I).all = Item
            then
               return I;
            end if;
         end loop;
         return Cursor'Last; --  Find (...) <= Last
      end Find;

      function Reverse_Find (Container : Vector; Item : Element_Type)
         return Cursor is
      begin
         return Reverse_Find (Container, Item, Last (Container));
      end Reverse_Find;

      function Reverse_Find (
         Container : Vector;
         Item : Element_Type;
         Position : Cursor)
         return Cursor is
      begin
         for I in reverse Index_Type'First .. Position loop
            if Container.Data.Items (I) /= null
               and then Container.Data.Items (I).all = Item
            then
               return I;
            end if;
         end loop;
         return Cursor'First;
      end Reverse_Find;

      function "=" (Left, Right : Vector) return Boolean is
      begin
         if Left.Length /= Right.Length then
            return False;
         else
            for I in Index_Type'First .. Last_Index (Left) loop
               if (Left.Data.Items (I) /= null) /=
                  (Right.Data.Items (I) /= null)
               then
                  return False;
               elsif Left.Data.Items (I) /= null
                  and then Left.Data.Items (I).all /= Right.Data.Items (I).all
               then
                  return False;
               end if;
            end loop;
            return True;
         end if;
      end "=";

   end Equivalents;

end Ada.Containers.Limited_Vectors;
