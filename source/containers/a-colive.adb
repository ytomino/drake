pragma Check_Policy (Validate, Off);
with Ada.Containers.Array_Sorting;
--  diff (Ada.Unchecked_Conversion)
with Ada.Unchecked_Deallocation;
with System.Address_To_Named_Access_Conversions;
package body Ada.Containers.Limited_Vectors is
--  diff

   package Data_Cast is
      new System.Address_To_Named_Access_Conversions (Data, Data_Access);

--  diff (Upcast)
--
--  diff (Downcast)
--

   procedure Free is new Unchecked_Deallocation (Element_Type, Element_Access);
   procedure Free is new Unchecked_Deallocation (Data, Data_Access);

   procedure Swap_Element (I, J : Integer; Params : System.Address);
   procedure Swap_Element (I, J : Integer; Params : System.Address) is
      Data : constant Data_Access := Data_Cast.To_Pointer (Params);
      Temp : constant Element_Access := Data.Items (Index_Type'Val (I));
   begin
      Data.Items (Index_Type'Val (I)) := Data.Items (Index_Type'Val (J));
      Data.Items (Index_Type'Val (J)) := Temp;
   end Swap_Element;

--  diff (Equivalent_Element)
--
--
--
--
--
--

--  diff (Allocate_Element)
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
      if Data /= null then
         for I in Data.Items'Range loop
            Free (Data.Items (I));
         end loop;
         Free (Data);
      end if;
   end Release;

--  diff (Allocate_Data)
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

--  diff (Move_Data)
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

--  diff (Copy_Data)
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

--  diff (Unique)
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

   --  implementation

--  diff (Assign)
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

   function Capacity (Container : Vector) return Count_Type is
   begin
      if Container.Data = null then
         return 0;
      else
         return Container.Data.Items'Length;
      end if;
   end Capacity;

   procedure Clear (Container : in out Vector) is
   begin
      Release (Container.Data);
--  diff
      Container.Data := null;
      Container.Length := 0;
   end Clear;

   function Constant_Reference (
      Container : aliased Vector;
      Index : Index_Type)
      return Constant_Reference_Type is
   begin
--  diff
      declare
         Data : constant Data_Access := Container.Data;
      begin
--  diff
         return (Element => Data.Items (Index).all'Access);
      end;
   end Constant_Reference;

--  diff (Constant_Reference)
--
--
--
--
--
--
--
--

--  diff (Constant_Reference)
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
               (Index_Type'First + Index_Type'Base (Old_Length))
               - (Index + Index_Type'Base (Count))
               - 1;
            Before : constant Index_Type := Index + Index_Type'Base (Count);
            After : constant Index_Type := Index;
         begin
            Container.Length := Container.Length - Count;
            for I in After .. After + Index_Type'Base (Count) - 1 loop
               Free (Container.Data.Items (I));
            end loop;
            Container.Data.Items (After .. After + Moving) :=
               Container.Data.Items (Before .. Before + Moving);
--  diff
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
         Data => null,
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
--

   function First (Container : Vector) return Cursor is
   begin
      if Container.Length = 0 then
         return No_Element;
      else
         return Index_Type'First;
      end if;
   end First;

   function First_Index (Container : Vector) return Index_Type is
      pragma Unreferenced (Container);
   begin
      return Index_Type'First;
   end First_Index;

--  diff (Generic_Array_To_Vector)
--
--
--

   function Has_Element (Position : Cursor) return Boolean is
   begin
      return Position > No_Index;
   end Has_Element;

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
      New_Item : not null access function return Element_Type;
      Position : out Cursor;
      Count : Count_Type := 1) is
   begin
      Insert_Space (Container, Before, Position, Count);
      for I in Position .. Position + Index_Type'Base (Count) - 1 loop
         declare
            E : Element_Access
               renames Container.Data.Items (I);
         begin
            pragma Check (Validate, E = null);
            E := new Element_Type'(New_Item.all);
         end;
      end loop;
   end Insert;

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
      if Position = Index_Type'First + Index_Type'Base (Container.Length) then
         Set_Length (Container, Container.Length + Count);
      else
         declare
            Old_Length : constant Count_Type := Container.Length;
            Moving : constant Index_Type'Base :=
               Last_Index (Container) - Position;
            After : constant Index_Type := Position + Index_Type'Base (Count);
         begin
            Set_Length (Container, Old_Length + Count);
--  diff
            for I in
               Index_Type'First + Index_Type'Base (Old_Length) ..
               Last_Index (Container)
            loop
               Free (Container.Data.Items (I));
            end loop;
            Container.Data.Items (After .. After + Moving) :=
               Container.Data.Items (Position .. Position + Moving);
--  diff
            for I in Position .. After - 1 loop
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
      Container : Vector'Class;
      Process : not null access procedure (Position : Cursor)) is
   begin
      for I in Index_Type'First .. Last_Index (Container) loop
         Process (I);
      end loop;
   end Iterate;

   function Iterate (Container : Vector)
      return Vector_Iterator_Interfaces.Reversible_Iterator'Class is
   begin
      return Vector_Iterator'(
         First => First (Container),
         Last => Last (Container));
   end Iterate;

   function Iterate (Container : Vector; First, Last : Cursor)
      return Vector_Iterator_Interfaces.Reversible_Iterator'Class
   is
      pragma Unreferenced (Container);
   begin
      if First > Last then
         return Vector_Iterator'(First => No_Element, Last => No_Element);
      else
         return Vector_Iterator'(First => First, Last => Last);
      end if;
   end Iterate;

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
      Release (Target.Data);
      Target.Data := Source.Data;
      Target.Length := Source.Length;
      Source.Data := null;
      Source.Length := 0;
   end Move;

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

   procedure Query_Element (
      Container : Vector'Class;
      Index : Index_Type;
      Process : not null access procedure (Element : Element_Type)) is
   begin
      Process (Container.Constant_Reference (Index).Element.all);
   end Query_Element;

   function Reference (
      Container : aliased in out Vector;
      Index : Index_Type)
      return Reference_Type is
   begin
--  diff
      declare
         Data : constant Data_Access := Container.Data;
      begin
--  diff
         return (Element => Data.Items (Index).all'Access);
      end;
   end Reference;

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
--
--
--
--
--

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

   procedure Reserve_Capacity (
      Container : in out Vector;
      Capacity : Count_Type) is
   begin
      if Capacity /= Limited_Vectors.Capacity (Container) then
         declare
            New_Capacity : constant Count_Type := Count_Type'Max (
               Capacity, Container.Length);
            Old_Data : Data_Access := Container.Data;
         begin
            if New_Capacity = 0 then
               Container.Data := null;
            else
               Container.Data := new Data'(
                  Capacity_Last =>
                     Index_Type'First - 1 + Index_Type'Base (New_Capacity),
                  Items => <>);
               for I in Index_Type'First .. Last_Index (Container) loop
                  Container.Data.Items (I) := Old_Data.Items (I);
                  Old_Data.Items (I) := null;
               end loop;
            end if;
            Release (Old_Data);
         end;
      end if;
   end Reserve_Capacity;

   procedure Reverse_Elements (Container : in out Vector) is
   begin
--  diff
      Array_Sorting.In_Place_Reverse (
         Index_Type'Pos (Index_Type'First),
         Index_Type'Pos (Last_Index (Container)),
         Data_Cast.To_Address (Container.Data),
         Swap => Swap_Element'Access);
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
--

   procedure Reverse_Iterate (
      Container : Vector'Class;
      Process : not null access procedure (Position : Cursor)) is
   begin
      for I in reverse Index_Type'First .. Last_Index (Container) loop
         Process (I);
      end loop;
   end Reverse_Iterate;

   procedure Set_Length (Container : in out Vector; Length : Count_Type) is
      Old_Capacity : constant Count_Type := Capacity (Container);
   begin
      if Length > Old_Capacity then
         declare
            New_Capacity : constant Count_Type :=
               Count_Type'Max (Old_Capacity * 2, Length);
         begin
            Reserve_Capacity (Container, New_Capacity);
         end;
      end if;
      Container.Length := Length;
   end Set_Length;

   procedure Swap (Container : in out Vector; I, J : Index_Type) is
   begin
--  diff
      Swap_Element (
         Index_Type'Pos (I),
         Index_Type'Pos (J),
         Data_Cast.To_Address (Container.Data));
   end Swap;

   function To_Cursor (Container : Vector; Index : Extended_Index)
      return Cursor
   is
      pragma Unreferenced (Container);
   begin
      return Cursor (Index);
   end To_Cursor;

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
      Container : in out Vector'Class;
      Index : Index_Type;
      Process : not null access procedure (Element : in out Element_Type)) is
   begin
      Process (Container.Reference (Index).Element.all);
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

--  diff (Adjust)
--
--
--

   overriding function First (Object : Vector_Iterator) return Cursor is
   begin
      return Object.First;
   end First;

   overriding function Next (Object : Vector_Iterator; Position : Cursor)
      return Cursor is
   begin
      if Position >= Object.Last then
         return No_Element;
      else
         return Position + 1;
      end if;
   end Next;

   overriding function Last (Object : Vector_Iterator) return Cursor is
   begin
      return Object.Last;
   end Last;

   overriding function Previous (Object : Vector_Iterator; Position : Cursor)
      return Cursor is
   begin
      if Position <= Object.First then
         return No_Element;
      else
         return Position - 1;
      end if;
   end Previous;

   package body Generic_Sorting is

      function LT (Left, Right : Integer; Params : System.Address)
         return Boolean;
      function LT (Left, Right : Integer; Params : System.Address)
         return Boolean
      is
         Data : constant Data_Access := Data_Cast.To_Pointer (Params);
      begin
         return Data.Items (Index_Type'Val (Left)).all <
            Data.Items (Index_Type'Val (Right)).all;
      end LT;

      function Is_Sorted (Container : Vector) return Boolean is
      begin
         return Array_Sorting.Is_Sorted (
            Index_Type'Pos (Index_Type'First),
            Index_Type'Pos (Last_Index (Container)),
            Data_Cast.To_Address (Container.Data),
            LT => LT'Access);
      end Is_Sorted;

      procedure Sort (Container : in out Vector) is
      begin
--  diff
         Array_Sorting.In_Place_Merge_Sort (
            Index_Type'Pos (Index_Type'First),
            Index_Type'Pos (Last_Index (Container)),
            Data_Cast.To_Address (Container.Data),
            LT => LT'Access,
            Swap => Swap_Element'Access);
      end Sort;

      procedure Merge (Target : in out Vector; Source : in out Vector) is
      begin
         if Source.Length > 0 then
            declare
               Old_Length : constant Count_Type := Target.Length;
            begin
               if Old_Length = 0 then
                  Move (Target, Source);
               else
                  Set_Length (Target, Old_Length + Source.Length);
--  diff
--  diff
                  for I in Index_Type'First .. Last_Index (Source) loop
                     Target.Data.Items
                        (I + Index_Type'Base (Old_Length)) :=
                        Source.Data.Items (I);
                     Source.Data.Items (I) := null;
                  end loop;
                  Source.Length := 0;
                  Array_Sorting.In_Place_Merge (
                     Index_Type'Pos (Index_Type'First),
                     Integer (Index_Type'First) - 1 + Integer (Old_Length),
                     Index_Type'Pos (Last_Index (Target)),
                     Data_Cast.To_Address (Target.Data),
                     LT => LT'Access,
                     Swap => Swap_Element'Access);
               end if;
            end;
         end if;
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
         return No_Element;
      end Find;

      function Reverse_Find (Container : Vector; Item : Element_Type)
         return Cursor is
      begin
         return Reverse_Find (Container, Item, Last_Index (Container));
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
         return No_Element;
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
