with Ada.Containers.Array_Sorting;
--  diff (Ada.Unchecked_Conversion)
with Ada.Unchecked_Deallocation;
with System.Address_To_Named_Access_Conversions;
with System.Long_Long_Integer_Types;
package body Ada.Containers.Limited_Vectors is
   pragma Check_Policy (Validate => Ignore);
--  diff
   use type System.Address;
   use type System.Long_Long_Integer_Types.Word_Integer;

   subtype Word_Integer is System.Long_Long_Integer_Types.Word_Integer;

   package DA_Conv is
      new System.Address_To_Named_Access_Conversions (Data, Data_Access);

--  diff (Upcast)
--
--  diff (Downcast)
--

   procedure Free is new Unchecked_Deallocation (Element_Type, Element_Access);
   procedure Free is new Unchecked_Deallocation (Data, Data_Access);

--  diff (Assign_Element)
--
--
--
--
--
--
--
--

   procedure Swap_Element (I, J : Word_Integer; Params : System.Address);
   procedure Swap_Element (I, J : Word_Integer; Params : System.Address) is
      Data : constant Data_Access := DA_Conv.To_Pointer (Params);
      Temp : constant Element_Access := Data.Items (Index_Type'Val (I));
   begin
      Data.Items (Index_Type'Val (I)) := Data.Items (Index_Type'Val (J));
--  diff
--  diff
      Data.Items (Index_Type'Val (J)) := Temp;
   end Swap_Element;

--  diff (Equivalent_Element)
--
--
--
--
--
--

   procedure Allocate_Element (
      Item : out Element_Access;
      New_Item : not null access function return Element_Type);
   procedure Allocate_Element (
      Item : out Element_Access;
      New_Item : not null access function return Element_Type) is
   begin
      Item := new Element_Type'(New_Item.all);
   end Allocate_Element;

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

--  diff (Max_Length)
--
--
--
--
--
--

   procedure Reallocate (Container : in out Vector; Capacity : Count_Type);
   procedure Reallocate (Container : in out Vector; Capacity : Count_Type) is
   begin
      if Capacity /= Limited_Vectors.Capacity (Container) then
         declare
            Old_Data : Data_Access := Container.Data;
         begin
            if Capacity = 0 then
               Container.Data := null;
            else
               Container.Data :=
                  new Data'(
                     Capacity_Last =>
                       Index_Type'First - 1 + Index_Type'Base (Capacity),
                       Items => <>);
               for I in Index_Type'First .. Last (Container) loop
                  Container.Data.Items (I) := Old_Data.Items (I);
                  Old_Data.Items (I) := null;
               end loop;
            end if;
            Release (Old_Data);
         end;
      end if;
   end Reallocate;

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

   --  implementation

   function Empty_Vector return Vector is
   begin
      return (Finalization.Limited_Controlled with Data => null, Length => 0);
   end Empty_Vector;

   function Has_Element (Position : Cursor) return Boolean is
   begin
      return Position /= No_Element;
   end Has_Element;

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
--
--

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

--  diff (Generic_Array_To_Vector)
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

   function Capacity (Container : Vector) return Count_Type is
      Data : constant Data_Access := Container.Data;
   begin
      if Data = null then
         return 0;
      else
         return Count_Type'Base (Data.Capacity_Last - Index_Type'First + 1);
      end if;
   end Capacity;

   procedure Reserve_Capacity (
      Container : in out Vector;
      Capacity : Count_Type)
   is
      New_Capacity : constant Count_Type :=
         Count_Type'Max (Capacity, Container.Length);
   begin
      Reallocate (Container, New_Capacity);
   end Reserve_Capacity;

   function Length (Container : Vector) return Count_Type is
   begin
      return Container.Length;
   end Length;

   procedure Set_Length (Container : in out Vector; Length : Count_Type) is
      Old_Capacity : constant Count_Type := Capacity (Container);
--  diff
   begin
--  diff
--  diff
--  diff
--  diff
--  diff
--  diff
--  diff
      if Length > Old_Capacity then
         declare
            New_Capacity : constant Count_Type :=
               Count_Type'Max (Old_Capacity * 2, Length);
         begin
--  diff
--  diff
--  diff
--  diff
            Reallocate (Container, New_Capacity);
         end;
      end if;
      Container.Length := Length;
   end Set_Length;

   function Is_Empty (Container : Vector) return Boolean is
   begin
      return Container.Length = 0;
   end Is_Empty;

   procedure Clear (Container : in out Vector) is
   begin
      Release (Container.Data);
      Container.Length := 0;
   end Clear;

   function To_Cursor (
      Container : Vector'Class;
      Index : Extended_Index)
      return Cursor
   is
      pragma Check (Pre,
         Check =>
            Index <= Last_Index (Container) + 1
            or else raise Constraint_Error);
   begin
      if Index = Index_Type'First + Index_Type'Base (Container.Length) then
         return No_Element; -- Last_Index (Container) + 1
      else
         return Index;
      end if;
   end To_Cursor;

--  diff (Element)
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
      Process (
         Constant_Reference (
               Vector (Container),
               Index) -- checking Constraint_Error
            .Element.all);
   end Query_Element;

   procedure Update_Element (
      Container : in out Vector'Class;
      Position : Cursor;
      Process : not null access procedure (Element : in out Element_Type)) is
   begin
      Process (
         Reference (Vector (Container), Position) -- checking Constraint_Error
            .Element.all);
   end Update_Element;

   function Constant_Reference (Container : aliased Vector; Position : Cursor)
      return Constant_Reference_Type
   is
      pragma Check (Pre,
         Check =>
            Position in Index_Type'First .. Last (Container)
            or else raise Constraint_Error);
   begin
--  diff
      declare
         Data : constant Data_Access := Container.Data;
      begin
         return (Element => Data.Items (Position).all'Access);
      end;
   end Constant_Reference;

   function Reference (Container : aliased in out Vector; Position : Cursor)
      return Reference_Type
   is
      pragma Check (Pre,
         Check =>
            Position in Index_Type'First .. Last (Container)
            or else raise Constraint_Error);
   begin
--  diff
      declare
         Data : constant Data_Access := Container.Data;
      begin
         return (Element => Data.Items (Position).all'Access);
      end;
   end Reference;

--  diff (Assign)
--
--
--
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

   procedure Move (Target : in out Vector; Source : in out Vector) is
   begin
      Release (Target.Data);
      Target.Data := Source.Data;
      Source.Data := null;
--  diff
      Target.Length := Source.Length;
      Source.Length := 0;
   end Move;

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

   procedure Insert (
      Container : in out Vector'Class;
      Before : Cursor;
      New_Item : not null access function return Element_Type;
      Count : Count_Type := 1)
   is
      Position : Cursor;
   begin
      Insert (
         Container,
         Before, -- checking Constraint_Error
         New_Item,
         Position,
         Count);
   end Insert;

   procedure Insert (
      Container : in out Vector'Class;
      Before : Cursor;
      New_Item : not null access function return Element_Type;
      Position : out Cursor;
      Count : Count_Type := 1) is
   begin
      Insert_Space (
         Vector (Container),
         Before, -- checking Constraint_Error
         Position,
         Count);
      for I in Position .. Position + Index_Type'Base (Count) - 1 loop
         declare
            E : Element_Access
               renames Container.Data.Items (I);
         begin
            pragma Check (Validate, E = null);
            Allocate_Element (E, New_Item);
         end;
      end loop;
   end Insert;

--  diff (Prepend)
--
--
--
--
--
--

   procedure Prepend (
      Container : in out Vector'Class;
      New_Item : not null access function return Element_Type;
      Count : Count_Type := 1) is
   begin
      Insert (Container, Index_Type'First, New_Item, Count);
   end Prepend;

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

   procedure Append (
      Container : in out Vector'Class;
      New_Item : not null access function return Element_Type;
      Count : Count_Type := 1)
   is
      Old_Length : constant Count_Type := Container.Length;
   begin
      Set_Length (Vector (Container), Old_Length + Count);
      for I in
         Index_Type'First + Index_Type'Base (Old_Length) ..
         Last (Vector (Container))
      loop
         declare
            E : Element_Access
               renames Container.Data.Items (I);
         begin
            pragma Check (Validate, E = null);
            Allocate_Element (E, New_Item);
         end;
      end loop;
   end Append;

   procedure Insert_Space (
      Container : in out Vector'Class;
      Before : Extended_Index;
      Count : Count_Type := 1)
   is
      Position : Cursor;
   begin
      Insert_Space (
         Vector (Container),
         Before, -- checking Constraint_Error
         Position,
         Count);
   end Insert_Space;

   procedure Insert_Space (
      Container : in out Vector;
      Before : Cursor;
      Position : out Cursor;
      Count : Count_Type := 1)
   is
      pragma Check (Pre,
         Check =>
            Before <= Last (Container) + 1 or else raise Constraint_Error);
      Old_Length : constant Count_Type := Container.Length;
      After_Last : constant Index_Type'Base :=
         Index_Type'First + Index_Type'Base (Old_Length);
   begin
      Position := Before;
      if Position = No_Element then
         Position := After_Last;
      end if;
      if Count > 0 then
         Set_Length (Container, Old_Length + Count);
         if Position < After_Last then -- Last_Index (Container) + 1
--  diff
            declare
               Data : constant Data_Access := Container.Data;
               subtype R1 is
                  Extended_Index range
                     Position + Index_Type'Base (Count) ..
                     After_Last - 1 + Index_Type'Base (Count);
               subtype R2 is Extended_Index range Position .. After_Last - 1;
            begin
               for I in R2'Last + 1 .. R1'Last loop
                  Free (Data.Items (I));
               end loop;
               Data.Items (R1) := Data.Items (R2);
               for I in R2'First .. R1'First - 1 loop
                  Data.Items (I) := null;
               end loop;
            end;
         end if;
      end if;
   end Insert_Space;

   procedure Delete (
      Container : in out Vector;
      Position : in out Cursor;
      Count : Count_Type := 1)
   is
      pragma Check (Pre,
         Check =>
            Position in
               Index_Type'First ..
               Last (Container) - Index_Type'Base (Count) + 1
            or else raise Constraint_Error);
   begin
      if Count > 0 then
         declare
            Old_Length : constant Count_Type := Container.Length;
            After_Last : constant Index_Type'Base :=
               Index_Type'First + Index_Type'Base (Old_Length);
         begin
            if Position + Index_Type'Base (Count) < After_Last then
--  diff
               declare
                  Data : constant Data_Access :=
                     Container.Data;
                  subtype R1 is
                     Extended_Index range
                        Position .. After_Last - 1 - Index_Type'Base (Count);
                  subtype R2 is
                     Extended_Index range
                        Position + Index_Type'Base (Count) .. After_Last - 1;
               begin
                  for I in R1'First .. R2'First - 1 loop
                     Free (Data.Items (I));
                  end loop;
                  Data.Items (R1) := Data.Items (R2);
                  for I in R1'Last + 1 .. R2'Last loop
                     Data.Items (I) := null;
                  end loop;
               end;
            end if;
            Container.Length := Old_Length - Count;
            Position := No_Element;
         end;
      end if;
   end Delete;

   procedure Delete_First (
      Container : in out Vector'Class;
      Count : Count_Type := 1)
   is
      Position : Cursor := Index_Type'First;
   begin
      Delete (Vector (Container), Position, Count => Count);
   end Delete_First;

   procedure Delete_Last (
      Container : in out Vector'Class;
      Count : Count_Type := 1) is
   begin
      Container.Length := Container.Length - Count;
   end Delete_Last;

   procedure Reverse_Elements (Container : in out Vector) is
   begin
--  diff
--  diff
      Array_Sorting.In_Place_Reverse (
         Index_Type'Pos (Index_Type'First),
         Index_Type'Pos (Last (Container)),
         DA_Conv.To_Address (Container.Data),
         Swap => Swap_Element'Access);
--  diff
   end Reverse_Elements;

   procedure Swap (Container : in out Vector; I, J : Cursor) is
      pragma Check (Pre,
         Check =>
            (I in Index_Type'First .. Last (Container)
               and then J in Index_Type'First .. Last (Container))
            or else raise Constraint_Error);
   begin
--  diff
      Swap_Element (
         Index_Type'Pos (I),
         Index_Type'Pos (J),
         DA_Conv.To_Address (Container.Data));
   end Swap;

   function First_Index (Container : Vector'Class)
      return Index_Type
   is
      pragma Unreferenced (Container);
   begin
      return Index_Type'First;
   end First_Index;

   function First (Container : Vector) return Cursor is
   begin
      if Container.Length = 0 then
         return No_Element;
      else
         return Index_Type'First;
      end if;
   end First;

--  diff (First_Element)
--
--
--
--

   function Last_Index (Container : Vector'Class)
      return Extended_Index is
   begin
      return Last (Vector (Container));
   end Last_Index;

   function Last (Container : Vector) return Cursor is
   begin
      return Index_Type'First - 1 + Index_Type'Base (Container.Length);
   end Last;

--  diff (Last_Element)
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

--  diff (Find)
--
--
--
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

--  diff (Reverse_Find)
--
--
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

--  diff (Contains)
--
--
--
--

   procedure Iterate (
      Container : Vector'Class;
      Process : not null access procedure (Position : Cursor)) is
   begin
      for I in Index_Type'First .. Last (Vector (Container)) loop
         Process (I);
      end loop;
   end Iterate;

   procedure Reverse_Iterate (
      Container : Vector'Class;
      Process : not null access procedure (Position : Cursor)) is
   begin
      for I in reverse Index_Type'First .. Last (Vector (Container)) loop
         Process (I);
      end loop;
   end Reverse_Iterate;

   function Iterate (Container : Vector'Class)
      return Vector_Iterator_Interfaces.Reversible_Iterator'Class is
   begin
      return Vector_Iterator'(
         First => First (Vector (Container)),
         Last => Last (Vector (Container)));
   end Iterate;

   function Iterate (Container : Vector'Class; First, Last : Cursor)
      return Vector_Iterator_Interfaces.Reversible_Iterator'Class
   is
      pragma Check (Pre,
         Check =>
            (First <= Limited_Vectors.Last (Vector (Container)) + 1
               and then Last <= Limited_Vectors.Last (Vector (Container)))
            or else raise Constraint_Error);
      Actual_First : Cursor := First;
      Actual_Last : Cursor := Last;
   begin
      if Actual_First = No_Element
         or else Actual_Last < Actual_First -- implies Last = No_Element
      then
         Actual_First := No_Element;
         Actual_Last := No_Element;
      end if;
      return Vector_Iterator'(First => Actual_First, Last => Actual_Last);
   end Iterate;

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

--  diff (Constant_Indexing)
--
--
--
--
--
--

--  diff (Indexing)
--
--
--
--
--
--

   package body Generic_Sorting is

      function LT (Left, Right : Word_Integer; Params : System.Address)
         return Boolean;
      function LT (Left, Right : Word_Integer; Params : System.Address)
         return Boolean
      is
         Data : constant Data_Access := DA_Conv.To_Pointer (Params);
      begin
         return Data.Items (Index_Type'Val (Left)).all <
            Data.Items (Index_Type'Val (Right)).all;
      end LT;

      function Is_Sorted (Container : Vector) return Boolean is
      begin
--  diff
--  diff
--  diff
--  diff
         return Array_Sorting.Is_Sorted (
            Index_Type'Pos (Index_Type'First),
            Index_Type'Pos (Last (Container)),
            DA_Conv.To_Address (Container.Data),
            LT => LT'Access);
--  diff
      end Is_Sorted;

      procedure Sort (Container : in out Vector) is
      begin
--  diff
--  diff
         Array_Sorting.In_Place_Merge_Sort (
            Index_Type'Pos (Index_Type'First),
            Index_Type'Pos (Last (Container)),
            DA_Conv.To_Address (Container.Data),
            LT => LT'Access,
            Swap => Swap_Element'Access);
--  diff
      end Sort;

      procedure Merge (Target : in out Vector; Source : in out Vector) is
         pragma Check (Pre,
            Check =>
               Target'Address /= Source'Address
               or else Is_Empty (Target)
               or else raise Program_Error);
               --  RM A.18.2(237/3), same nonempty container
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
                  for I in Index_Type'First .. Last (Source) loop
                     Target.Data.Items (
                           I + Index_Type'Base (Old_Length)) :=
                        Source.Data.Items (I);
                     Source.Data.Items (I) := null;
                  end loop;
                  Source.Length := 0;
                  Array_Sorting.In_Place_Merge (
                     Index_Type'Pos (Index_Type'First),
                     Word_Integer (Index_Type'First)
                        + Word_Integer (Old_Length),
                     Index_Type'Pos (Last (Target)),
                     DA_Conv.To_Address (Target.Data),
                     LT => LT'Access,
                     Swap => Swap_Element'Access);
               end if;
            end;
         end if;
      end Merge;

   end Generic_Sorting;

   package body Equivalents is

      function Equivalent_Element (Left : Element_Access; Right : Element_Type)
         return Boolean;
      function Equivalent_Element (Left : Element_Access; Right : Element_Type)
         return Boolean is
      begin
         return Left /= null and then Left.all = Right;
      end Equivalent_Element;

      function "=" (Left, Right : Vector) return Boolean is
      begin
         if Left.Length /= Right.Length then
            return False;
         else
            for I in Index_Type'First .. Last (Left) loop
               if Left.Data.Items (I) = null then
                  if Right.Data.Items (I) /= null then
                     return False;
                  end if;
               elsif not Equivalent_Element (
                  Right.Data.Items (I),
                  Left.Data.Items (I).all)
               then
                  return False;
               end if;
            end loop;
            return True;
         end if;
      end "=";

      function Find (
         Container : Vector;
         Item : Element_Type)
         return Cursor is
      begin
         return Find (Container, Item, First (Container));
      end Find;

      function Find (
         Container : Vector;
         Item : Element_Type;
         Position : Cursor)
         return Cursor
      is
         pragma Check (Pre,
            Check =>
               Position in Index_Type'First .. Last (Container)
               or else (Is_Empty (Container) and then Position = No_Element)
               or else raise Constraint_Error);
      begin
         if Position >= Index_Type'First then
            for I in Position .. Last (Container) loop
               if Equivalent_Element (Container.Data.Items (I), Item) then
                  return I;
               end if;
            end loop;
         end if;
         return No_Element;
      end Find;

      function Reverse_Find (
         Container : Vector;
         Item : Element_Type)
         return Cursor is
      begin
         return Reverse_Find (Container, Item, Last (Container));
      end Reverse_Find;

      function Reverse_Find (
         Container : Vector;
         Item : Element_Type;
         Position : Cursor)
         return Cursor
      is
         pragma Check (Pre,
            Check =>
               Position in Index_Type'First .. Last (Container)
               or else (Is_Empty (Container) and then Position = No_Element)
               or else raise Constraint_Error);
      begin
         if Position >= Index_Type'First then
            for I in reverse Index_Type'First .. Position loop
               if Equivalent_Element (Container.Data.Items (I), Item) then
                  return I;
               end if;
            end loop;
         end if;
         return No_Element;
      end Reverse_Find;

      function Contains (Container : Vector; Item : Element_Type)
         return Boolean is
      begin
         return Reverse_Find (Container, Item) /= No_Element;
      end Contains;

   end Equivalents;

end Ada.Containers.Limited_Vectors;
