with Ada.Unchecked_Deallocation;
with Ada.Containers.Inside.Array_Sorting;
package body Ada.Containers.Vectors is
   use type Interfaces.Integer_32;

--  diff (Free)
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
         if Interfaces.sync_sub_and_fetch (
            Data.Reference_Count'Access,
            1) = 0
         then
--  diff
--  diff
--  diff
            Free (Data);
         end if;
      end if;
   end Release;

   procedure Unique (Container : in out Vector);
   procedure Unique (Container : in out Vector) is
   begin
      Reserve_Capacity (Container, Capacity (Container));
   end Unique;

   function Array_To_Vector (
      Source : Element_Array;
      Capacity : Count_Type)
      return Vector;
   function Array_To_Vector (
      Source : Element_Array;
      Capacity : Count_Type)
      return Vector
   is
      Length : constant Count_Type := Source'Length;
      New_Capacity : constant Count_Type := Count_Type'Min (Capacity, Length);
      New_Data : Data_Access;
   begin
      if New_Capacity = 0 then
         New_Data := Empty_Data'Unrestricted_Access;
      else
         declare
            Capacity_Last : constant Extended_Index :=
               Index_Type'First - 1 + Index_Type'Base (New_Capacity);
            Last : constant Extended_Index :=
               Index_Type'First - 1 + Index_Type'Base (Length);
         begin
            New_Data := new Data'(
               Capacity_Last => Last,
               Reference_Count => 1,
               Max_Length => Interfaces.Integer_32 (Length),
               Items => Source (Index_Type'First .. Last) &
                  (Last + 1 .. Capacity_Last => <>));
--  diff
--  diff
--  diff
--  diff
--  diff
         end;
      end if;
      return (Finalization.Controlled with Data => New_Data, Length => Length);
   end Array_To_Vector;

   procedure Adjust (Object : in out Vector) is
   begin
      if Object.Data /= Empty_Data'Unrestricted_Access then
         Interfaces.sync_add_and_fetch (Object.Data.Reference_Count'Access, 1);
      end if;
   end Adjust;

   procedure Assign (Target : in out Vector; Source : Vector) is
   begin
      Target.Length := Source.Length;
      if Target.Data /= Source.Data then
         Clear (Target);
         Target.Data := Source.Data;
         Adjust (Target);
      end if;
   end Assign;

   procedure Append (Container : in out Vector; New_Item : Vector) is
   begin
      if New_Item.Length > 0 then
         declare
            Old_Length : constant Count_Type := Container.Length;
         begin
            if Old_Length = 0 then
               Assign (Container, New_Item);
            else
               Set_Length (Container, Old_Length + New_Item.Length);
               declare
                  subtype R1 is Index_Type range
                     Index_Type'First + Index_Type'Base (Old_Length) ..
                     Last_Index (Container);
                  subtype R2 is Index_Type range
                     Index_Type'First ..
                     Last_Index (New_Item);
               begin
                  Container.Data.Items (R1) := New_Item.Data.Items (R2);
               end;
            end if;
         end;
      end if;
   end Append;

   procedure Append (
      Container : in out Vector;
      New_Item : Element_Type;
      Count : Count_Type := 1)
   is
      Old_Length : constant Count_Type := Container.Length;
   begin
      Set_Length (Container, Old_Length + Count);
      for I in
         Index_Type'First + Index_Type'Base (Old_Length) ..
         Last_Index (Container)
      loop
         Container.Data.Items (I) := New_Item;
--  diff
--  diff
--  diff
      end loop;
   end Append;

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
      return (Element => Container.Data.Items (Index)'Access);
   end Constant_Reference;

   function Constant_Reference (Container : not null access constant Vector)
      return Slicing.Constant_Reference_Type is
   begin
      return Constant_Reference (
         Container,
         Index_Type'First,
         Last_Index (Container.all));
   end Constant_Reference;

   function Constant_Reference (
      Container : not null access constant Vector;
      First_Index : Index_Type;
      Last_Index : Extended_Index)
      return Slicing.Constant_Reference_Type is
   begin
      return Slicing.Constant_Slice (
         Container.Data.Items'Unrestricted_Access,
         First_Index,
         Last_Index);
   end Constant_Reference;

   function Contains (Container : Vector; Item : Element_Type)
      return Boolean is
   begin
      return Find_Index (Container, Item) /= No_Index;
   end Contains;

   function Copy (Source : Vector; Capacity : Count_Type := 0) return Vector is
   begin
      return Result : Vector := Source do
         Reserve_Capacity (Result, Capacity);
      end return;
   end Copy;

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
         Unique (Container);
         declare
            Old_Length : constant Count_Type := Container.Length;
            Moving : constant Index_Type'Base :=
               (Index_Type'First + Index_Type'Base (Old_Length)) -
               (Index + Index_Type'Base (Count)) - 1;
            Before : constant Index_Type := Index + Index_Type'Base (Count);
            After : constant Index_Type := Index;
         begin
            Container.Length := Container.Length - Count;
--  diff
--  diff
--  diff
            Container.Data.Items (After .. After + Moving) :=
               Container.Data.Items (Before .. Before + Moving);
--  diff
--  diff
--  diff
--  diff
--  diff
--  diff
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

   function Element (Container : Vector; Index : Index_Type)
      return Element_Type is
   begin
      return Container.Data.Items (Index);
   end Element;

   function Empty_Vector return Vector is
   begin
      return (Finalization.Controlled with
         Data => Empty_Data'Unrestricted_Access,
         Length => 0);
   end Empty_Vector;

   function Find (Container : Vector; Item : Element_Type) return Cursor is
   begin
      return Find (Container, Item, Index_Type'First);
   end Find;

   function Find (
      Container : Vector;
      Item : Element_Type;
      Position : Cursor) return Cursor
   is
      Result : constant Cursor := Find_Index (Container, Item, Position);
   begin
      if Result = No_Index then
         return Cursor'Last; --  Find (...) <= Last
      else
         return Result;
      end if;
   end Find;

   function Find_Index (
      Container : Vector;
      Item : Element_Type;
      Index : Index_Type := Index_Type'First)
      return Extended_Index is
   begin
      for I in Index .. Last_Index (Container) loop
         if Container.Data.Items (I) = Item then
--  diff
--  diff
            return I;
         end if;
      end loop;
      return No_Index;
   end Find_Index;

   function First_Element (Container : Vector) return Element_Type is
   begin
      return Container.Data.Items (Index_Type'First);
   end First_Element;

   function First_Index (Container : Vector) return Index_Type is
      pragma Unreferenced (Container);
   begin
      return Index_Type'First;
   end First_Index;

   function Generic_Array_To_Vector (S : Element_Array) return Vector is
   begin
      return Array_To_Vector (Vectors.Element_Array (S), 0);
   end Generic_Array_To_Vector;

   procedure Insert (
      Container : in out Vector;
      Before : Extended_Index;
      New_Item : Vector)
   is
      Position : Cursor;
   begin
      Insert (Container, Before, New_Item, Position);
   end Insert;

   procedure Insert (
      Container : in out Vector;
      Before : Cursor;
      New_Item : Vector;
      Position : out Cursor) is
   begin
      if Container.Length = 0 then
         Position := Before;
         Assign (Container, New_Item);
      else
         Insert_Space (Container, Before, Position, New_Item.Length);
         Container.Data.Items (
            Before ..
            Before + Index_Type'Base (New_Item.Length) - 1) :=
            New_Item.Data.Items;
--  diff
--  diff
      end if;
   end Insert;

   procedure Insert (
      Container : in out Vector;
      Before : Extended_Index;
      New_Item : Element_Type;
      Count : Count_Type := 1)
   is
      Position : Cursor;
   begin
      Insert (Container, Before, New_Item, Position, Count);
   end Insert;

   procedure Insert (
      Container : in out Vector;
      Before : Cursor;
      New_Item : Element_Type;
      Position : out Cursor;
      Count : Count_Type := 1) is
   begin
      Insert_Space (Container, Before, Position, Count);
      for I in Before .. Before + Index_Type'Base (Count) - 1 loop
         Container.Data.Items (I) := New_Item;
--  diff
--  diff
--  diff
      end loop;
   end Insert;

   procedure Insert (
      Container : in out Vector;
      Before : Extended_Index;
      Count : Count_Type := 1)
   is
      Position : Cursor;
   begin
      Insert_Space (Container, Before, Position, Count);
   end Insert;

   procedure Insert (
      Container : in out Vector;
      Before : Cursor;
      Position : out Cursor;
      Count : Count_Type := 1) is
   begin
      Insert_Space (Container, Before, Position, Count);
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
--  diff
--  diff
--  diff
--  diff
--  diff
--  diff
            Container.Data.Items (After .. After + Moving) :=
               Container.Data.Items (Before .. Before + Moving);
--  diff
--  diff
--  diff
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

   function Last_Element (Container : Vector) return Element_Type is
   begin
      return Container.Data.Items (Last_Index (Container));
   end Last_Element;

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

   procedure Prepend (Container : in out Vector; New_Item : Vector) is
   begin
      Insert (Container, Index_Type'First, New_Item);
   end Prepend;

   procedure Prepend (
      Container : in out Vector;
      New_Item : Element_Type;
      Count : Count_Type := 1) is
   begin
      Insert (Container, Index_Type'First, New_Item, Count);
   end Prepend;

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
      Process (Container.Data.Items (Index));
   end Query_Element;

   function Reference (
      Container : not null access Vector;
      Index : Index_Type)
      return Reference_Type is
   begin
      Unique (Container.all);
      return (Element => Container.Data.Items (Index)'Access);
   end Reference;

   function Reference (Container : not null access Vector)
      return Slicing.Reference_Type is
   begin
      return Reference (
         Container,
         Index_Type'First,
         Last_Index (Container.all));
   end Reference;

   function Reference (
      Container : not null access Vector;
      First_Index : Index_Type;
      Last_Index : Extended_Index)
      return Slicing.Reference_Type is
   begin
      Unique (Container.all);
      return Slicing.Slice (
         Container.Data.Items'Unrestricted_Access,
         First_Index,
         Last_Index);
   end Reference;

   procedure Replace_Element (
      Container : in out Vector;
      Index : Index_Type;
      New_Item : Element_Type) is
   begin
      Unique (Container);
      Container.Data.Items (Index) := New_Item;
--  diff
--  diff
--  diff
   end Replace_Element;

   procedure Reserve_Capacity (
      Container : in out Vector;
      Capacity : Count_Type) is
   begin
      if Capacity /= Vectors.Capacity (Container)
         or else (Container.Data /= Empty_Data'Unrestricted_Access
            and then Container.Data.Reference_Count > 1)
      then
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
                     Reference_Count => 1,
                     Max_Length => Interfaces.Integer_32 (Container.Length),
                     Items => <>);
                  declare
                     Last : constant Extended_Index := Last_Index (Container);
                     subtype R is Index_Type range Index_Type'First .. Last;
                  begin
                     Container.Data.Items (R) := Old_Data.Items (R);
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
      Unique (Container);
      Inside.Array_Sorting.In_Place_Reverse (Index_Type'Pos (Index_Type'First),
         Index_Type'Pos (Last_Index (Container)),
         Swap => Swap'Access);
   end Reverse_Elements;

   function Reverse_Find (Container : Vector; Item : Element_Type)
      return Cursor is
   begin
      return Reverse_Find_Index (Container, Item);
   end Reverse_Find;

   function Reverse_Find (
      Container : Vector;
      Item : Element_Type;
      Position : Cursor)
      return Cursor is
   begin
      return Reverse_Find_Index (Container, Item, Position);
   end Reverse_Find;

   function Reverse_Find_Index (
      Container : Vector;
      Item : Element_Type;
      Index : Index_Type := Index_Type'Last)
      return Extended_Index
   is
      Start : constant Extended_Index :=
         Extended_Index'Min (Index, Last_Index (Container));
   begin
      for I in reverse Index_Type'First .. Start loop
         if Container.Data.Items (I) = Item then
--  diff
--  diff
            return I;
         end if;
      end loop;
      return No_Index;
   end Reverse_Find_Index;

   procedure Reverse_Iterate (
      Container : Vector;
      Process : not null access procedure (Position : Cursor)) is
   begin
      for I in reverse Index_Type'First .. Last_Index (Container) loop
         Process (I);
      end loop;
   end Reverse_Iterate;

   procedure Set_Length (Container : in out Vector; Length : Count_Type) is
   begin
      if Length > Container.Length then
         declare
            Old_Capacity : constant Count_Type := Capacity (Container);
         begin
            if Length > Old_Capacity then
               declare
                  New_Capacity : constant Count_Type :=
                     Count_Type'Max (Old_Capacity * 2, Length);
               begin
                  Reserve_Capacity (Container, New_Capacity);
                  Container.Data.Max_Length := Interfaces.Integer_32 (Length);
               end;
            else
               if Interfaces.sync_bool_compare_and_swap (
                  Container.Data.Max_Length'Access,
                  Interfaces.Integer_32 (Container.Length),
                  Interfaces.Integer_32 (Length))
               then
                  null;
               elsif Container.Data.Reference_Count > 1 then
                  Reserve_Capacity (Container, Old_Capacity);
                  Container.Data.Max_Length := Interfaces.Integer_32 (Length);
               end if;
            end if;
         end;
      end if;
      Container.Length := Length;
   end Set_Length;

   procedure Swap (Container : in out Vector; I, J : Index_Type) is
   begin
      Unique (Container);
      declare
         Temp : constant Element_Type := Container.Data.Items (I);
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

   function To_Vector (New_Item : Element_Type; Length : Count_Type)
      return Vector is
   begin
      return Result : Vector do
         Append (Result, New_Item, Length);
      end return;
   end To_Vector;

   procedure Update_Element (
      Container : in out Vector;
      Index : Index_Type;
      Process : not null access procedure (Element : in out Element_Type)) is
   begin
      Unique (Container);
      Process (Container.Data.Items (Index));
   end Update_Element;

   function "=" (Left, Right : Vector) return Boolean is
   begin
      if Left.Length /= Right.Length then
         return False;
      elsif Left.Data = Right.Data then
         return True;
      else
         declare
            Last : constant Extended_Index := Last_Index (Left);
         begin
            return Left.Data.Items (Index_Type'First .. Last) =
               Right.Data.Items (Index_Type'First .. Last);
         end;
--  diff
--  diff
--  diff
--  diff
--  diff
--  diff
      end if;
   end "=";

   function "&" (Left, Right : Vector) return Vector is
   begin
      return Result : Vector := Left do
         Append (Result, Right);
      end return;
   end "&";

   function "&" (Left : Vector; Right : Element_Type) return Vector is
   begin
      return Result : Vector := Left do
         Append (Result, Right);
      end return;
   end "&";

   function "&" (Left : Element_Type; Right : Vector) return Vector is
   begin
      return Result : Vector do
         Reserve_Capacity (Result, 1 + Right.Length);
         Append (Result, Left);
         Append (Result, Right);
      end return;
   end "&";

   function "&" (Left, Right : Element_Type) return Vector is
   begin
      return Result : Vector do
         Reserve_Capacity (Result, 2);
         Append (Result, Left);
         Append (Result, Right);
      end return;
   end "&";

   package body Generic_Sorting is

      function Is_Sorted (Container : Vector) return Boolean is
         function LT (Left, Right : Integer) return Boolean;
         function LT (Left, Right : Integer) return Boolean is
         begin
            return Container.Data.Items (Index_Type'Val (Left)) <
               Container.Data.Items (Index_Type'Val (Right));
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
            return Container.Data.Items (Index_Type'Val (Left)) <
               Container.Data.Items (Index_Type'Val (Right));
         end LT;
         procedure Swap (I, J : Integer);
         procedure Swap (I, J : Integer) is
         begin
            Swap (Container, Index_Type'Val (I), Index_Type'Val (J));
         end Swap;
      begin
         Unique (Container);
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
            return Target.Data.Items (Index_Type'Val (Left)) <
               Target.Data.Items (Index_Type'Val (Right));
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
            Append (Target, Source);
--  diff
--  diff
--  diff
--  diff
--  diff
            Source.Length := 0;
            Unique (Target);
         end if;
         Inside.Array_Sorting.In_Place_Merge (
            Index_Type'Pos (Index_Type'First),
            Integer (Index_Type'First) - 1 + Integer (Old_Length),
            Index_Type'Pos (Last_Index (Target)),
            LT => LT'Access,
            Swap => Swap'Access);
      end Merge;

   end Generic_Sorting;

   package body No_Primitives is

      procedure Read (
         Stream : not null access Streams.Root_Stream_Type'Class;
         Container : out Vector)
      is
         Length : Count_Type'Base;
      begin
         Clear (Container);
         Count_Type'Base'Read (Stream, Length);
         if Length > 0 then
            Set_Length (Container, Length);
            Element_Array'Read (
               Stream,
               Container.Data.Items (
                  Index_Type'First ..
                  Last_Index (Container)));
         end if;
      end Read;

      procedure Write (
         Stream : not null access Streams.Root_Stream_Type'Class;
         Container : Vector)
      is
         Length : constant Count_Type := Vectors.Length (Container);
      begin
         Count_Type'Base'Write (Stream, Length);
         if Length > 0 then
            Element_Array'Write (
               Stream,
               Container.Data.Items (
                  Index_Type'First ..
                  Last_Index (Container)));
         end if;
      end Write;

   end No_Primitives;

end Ada.Containers.Vectors;
