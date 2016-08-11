with Ada.Containers.Array_Sorting;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System.Address_To_Named_Access_Conversions;
package body Ada.Containers.Vectors is
   pragma Check_Policy (Validate => Ignore);
   use type Copy_On_Write.Data_Access;

   type Element_Array_Access is access all Element_Array;

   package DA_Conv is
      new System.Address_To_Named_Access_Conversions (Data, Data_Access);

   function Upcast is
      new Unchecked_Conversion (Data_Access, Copy_On_Write.Data_Access);
   function Downcast is
      new Unchecked_Conversion (Copy_On_Write.Data_Access, Data_Access);

--  diff (Free)
   procedure Free is new Unchecked_Deallocation (Data, Data_Access);

   procedure Swap_Element (I, J : Integer; Params : System.Address);
   procedure Swap_Element (I, J : Integer; Params : System.Address) is
      Data : constant Data_Access := DA_Conv.To_Pointer (Params);
      Temp : constant Element_Type := Data.Items (Index_Type'Val (I));
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

   procedure Free_Data (Data : in out Copy_On_Write.Data_Access);
   procedure Free_Data (Data : in out Copy_On_Write.Data_Access) is
      X : Data_Access := Downcast (Data);
   begin
--  diff
--  diff
--  diff
      Free (X);
      Data := null;
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
      New_Data : constant Data_Access :=
         new Data'(
            Capacity_Last => Index_Type'First - 1 + Index_Type'Base (Capacity),
            Super => <>,
            Max_Length => Max_Length,
            Items => <>);
   begin
      Target := Upcast (New_Data);
   end Allocate_Data;

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
      Capacity : Count_Type) is
   begin
      Allocate_Data (Target, Max_Length, Capacity);
      declare
         subtype R is
            Extended_Index range
               Index_Type'First ..
               Index_Type'First - 1 + Index_Type'Base (Length);
      begin
         Downcast (Target).Items (R) := Downcast (Source).Items (R);
--  diff
--  diff
--  diff
--  diff
--  diff
--  diff
      end;
   end Copy_Data;

   procedure Reallocate (
      Container : in out Vector;
      Length : Count_Type;
      Capacity : Count_Type;
      To_Update : Boolean);
   procedure Reallocate (
      Container : in out Vector;
      Length : Count_Type;
      Capacity : Count_Type;
      To_Update : Boolean) is
   begin
      Copy_On_Write.Unique (
         Target => Container.Super'Access,
         Target_Length => Container.Length,
         Target_Capacity => Vectors.Capacity (Container),
         New_Length => Length,
         New_Capacity => Capacity,
         To_Update => To_Update,
         Allocate => Allocate_Data'Access,
         Move => Copy_Data'Access,
         Copy => Copy_Data'Access,
         Free => Free_Data'Access);
--  diff
   end Reallocate;

   procedure Unique (Container : in out Vector; To_Update : Boolean);
   procedure Unique (Container : in out Vector; To_Update : Boolean) is
   begin
      if Copy_On_Write.Shared (Container.Super.Data) then
         Reallocate (
            Container,
            Container.Length,
            Capacity (Container), -- not shrinking
            To_Update);
      end if;
   end Unique;

   --  implementation

   function Empty_Vector return Vector is
   begin
      return (Finalization.Controlled with Super => <>, Length => 0);
   end Empty_Vector;

   function Has_Element (Position : Cursor) return Boolean is
   begin
      return Position /= No_Element;
   end Has_Element;

   overriding function "=" (Left, Right : Vector) return Boolean is
   begin
      if Left.Length /= Right.Length then
         return False;
      elsif Left.Super.Data = Right.Super.Data then
         return True;
      else
         Unique (Left'Unrestricted_Access.all, False); -- private
         Unique (Right'Unrestricted_Access.all, False); -- private
         for I in Index_Type'First .. Last (Left) loop
            if Downcast (Left.Super.Data).Items (I) /=
               Downcast (Right.Super.Data).Items (I)
            then
               return False;
            end if;
--  diff
--  diff
--  diff
--  diff
--  diff
         end loop;
         return True;
      end if;
   end "=";

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

   function Generic_Array_To_Vector (S : Element_Array) return Vector is
   begin
      return Result : Vector do
         declare
            Length : constant Count_Type := S'Length;
            subtype R1 is
               Extended_Index range
                  Index_Type'First ..
                  Index_Type'First - 1 + Index_Type'Base (Length);
         begin
            Set_Length (Result, Length);
            Downcast (Result.Super.Data).Items (R1) :=
               Vectors.Element_Array (S);
         end;
      end return;
   end Generic_Array_To_Vector;

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
         Reallocate (Result, 0, 1 + Right.Length, True);
         Append (Result, Left);
         Append (Result, Right);
      end return;
   end "&";

   function "&" (Left, Right : Element_Type) return Vector is
   begin
      return Result : Vector do
         Reallocate (Result, 0, 2, True);
         Append (Result, Left);
         Append (Result, Right);
      end return;
   end "&";

   function Capacity (Container : Vector) return Count_Type is
      Data : constant Data_Access := Downcast (Container.Super.Data);
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
      Reallocate (Container, Container.Length, New_Capacity, True);
   end Reserve_Capacity;

   function Length (Container : Vector) return Count_Type is
   begin
      return Container.Length;
   end Length;

   procedure Set_Length (Container : in out Vector; Length : Count_Type) is
      Old_Capacity : constant Count_Type := Capacity (Container);
      Data : constant Data_Access := Downcast (Container.Super.Data);
      Failure : Boolean;
   begin
      Copy_On_Write.In_Place_Set_Length (
         Target_Data => Upcast (Data),
         Target_Length => Container.Length,
         Target_Max_Length => Data.Max_Length,
         Target_Capacity => Old_Capacity,
         New_Length => Length,
         Failure => Failure);
      if Failure then
         declare
            New_Capacity : Count_Type;
         begin
            if Old_Capacity >= Length then
               New_Capacity := Old_Capacity; -- not shrinking
            else
               New_Capacity := Count_Type'Max (Old_Capacity * 2, Length);
            end if;
            Reallocate (Container, Length, New_Capacity, False);
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
      Copy_On_Write.Clear (Container.Super'Access, Free => Free_Data'Access);
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

   function Element (
      Container : Vector'Class;
      Index : Index_Type)
      return Element_Type is
   begin
      return Constant_Reference (
         Vector (Container),
         Index).Element.all; -- checking Constraint_Error
   end Element;

   procedure Replace_Element (
      Container : in out Vector;
      Position : Cursor;
      New_Item : Element_Type)
   is
      pragma Check (Pre,
         Check =>
            Position in Index_Type'First .. Last (Container)
            or else raise Constraint_Error);
   begin
      Unique (Container, True);
      declare
         E : Element_Type
            renames Downcast (Container.Super.Data).Items (Position);
      begin
--  diff
         E := New_Item;
      end;
   end Replace_Element;

   procedure Query_Element (
      Container : Vector'Class;
      Index : Index_Type;
      Process : not null access procedure (Element : Element_Type)) is
   begin
      Process (
         Constant_Reference (
            Vector (Container),
            Index).Element.all); -- checking Constraint_Error
   end Query_Element;

   procedure Update_Element (
      Container : in out Vector'Class;
      Position : Cursor;
      Process : not null access procedure (Element : in out Element_Type)) is
   begin
      Process (
         Reference (
            Vector (Container),
            Position).Element.all); -- checking Constraint_Error
   end Update_Element;

   function Constant_Reference (Container : aliased Vector; Position : Cursor)
      return Constant_Reference_Type
   is
      pragma Check (Pre,
         Check =>
            Position in Index_Type'First .. Last (Container)
            or else raise Constraint_Error);
   begin
      Unique (Container'Unrestricted_Access.all, False);
      declare
         Data : constant Data_Access := Downcast (Container.Super.Data);
      begin
         return (Element => Data.all.Items (Position)'Access); -- [gcc-6] .all
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
      Unique (Container, True);
      declare
         Data : constant Data_Access := Downcast (Container.Super.Data);
      begin
         return (Element => Data.all.Items (Position)'Access); -- [gcc-6] .all
      end;
   end Reference;

   procedure Assign (Target : in out Vector; Source : Vector) is
   begin
      Copy_On_Write.Assign (
         Target.Super'Access,
         Source.Super'Access,
         Free => Free_Data'Access);
      Target.Length := Source.Length;
   end Assign;

   function Copy (Source : Vector; Capacity : Count_Type := 0) return Vector is
   begin
      return Result : Vector := Source do
         Reserve_Capacity (Result, Capacity);
      end return;
   end Copy;

   procedure Move (Target : in out Vector; Source : in out Vector) is
   begin
      Copy_On_Write.Move (
         Target.Super'Access,
         Source.Super'Access,
         Free => Free_Data'Access);
      Target.Length := Source.Length;
      Source.Length := 0;
   end Move;

   procedure Insert (
      Container : in out Vector;
      Before : Cursor;
      New_Item : Vector)
   is
      Position : Cursor;
   begin
      Insert (
         Container,
         Before, -- checking Constraint_Error
         New_Item,
         Position);
   end Insert;

   procedure Insert (
      Container : in out Vector;
      Before : Cursor;
      New_Item : Vector;
      Position : out Cursor)
   is
      pragma Check (Pre,
         Check =>
            Before <= Last (Container) + 1 or else raise Constraint_Error);
      New_Item_Length : constant Count_Type := New_Item.Length;
   begin
      if Container.Length = 0
         and then Capacity (Container) < New_Item_Length -- New_Item_Length > 0
      then
         Position := Index_Type'First;
         Assign (Container, New_Item);
      else
         Insert_Space (Container, Before, Position, New_Item_Length);
         if New_Item_Length > 0 then
            declare
               subtype R1 is
                  Extended_Index range
                     Position ..
                     Position + Index_Type'Base (New_Item_Length) - 1;
               subtype R2 is
                  Extended_Index range
                     Index_Type'First ..
                     Index_Type'First - 1 + Index_Type'Base (New_Item_Length);
               --  Do not use New_Item.Length or Last (New_Item) in here
               --    for Append (X, X).
            begin
               Downcast (Container.Super.Data).Items (R1) :=
                  Downcast (New_Item.Super.Data).Items (R2);
            end;
         end if;
--  diff
--  diff
      end if;
   end Insert;

   procedure Insert (
      Container : in out Vector;
      Before : Cursor;
      New_Item : Element_Type;
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
      Container : in out Vector;
      Before : Cursor;
      New_Item : Element_Type;
      Position : out Cursor;
      Count : Count_Type := 1) is
   begin
      Insert_Space (
         Container,
         Before, -- checking Constraint_Error
         Position,
         Count);
      for I in Position .. Position + Index_Type'Base (Count) - 1 loop
         declare
            E : Element_Type
               renames Downcast (Container.Super.Data).Items (I);
         begin
--  diff
            E := New_Item;
         end;
      end loop;
   end Insert;

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

   procedure Append (Container : in out Vector; New_Item : Vector) is
      New_Item_Length : constant Count_Type := New_Item.Length;
   begin
      if New_Item_Length > 0 then
         declare
            Old_Length : constant Count_Type := Container.Length;
         begin
            if Old_Length = 0
               and then Capacity (Container) < New_Item_Length
            then
               Assign (Container, New_Item);
            else
               Set_Length (Container, Old_Length + New_Item.Length);
               declare
                  subtype R1 is
                     Extended_Index range
                        Index_Type'First + Index_Type'Base (Old_Length) ..
                        Last (Container);
                  subtype R2 is
                     Extended_Index range
                        Index_Type'First ..
                        Index_Type'First
                           - 1
                           + Index_Type'Base (New_Item_Length);
                  --  Do not use New_Item.Length or Last (New_Item) in here
                  --    for Append (X, X).
               begin
                  Downcast (Container.Super.Data).Items (R1) :=
                     Downcast (New_Item.Super.Data).Items (R2);
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
         Index_Type'First + Index_Type'Base (Old_Length) .. Last (Container)
      loop
         declare
            E : Element_Type
               renames Downcast (Container.Super.Data).Items (I);
         begin
--  diff
            E := New_Item;
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
            Unique (Container, True);
            declare
               Data : constant Data_Access := Downcast (Container.Super.Data);
               subtype R1 is
                  Extended_Index range
                     Position + Index_Type'Base (Count) ..
                     After_Last - 1 + Index_Type'Base (Count);
               subtype R2 is Extended_Index range Position .. After_Last - 1;
            begin
--  diff
--  diff
--  diff
               Data.Items (R1) := Data.Items (R2);
--  diff
--  diff
--  diff
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
               Unique (Container, True);
               declare
                  Data : constant Data_Access :=
                     Downcast (Container.Super.Data);
                  subtype R1 is
                     Extended_Index range
                        Position ..
                        After_Last - 1 - Index_Type'Base (Count);
                  subtype R2 is
                     Extended_Index range
                        Position + Index_Type'Base (Count) ..
                        After_Last - 1;
               begin
--  diff
--  diff
--  diff
                  Data.Items (R1) := Data.Items (R2);
--  diff
--  diff
--  diff
               end;
            end if;
            Set_Length (Container, Old_Length - Count);
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
      Set_Length (Vector (Container), Container.Length - Count);
   end Delete_Last;

   procedure Reverse_Elements (Container : in out Vector) is
   begin
      if Container.Length > 1 then
         Unique (Container, True);
         Array_Sorting.In_Place_Reverse (
            Index_Type'Pos (Index_Type'First),
            Index_Type'Pos (Last (Container)),
            DA_Conv.To_Address (Downcast (Container.Super.Data)),
            Swap => Swap_Element'Access);
      end if;
   end Reverse_Elements;

   procedure Swap (Container : in out Vector; I, J : Cursor) is
      pragma Check (Pre,
         Check =>
            (I in Index_Type'First .. Last (Container)
               and then J in Index_Type'First .. Last (Container))
            or else raise Constraint_Error);
   begin
      Unique (Container, True);
      Swap_Element (
         Index_Type'Pos (I),
         Index_Type'Pos (J),
         DA_Conv.To_Address (Downcast (Container.Super.Data)));
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

   function First_Element (Container : Vector'Class)
      return Element_Type is
   begin
      return Element (Container, Index_Type'First);
   end First_Element;

   function Last_Index (Container : Vector'Class)
      return Extended_Index is
   begin
      return Last (Vector (Container));
   end Last_Index;

   function Last (Container : Vector) return Cursor is
   begin
      return Index_Type'First - 1 + Index_Type'Base (Container.Length);
   end Last;

   function Last_Element (Container : Vector'Class)
      return Element_Type is
   begin
      return Element (Container, Last_Index (Container));
   end Last_Element;

   function Find_Index (
      Container : Vector'Class;
      Item : Element_Type;
      Index : Index_Type := Index_Type'First)
      return Extended_Index is
   begin
      return Find (
         Vector (Container),
         Item,
         Index); -- checking Constraint_Error
   end Find_Index;

   function Find (
      Container : Vector;
      Item : Element_Type)
      return Cursor is
   begin
      return Find (Container, Item, Index_Type'First);
   end Find;

   function Find (
      Container : Vector;
      Item : Element_Type;
      Position : Cursor)
      return Cursor
   is
      pragma Check (Pre,
         Check =>
            (Position in Index_Type'First .. Last (Container))
            or else (Is_Empty (Container) and then Position = Index_Type'First)
            or else raise Constraint_Error);
      Last : constant Cursor := Vectors.Last (Container);
   begin
      if Position <= Last then
         Unique (Container'Unrestricted_Access.all, False); -- private
         for I in Position .. Last loop
            if Downcast (Container.Super.Data).Items (I) = Item then
--  diff
--  diff
--  diff
               return I;
            end if;
         end loop;
      end if;
      return No_Element;
   end Find;

   function Reverse_Find_Index (
      Container : Vector'Class;
      Item : Element_Type;
      Index : Index_Type := Index_Type'Last)
      return Extended_Index
   is
      Start : constant Extended_Index :=
         Extended_Index'Min (Index, Last_Index (Container));
   begin
      return Reverse_Find (
         Vector (Container),
         Item,
         Start); -- checking Constraint_Error
   end Reverse_Find_Index;

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
            (Position in Index_Type'First .. Last (Container))
            or else (Is_Empty (Container) and then Position = No_Element)
            or else raise Constraint_Error);
   begin
      if Position >= Index_Type'First then
         Unique (Container'Unrestricted_Access.all, False); -- private
         for I in reverse Index_Type'First .. Position loop
            if Downcast (Container.Super.Data).Items (I) = Item then
--  diff
--  diff
--  diff
               return I;
            end if;
         end loop;
      end if;
      return No_Element;
   end Reverse_Find;

   function Contains (Container : Vector; Item : Element_Type)
      return Boolean is
   begin
      return Find (Container, Item) /= No_Element;
   end Contains;

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
            (First <= Vectors.Last (Vector (Container)) + 1
               and then Last <= Vectors.Last (Vector (Container)))
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

   function Constant_Reference (Container : aliased Vector)
      return Slicing.Constant_Reference_Type is
   begin
      Unique (Container'Unrestricted_Access.all, False);
      declare
         Data : constant Data_Access := Downcast (Container.Super.Data);
      begin
         return Slicing.Constant_Slice (
            Element_Array_Access'(Data.Items'Unrestricted_Access).all,
            Index_Type'First,
            Last (Container));
      end;
   end Constant_Reference;

   function Reference (Container : aliased in out Vector)
      return Slicing.Reference_Type is
   begin
      Unique (Container, True);
      declare
         Data : constant Data_Access := Downcast (Container.Super.Data);
      begin
         return Slicing.Slice (
            Element_Array_Access'(Data.Items'Unrestricted_Access).all,
            Index_Type'First,
            Last (Container));
      end;
   end Reference;

   overriding procedure Adjust (Object : in out Vector) is
   begin
      Copy_On_Write.Adjust (Object.Super'Access);
   end Adjust;

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

   function Constant_Indexing (
      Container : aliased Vector'Class;
      Index : Index_Type)
      return Constant_Reference_Type is
   begin
      return Constant_Reference (Vector (Container), Index);
   end Constant_Indexing;

   function Indexing (
      Container : aliased in out Vector'Class;
      Index : Index_Type)
      return Reference_Type is
   begin
      return Reference (Vector (Container), Index);
   end Indexing;

   package body Generic_Sorting is

      function LT (Left, Right : Integer; Params : System.Address)
         return Boolean;
      function LT (Left, Right : Integer; Params : System.Address)
         return Boolean
      is
         Data : constant Data_Access := DA_Conv.To_Pointer (Params);
      begin
         return Data.Items (Index_Type'Val (Left)) <
            Data.Items (Index_Type'Val (Right));
      end LT;

      function Is_Sorted (Container : Vector) return Boolean is
      begin
         if Container.Length <= 1 then
            return True;
         else
            Unique (Container'Unrestricted_Access.all, False); -- private
            return Array_Sorting.Is_Sorted (
               Index_Type'Pos (Index_Type'First),
               Index_Type'Pos (Last (Container)),
               DA_Conv.To_Address (Downcast (Container.Super.Data)),
               LT => LT'Access);
         end if;
      end Is_Sorted;

      procedure Sort (Container : in out Vector) is
      begin
         if Container.Length > 1 then
            Unique (Container, True);
            Array_Sorting.In_Place_Merge_Sort (
               Index_Type'Pos (Index_Type'First),
               Index_Type'Pos (Last (Container)),
               DA_Conv.To_Address (Downcast (Container.Super.Data)),
               LT => LT'Access,
               Swap => Swap_Element'Access);
         end if;
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
                  Append (Target, Source);
                  Unique (Target, True);
--  diff
--  diff
--  diff
--  diff
--  diff
--  diff
--  diff
                  Set_Length (Source, 0);
                  Array_Sorting.In_Place_Merge (
                     Index_Type'Pos (Index_Type'First),
                     Integer (Index_Type'First) - 1 + Integer (Old_Length),
                     Index_Type'Pos (Last (Target)),
                     DA_Conv.To_Address (Downcast (Target.Super.Data)),
                     LT => LT'Access,
                     Swap => Swap_Element'Access);
               end if;
            end;
         end if;
      end Merge;

   end Generic_Sorting;

   package body Streaming is

      procedure Read (
         Stream : not null access Streams.Root_Stream_Type'Class;
         Item : out Vector)
      is
         Length : Count_Type'Base;
      begin
         Count_Type'Base'Read (Stream, Length);
         Clear (Item);
         if Length > 0 then
            Set_Length (Item, Length);
            Element_Array'Read (
               Stream,
               Downcast (Item.Super.Data).Items (
                  Index_Type'First ..
                  Last (Item)));
--  diff
--  diff
--  diff
--  diff
         end if;
      end Read;

      procedure Write (
         Stream : not null access Streams.Root_Stream_Type'Class;
         Item : Vector)
      is
         Length : constant Count_Type := Vectors.Length (Item);
      begin
         Count_Type'Write (Stream, Length);
         if Length > 0 then
            Unique (Item'Unrestricted_Access.all, False); -- private
            Element_Array'Write (
               Stream,
               Downcast (Item.Super.Data).Items (
                  Index_Type'First ..
                  Last (Item)));
         end if;
      end Write;

   end Streaming;

end Ada.Containers.Vectors;
