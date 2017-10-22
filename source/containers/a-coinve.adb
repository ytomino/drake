with Ada.Containers.Array_Sorting;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System.Address_To_Named_Access_Conversions;
with System.Long_Long_Integer_Types;
package body Ada.Containers.Indefinite_Vectors is
   pragma Check_Policy (Validate => Ignore);
   use type Copy_On_Write.Data_Access;
   use type System.Address;
   use type System.Long_Long_Integer_Types.Word_Integer;

   subtype Word_Integer is System.Long_Long_Integer_Types.Word_Integer;

   package DA_Conv is
      new System.Address_To_Named_Access_Conversions (Data, Data_Access);

   function Upcast is
      new Unchecked_Conversion (Data_Access, Copy_On_Write.Data_Access);
   function Downcast is
      new Unchecked_Conversion (Copy_On_Write.Data_Access, Data_Access);

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

   function Equivalent_Element (Left : Element_Access; Right : Element_Type)
      return Boolean;
   function Equivalent_Element (Left : Element_Access; Right : Element_Type)
      return Boolean is
   begin
      return Left /= null and then Left.all = Right;
   end Equivalent_Element;

   procedure Allocate_Element (
      Item : out Element_Access;
      New_Item : Element_Type);
   procedure Allocate_Element (
      Item : out Element_Access;
      New_Item : Element_Type) is
   begin
      Item := new Element_Type'(New_Item);
   end Allocate_Element;

   procedure Free_Data (Data : in out Copy_On_Write.Data_Access);
   procedure Free_Data (Data : in out Copy_On_Write.Data_Access) is
      X : Data_Access := Downcast (Data);
   begin
      for I in X.Items'Range loop
         Free (X.Items (I));
      end loop;
      Free (X);
      Data := null;
   end Free_Data;

   procedure Allocate_Data (
      Target : out not null Copy_On_Write.Data_Access;
      New_Length : Count_Type;
      Capacity : Count_Type);
   procedure Allocate_Data (
      Target : out not null Copy_On_Write.Data_Access;
      New_Length : Count_Type;
      Capacity : Count_Type)
   is
      New_Data : constant Data_Access :=
         new Data'(
            Capacity_Last => Index_Type'First - 1 + Index_Type'Base (Capacity),
            Super => <>,
            Max_Length => New_Length,
            Items => <>);
   begin
      Target := Upcast (New_Data);
   end Allocate_Data;

   procedure Move_Data (
      Target : out not null Copy_On_Write.Data_Access;
      Source : not null Copy_On_Write.Data_Access;
      Length : Count_Type;
      New_Length : Count_Type;
      Capacity : Count_Type);
   procedure Move_Data (
      Target : out not null Copy_On_Write.Data_Access;
      Source : not null Copy_On_Write.Data_Access;
      Length : Count_Type;
      New_Length : Count_Type;
      Capacity : Count_Type) is
   begin
      Allocate_Data (Target, New_Length, Capacity);
      declare
         subtype R is
            Extended_Index range
               Index_Type'First ..
               Index_Type'First - 1 + Index_Type'Base (Length);
      begin
         for I in R loop
            Downcast (Target).Items (I) := Downcast (Source).Items (I);
            Downcast (Source).Items (I) := null;
         end loop;
      end;
   end Move_Data;

   procedure Copy_Data (
      Target : out not null Copy_On_Write.Data_Access;
      Source : not null Copy_On_Write.Data_Access;
      Length : Count_Type;
      New_Length : Count_Type;
      Capacity : Count_Type);
   procedure Copy_Data (
      Target : out not null Copy_On_Write.Data_Access;
      Source : not null Copy_On_Write.Data_Access;
      Length : Count_Type;
      New_Length : Count_Type;
      Capacity : Count_Type) is
   begin
      Allocate_Data (Target, New_Length, Capacity);
      declare
         subtype R is
            Extended_Index range
               Index_Type'First ..
               Index_Type'First - 1 + Index_Type'Base (Length);
      begin
         for I in R loop
            if Downcast (Source).Items (I) /= null then
               Allocate_Element (
                  Downcast (Target).Items (I),
                  Downcast (Source).Items (I).all);
            end if;
         end loop;
      end;
   end Copy_Data;

   function Max_Length (Data : not null Copy_On_Write.Data_Access)
      return not null access Count_Type;
   function Max_Length (Data : not null Copy_On_Write.Data_Access)
      return not null access Count_Type is
   begin
      return Downcast (Data).Max_Length'Access;
   end Max_Length;

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
         Target_Capacity => Indefinite_Vectors.Capacity (Container),
         New_Length => Length,
         New_Capacity => Capacity,
         To_Update => To_Update,
         Allocate => Allocate_Data'Access,
         Move => Move_Data'Access,
         Copy => Copy_Data'Access,
         Free => Free_Data'Access,
         Max_Length => Max_Length'Access);
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
      elsif Left.Length = 0 or else Left.Super.Data = Right.Super.Data then
         return True;
      else
         Unique (Left'Unrestricted_Access.all, False); -- private
         Unique (Right'Unrestricted_Access.all, False); -- private
         for I in Index_Type'First .. Last (Left) loop
            if Downcast (Left.Super.Data).Items (I) = null then
               if Downcast (Right.Super.Data).Items (I) /= null then
                  return False;
               end if;
            elsif not Equivalent_Element (
               Downcast (Right.Super.Data).Items (I),
               Downcast (Left.Super.Data).Items (I).all)
            then
               return False;
            end if;
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
      Failure : Boolean;
   begin
      Copy_On_Write.In_Place_Set_Length (
         Target => Container.Super'Access,
         Target_Length => Container.Length,
         Target_Capacity => Old_Capacity,
         New_Length => Length,
         Failure => Failure,
         Max_Length => Max_Length'Access);
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
            Index) -- checking Constraint_Error
         .Element.all;
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
         E : Element_Access
            renames Downcast (Container.Super.Data).Items (Position);
      begin
         Free (E);
         Allocate_Element (E, New_Item);
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
      Unique (Container'Unrestricted_Access.all, False);
      declare
         Data : constant Data_Access := Downcast (Container.Super.Data);
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
      Unique (Container, True);
      declare
         Data : constant Data_Access := Downcast (Container.Super.Data);
      begin
         return (Element => Data.Items (Position).all'Access);
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
         New_Item, -- checking Program_Error if same nonempty container
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
      pragma Check (Pre,
         Check =>
            Container'Address /= New_Item'Address
            or else Is_Empty (Container)
            or else raise Program_Error);
            --  same nonempty container (should this case be supported?)
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
            Unique (New_Item'Unrestricted_Access.all, False); -- private
            declare
               D : constant Index_Type'Base := Position - Index_Type'First;
            begin
               for I in
                  Position .. Position + Index_Type'Base (New_Item_Length) - 1
               loop
                  declare
                     E : Element_Access
                        renames Downcast (Container.Super.Data).Items (I);
                     S : Element_Access
                        renames Downcast (New_Item.Super.Data).Items (I - D);
                  begin
                     pragma Check (Validate, E = null);
                     Allocate_Element (E, S.all);
                  end;
               end loop;
            end;
         end if;
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
            E : Element_Access
               renames Downcast (Container.Super.Data).Items (I);
         begin
            pragma Check (Validate, E = null);
            Allocate_Element (E, New_Item);
         end;
      end loop;
   end Insert;

   procedure Prepend (Container : in out Vector; New_Item : Vector) is
   begin
      Insert (
         Container,
         Index_Type'First,
         New_Item); -- checking Program_Error if same nonempty container
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
      Old_Length : constant Count_Type := Container.Length;
   begin
      if Old_Length = 0 and then Capacity (Container) < New_Item_Length then
         Assign (Container, New_Item);
      elsif New_Item_Length > 0 then
         Set_Length (Container, Old_Length + New_Item_Length);
         Unique (New_Item'Unrestricted_Access.all, False); -- private
         declare
            D : constant Index_Type'Base := Index_Type'Base (Old_Length);
         begin
            for I in Index_Type'First + D .. Last (Container) loop
               declare
                  E : Element_Access
                     renames Downcast (Container.Super.Data).Items (I);
                  S : Element_Access
                     renames Downcast (New_Item.Super.Data).Items (I - D);
               begin
                  pragma Check (Validate, E = null);
                  if S /= null then
                     Allocate_Element (E, S.all);
                  end if;
               end;
            end loop;
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
         Last (Container)
      loop
         declare
            E : Element_Access
               renames Downcast (Container.Super.Data).Items (I);
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
            Unique (Container, True);
            declare
               Data : constant Data_Access := Downcast (Container.Super.Data);
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
               Unique (Container, True);
               declare
                  Data : constant Data_Access :=
                     Downcast (Container.Super.Data);
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
      Last : constant Cursor := Indefinite_Vectors.Last (Container);
   begin
      if Position <= Last then
         Unique (Container'Unrestricted_Access.all, False); -- private
         for I in Position .. Last loop
            if Equivalent_Element (
               Downcast (Container.Super.Data).Items (I),
               Item)
            then
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
            if Equivalent_Element (
               Downcast (Container.Super.Data).Items (I),
               Item)
            then
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
            (First <= Indefinite_Vectors.Last (Vector (Container)) + 1
               and then Last <= Indefinite_Vectors.Last (Vector (Container)))
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
                  Unique (Target, True);
                  Unique (Source, True); -- splicing
                  for I in Index_Type'First .. Last (Source) loop
                     Downcast (Target.Super.Data).Items (
                           I + Index_Type'Base (Old_Length)) :=
                        Downcast (Source.Super.Data).Items (I);
                     Downcast (Source.Super.Data).Items (I) := null;
                  end loop;
                  Set_Length (Source, 0);
                  Array_Sorting.In_Place_Merge (
                     Index_Type'Pos (Index_Type'First),
                     Word_Integer (Index_Type'First)
                        + Word_Integer (Old_Length),
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
            for I in Index_Type'First .. Last (Item) loop
               declare
                  E : Element_Access
                     renames Downcast (Item.Super.Data).Items (I);
               begin
                  pragma Check (Validate, E = null);
                  Allocate_Element (E, Element_Type'Input (Stream));
               end;
            end loop;
         end if;
      end Read;

      procedure Write (
         Stream : not null access Streams.Root_Stream_Type'Class;
         Item : Vector)
      is
         Length : constant Count_Type := Indefinite_Vectors.Length (Item);
      begin
         Count_Type'Write (Stream, Length);
         if Length > 0 then
            Unique (Item'Unrestricted_Access.all, False); -- private
            for I in Index_Type'First .. Last (Item) loop
               Element_Type'Output (
                  Stream,
                  Downcast (Item.Super.Data).Items (I).all);
            end loop;
         end if;
      end Write;

   end Streaming;

end Ada.Containers.Indefinite_Vectors;
