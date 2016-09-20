--  diff (Ada.Exceptions.Finally)
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Ada.Streams; -- [gcc-4.7] can not search in private with
with System;
package body Ada.Containers.Hashed_Sets is
   use type Hash_Tables.Table_Access;
   use type Copy_On_Write.Data_Access;

   function Upcast is
      new Unchecked_Conversion (Cursor, Hash_Tables.Node_Access);
   function Downcast is
      new Unchecked_Conversion (Hash_Tables.Node_Access, Cursor);

   function Upcast is
      new Unchecked_Conversion (Data_Access, Copy_On_Write.Data_Access);
   function Downcast is
      new Unchecked_Conversion (Copy_On_Write.Data_Access, Data_Access);

--  diff
   procedure Free is new Unchecked_Deallocation (Node, Cursor);

   type Context_Type is limited record
      Left : not null access Element_Type;
   end record;
   pragma Suppress_Initialization (Context_Type);

   function Equivalent_Element (
      Position : not null Hash_Tables.Node_Access;
      Params : System.Address)
      return Boolean;
   function Equivalent_Element (
      Position : not null Hash_Tables.Node_Access;
      Params : System.Address)
      return Boolean
   is
      Context : Context_Type;
      for Context'Address use Params;
   begin
      return Equivalent_Elements (
         Context.Left.all,
         Downcast (Position).Element);
   end Equivalent_Element;

   function Equivalent_Node (Left, Right : not null Hash_Tables.Node_Access)
      return Boolean;
   function Equivalent_Node (Left, Right : not null Hash_Tables.Node_Access)
      return Boolean is
   begin
      return Equivalent_Elements (
         Downcast (Left).Element,
         Downcast (Right).Element);
   end Equivalent_Node;

--  diff (Allocate_Element)
--
--
--
--
--
--
--
--

   procedure Allocate_Node (Item : out Cursor; New_Item : Element_Type);
   procedure Allocate_Node (Item : out Cursor; New_Item : Element_Type) is
--  diff
--  diff
--  diff
   begin
      Item := new Node'(Super => <>, Element => New_Item);
--  diff
--  diff
--  diff
   end Allocate_Node;

   procedure Copy_Node (
      Target : out Hash_Tables.Node_Access;
      Source : not null Hash_Tables.Node_Access);
   procedure Copy_Node (
      Target : out Hash_Tables.Node_Access;
      Source : not null Hash_Tables.Node_Access)
   is
      New_Node : Cursor;
   begin
      Allocate_Node (New_Node, Downcast (Source).Element);
      Target := Upcast (New_Node);
   end Copy_Node;

   procedure Free_Node (Object : in out Hash_Tables.Node_Access);
   procedure Free_Node (Object : in out Hash_Tables.Node_Access) is
      X : Cursor := Downcast (Object);
   begin
--  diff
      Free (X);
      Object := null;
   end Free_Node;

   procedure Allocate_Data (
      Target : out not null Copy_On_Write.Data_Access;
      Max_Length : Count_Type;
      Capacity : Count_Type);
   procedure Allocate_Data (
      Target : out not null Copy_On_Write.Data_Access;
      Max_Length : Count_Type;
      Capacity : Count_Type)
   is
      pragma Unreferenced (Max_Length);
      New_Data : constant Data_Access :=
         new Data'(Super => <>, Table => null, Length => 0);
   begin
      Hash_Tables.Rebuild (New_Data.Table, Capacity);
      Target := Upcast (New_Data);
   end Allocate_Data;

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
      Capacity : Count_Type)
   is
      pragma Unreferenced (Length);
      pragma Unreferenced (Max_Length);
      New_Data : constant Data_Access :=
         new Data'(Super => <>, Table => null, Length => 0);
   begin
      Hash_Tables.Copy (
         New_Data.Table,
         New_Data.Length,
         Downcast (Source).Table,
         Capacity,
         Copy => Copy_Node'Access);
      Target := Upcast (New_Data);
   end Copy_Data;

   procedure Free is new Unchecked_Deallocation (Data, Data_Access);

   procedure Free_Data (Data : in out Copy_On_Write.Data_Access);
   procedure Free_Data (Data : in out Copy_On_Write.Data_Access) is
      X : Data_Access := Downcast (Data);
   begin
      Hash_Tables.Free (X.Table, X.Length, Free => Free_Node'Access);
      Free (X);
      Data := null;
   end Free_Data;

   procedure Reallocate (
      Container : in out Set;
      Capacity : Count_Type;
      To_Update : Boolean);
   procedure Reallocate (
      Container : in out Set;
      Capacity : Count_Type;
      To_Update : Boolean) is
   begin
      Copy_On_Write.Unique (
         Target => Container.Super'Access,
         Target_Length => 0, -- Length is unused
         Target_Capacity => Hashed_Sets.Capacity (Container),
         New_Length => 0,
         New_Capacity => Capacity,
         To_Update => To_Update,
         Allocate => Allocate_Data'Access,
         Move => Copy_Data'Access,
         Copy => Copy_Data'Access,
         Free => Free_Data'Access);
   end Reallocate;

   procedure Unique (Container : in out Set; To_Update : Boolean);
   procedure Unique (Container : in out Set; To_Update : Boolean) is
   begin
      if Copy_On_Write.Shared (Container.Super.Data) then
         Reallocate (
            Container,
            Capacity (Container), -- not shrinking
            To_Update);
      end if;
   end Unique;

   function Equivalent_Sets (
      Left, Right : Set;
      Equivalent : not null access function (
         Left, Right : not null Hash_Tables.Node_Access)
         return Boolean)
      return Boolean;
   function Equivalent_Sets (
      Left, Right : Set;
      Equivalent : not null access function (
         Left, Right : not null Hash_Tables.Node_Access)
         return Boolean)
      return Boolean
   is
      Left_Length : constant Count_Type := Length (Left);
      Right_Length : constant Count_Type := Length (Right);
   begin
      if Left_Length /= Right_Length then
         return False;
      elsif Left_Length = 0 or else Left.Super.Data = Right.Super.Data then
         return True;
      else
         Unique (Left'Unrestricted_Access.all, False); -- private
         Unique (Right'Unrestricted_Access.all, False); -- private
         declare
            Left_Data : constant Data_Access := Downcast (Left.Super.Data);
            Right_Data : constant Data_Access := Downcast (Right.Super.Data);
         begin
            return Hash_Tables.Equivalent (
               Left_Data.Table,
               Left_Data.Length,
               Right_Data.Table,
               Right_Data.Length,
               Equivalent => Equivalent);
         end;
      end if;
   end Equivalent_Sets;

   function Find (Container : Set; Hash : Hash_Type; Item : Element_Type)
      return Cursor;
   function Find (Container : Set; Hash : Hash_Type; Item : Element_Type)
      return Cursor is
   begin
      if Is_Empty (Container) then
         return null;
      else
         Unique (Container'Unrestricted_Access.all, False);
         declare
            Context : Context_Type := (Left => Item'Unrestricted_Access);
         begin
            return Downcast (Hash_Tables.Find (
               Downcast (Container.Super.Data).Table,
               Hash,
               Context'Address,
               Equivalent => Equivalent_Element'Access));
         end;
      end if;
   end Find;

   --  implementation

   function Empty_Set return Set is
   begin
      return (Finalization.Controlled with Super => (null, null));
   end Empty_Set;

   function Has_Element (Position : Cursor) return Boolean is
   begin
      return Position /= null;
   end Has_Element;

   overriding function "=" (Left, Right : Set) return Boolean is
      function Equivalent (Left, Right : not null Hash_Tables.Node_Access)
         return Boolean;
      function Equivalent (Left, Right : not null Hash_Tables.Node_Access)
         return Boolean is
      begin
         return Downcast (Left).Element = Downcast (Right).Element;
      end Equivalent;
   begin
      return Equivalent_Sets (Left, Right, Equivalent => Equivalent'Access);
   end "=";

   function Equivalent_Sets (Left, Right : Set) return Boolean is
   begin
      return Equivalent_Sets (Left, Right,
         Equivalent => Equivalent_Node'Access);
   end Equivalent_Sets;

   function To_Set (New_Item : Element_Type) return Set is
   begin
      return Result : Set do
         Insert (Result, New_Item);
      end return;
   end To_Set;

   function Generic_Array_To_Set (S : Element_Array) return Set is
   begin
      return Result : Set do
         Reallocate (Result, S'Length, True);
         for I in S'Range loop
            Insert (Result, S (I));
         end loop;
      end return;
   end Generic_Array_To_Set;

   function Capacity (Container : Set) return Count_Type is
      Data : constant Data_Access := Downcast (Container.Super.Data);
   begin
      if Data = null then
         return 0;
      else
         return Hash_Tables.Capacity (Data.Table);
      end if;
   end Capacity;

   procedure Reserve_Capacity (
      Container : in out Set;
      Capacity : Count_Type)
   is
      New_Capacity : constant Count_Type :=
         Count_Type'Max (Capacity, Length (Container));
   begin
      Reallocate (Container, New_Capacity, True);
   end Reserve_Capacity;

   function Length (Container : Set) return Count_Type is
      Data : constant Data_Access := Downcast (Container.Super.Data);
   begin
      if Data = null then
         return 0;
      else
         return Data.Length;
      end if;
   end Length;

   function Is_Empty (Container : Set) return Boolean is
      Data : constant Data_Access := Downcast (Container.Super.Data);
   begin
      return Data = null or else Data.Length = 0;
   end Is_Empty;

   procedure Clear (Container : in out Set) is
   begin
      Copy_On_Write.Clear (Container.Super'Access, Free => Free_Data'Access);
   end Clear;

   function Element (Position : Cursor) return Element_Type is
   begin
      return Position.Element;
   end Element;

   procedure Replace_Element (
      Container : in out Set;
      Position : Cursor;
      New_Item : Element_Type) is
   begin
      Unique (Container, True);
--  diff
      Position.Element := New_Item;
   end Replace_Element;

   procedure Query_Element (
      Position : Cursor;
      Process : not null access procedure (Element : Element_Type)) is
   begin
      Process (Position.Element);
   end Query_Element;

   function Constant_Reference (Container : aliased Set; Position : Cursor)
      return Constant_Reference_Type
   is
      pragma Unreferenced (Container);
   begin
      return (Element => Position.all.Element'Access); -- [gcc-6] .all
   end Constant_Reference;

   procedure Assign (Target : in out Set; Source : Set) is
   begin
      Copy_On_Write.Assign (
         Target.Super'Access,
         Source.Super'Access,
         Free => Free_Data'Access);
   end Assign;

   function Copy (Source : Set; Capacity : Count_Type := 0) return Set is
   begin
      return Result : Set do
         Copy_On_Write.Copy (
            Result.Super'Access,
            Source.Super'Access,
            0, -- Length is unused
            Count_Type'Max (Capacity, Length (Source)),
            Allocate => Allocate_Data'Access,
            Copy => Copy_Data'Access);
      end return;
   end Copy;

   procedure Move (Target : in out Set; Source : in out Set) is
   begin
      Copy_On_Write.Move (
         Target.Super'Access,
         Source.Super'Access,
         Free => Free_Data'Access);
--  diff
   end Move;

   procedure Insert (
      Container : in out Set;
      New_Item : Element_Type;
      Position : out Cursor;
      Inserted : out Boolean)
   is
--  diff
--  diff
--  diff
      New_Hash : constant Hash_Type := Hash (New_Item);
   begin
--  diff
--  diff
      Position := Find (Container, New_Hash, New_Item);
      Inserted := Position = null;
      if Inserted then
         Unique (Container, True);
         Allocate_Node (Position, New_Item);
         declare
            Data : constant Data_Access := Downcast (Container.Super.Data);
         begin
            Hash_Tables.Insert (
               Data.Table,
               Data.Length,
               New_Hash,
               Upcast (Position));
         end;
      end if;
   end Insert;

   procedure Insert (
      Container : in out Set;
      New_Item : Element_Type)
   is
      Position : Cursor;
      Inserted : Boolean;
   begin
      Insert (Container, New_Item, Position, Inserted);
      if not Inserted then
         raise Constraint_Error;
      end if;
   end Insert;

   procedure Include (Container : in out Set; New_Item : Element_Type) is
      Position : Cursor;
      Inserted : Boolean;
   begin
      Insert (Container, New_Item, Position, Inserted);
      if not Inserted then
         Replace_Element (Container, Position, New_Item);
      end if;
   end Include;

   procedure Replace (Container : in out Set; New_Item : Element_Type) is
   begin
      Replace_Element (Container, Find (Container, New_Item), New_Item);
   end Replace;

   procedure Exclude (Container : in out Set; Item : Element_Type) is
      Position : Cursor := Find (Container, Item);
   begin
      if Position /= null then
         Delete (Container, Position);
      end if;
   end Exclude;

   procedure Delete (Container : in out Set; Item : Element_Type) is
      Position : Cursor := Find (Container, Item);
   begin
      Delete (Container, Position);
   end Delete;

   procedure Delete (Container : in out Set; Position : in out Cursor) is
      Position_2 : Hash_Tables.Node_Access := Upcast (Position);
   begin
      Unique (Container, True);
      declare
         Data : constant Data_Access := Downcast (Container.Super.Data);
      begin
         Hash_Tables.Remove (Data.Table, Data.Length, Position_2);
      end;
      Free_Node (Position_2);
      Position := null;
   end Delete;

   procedure Union (Target : in out Set; Source : Set) is
   begin
      if not Is_Empty (Source) then
         if Is_Empty (Target) then
            Assign (Target, Source);
         else
            Unique (Target, True);
            declare
               Target_Data : constant Data_Access :=
                  Downcast (Target.Super.Data);
               Source_Data : constant Data_Access :=
                  Downcast (Source.Super.Data);
            begin
               Hash_Tables.Merge (
                  Target_Data.Table,
                  Target_Data.Length,
                  Source_Data.Table,
                  Source_Data.Length,
                  (others => True),
                  Equivalent => Equivalent_Node'Access,
                  Copy => Copy_Node'Access,
                  Free => Free_Node'Access);
            end;
         end if;
      end if;
   end Union;

   function Union (Left, Right : Set) return Set is
   begin
      return Result : Set do
         if Is_Empty (Left) then
            Assign (Result, Right);
         elsif Is_Empty (Right) then
            Assign (Result, Left);
         else
            Unique (Result, True);
            declare
               Result_Data : constant Data_Access :=
                  Downcast (Result.Super.Data);
               Left_Data : constant Data_Access := Downcast (Left.Super.Data);
               Right_Data : constant Data_Access :=
                  Downcast (Right.Super.Data);
            begin
               Hash_Tables.Copying_Merge (
                  Result_Data.Table,
                  Result_Data.Length,
                  Left_Data.Table,
                  Left_Data.Length,
                  Right_Data.Table,
                  Right_Data.Length,
                  (others => True),
                  Equivalent => Equivalent_Node'Access,
                  Copy => Copy_Node'Access);
            end;
         end if;
      end return;
   end Union;

   procedure Intersection (Target : in out Set; Source : Set) is
   begin
      if not Is_Empty (Target) then
         if Is_Empty (Source) then
            Clear (Target);
         else
            Unique (Target, True);
            declare
               Target_Data : constant Data_Access :=
                  Downcast (Target.Super.Data);
               Source_Data : constant Data_Access :=
                  Downcast (Source.Super.Data);
            begin
               Hash_Tables.Merge (
                  Target_Data.Table,
                  Target_Data.Length,
                  Source_Data.Table,
                  Source_Data.Length,
                  (Hash_Tables.In_Both => True, others => False),
                  Equivalent => Equivalent_Node'Access,
                  Copy => Copy_Node'Access,
                  Free => Free_Node'Access);
            end;
         end if;
      end if;
   end Intersection;

   function Intersection (Left, Right : Set) return Set is
   begin
      return Result : Set do
         if Is_Empty (Left) or else Is_Empty (Right) then
            null; -- Empty_Set
         else
            Unique (Result, True);
            declare
               Result_Data : constant Data_Access :=
                  Downcast (Result.Super.Data);
               Left_Data : constant Data_Access := Downcast (Left.Super.Data);
               Right_Data : constant Data_Access :=
                  Downcast (Right.Super.Data);
            begin
               Hash_Tables.Copying_Merge (
                  Result_Data.Table,
                  Result_Data.Length,
                  Left_Data.Table,
                  Left_Data.Length,
                  Right_Data.Table,
                  Right_Data.Length,
                  (Hash_Tables.In_Both => True, others => False),
                  Equivalent => Equivalent_Node'Access,
                  Copy => Copy_Node'Access);
            end;
         end if;
      end return;
   end Intersection;

   procedure Difference (Target : in out Set; Source : Set) is
   begin
      if not Is_Empty (Target) and then not Is_Empty (Source) then
         Unique (Target, True);
         declare
            Target_Data : constant Data_Access := Downcast (Target.Super.Data);
            Source_Data : constant Data_Access := Downcast (Source.Super.Data);
         begin
            Hash_Tables.Merge (
               Target_Data.Table,
               Target_Data.Length,
               Source_Data.Table,
               Source_Data.Length,
               (Hash_Tables.In_Only_Left => True, others => False),
               Equivalent => Equivalent_Node'Access,
               Copy => Copy_Node'Access,
               Free => Free_Node'Access);
         end;
      end if;
   end Difference;

   function Difference (Left, Right : Set) return Set is
   begin
      return Result : Set do
         if Is_Empty (Left) or else Is_Empty (Right) then
            Assign (Result, Left);
         else
            Unique (Result, True);
            declare
               Result_Data : constant Data_Access :=
                  Downcast (Result.Super.Data);
               Left_Data : constant Data_Access := Downcast (Left.Super.Data);
               Right_Data : constant Data_Access :=
                  Downcast (Right.Super.Data);
            begin
               Hash_Tables.Copying_Merge (
                  Result_Data.Table,
                  Result_Data.Length,
                  Left_Data.Table,
                  Left_Data.Length,
                  Right_Data.Table,
                  Right_Data.Length,
                  (Hash_Tables.In_Only_Left => True, others => False),
                  Equivalent => Equivalent_Node'Access,
                  Copy => Copy_Node'Access);
            end;
         end if;
      end return;
   end Difference;

   procedure Symmetric_Difference (Target : in out Set; Source : Set) is
   begin
      if not Is_Empty (Source) then
         if Is_Empty (Target) then
            Assign (Target, Source);
         else
            Unique (Target, True);
            declare
               Target_Data : constant Data_Access :=
                  Downcast (Target.Super.Data);
               Source_Data : constant Data_Access :=
                  Downcast (Source.Super.Data);
            begin
               Hash_Tables.Merge (
                  Target_Data.Table,
                  Target_Data.Length,
                  Source_Data.Table,
                  Source_Data.Length,
                  (Hash_Tables.In_Both => False, others => True),
                  Equivalent => Equivalent_Node'Access,
                  Copy => Copy_Node'Access,
                  Free => Free_Node'Access);
            end;
         end if;
      end if;
   end Symmetric_Difference;

   function Symmetric_Difference (Left, Right : Set) return Set is
   begin
      return Result : Set do
         if Is_Empty (Left) then
            Assign (Result, Right);
         elsif Is_Empty (Right) then
            Assign (Result, Left);
         else
            Unique (Result, True);
            declare
               Result_Data : constant Data_Access :=
                  Downcast (Result.Super.Data);
               Left_Data : constant Data_Access := Downcast (Left.Super.Data);
               Right_Data : constant Data_Access :=
                  Downcast (Right.Super.Data);
            begin
               Hash_Tables.Copying_Merge (
                  Result_Data.Table,
                  Result_Data.Length,
                  Left_Data.Table,
                  Left_Data.Length,
                  Right_Data.Table,
                  Right_Data.Length,
                  (Hash_Tables.In_Both => False, others => True),
                  Equivalent => Equivalent_Node'Access,
                  Copy => Copy_Node'Access);
            end;
         end if;
      end return;
   end Symmetric_Difference;

   function Overlap (Left, Right : Set) return Boolean is
   begin
      if Is_Empty (Left) or else Is_Empty (Right) then
         return False;
      elsif Left.Super.Data = Right.Super.Data then
         return True;
      else
         Unique (Left'Unrestricted_Access.all, False); -- private
         Unique (Right'Unrestricted_Access.all, False); -- private
         return Hash_Tables.Overlap (
            Downcast (Left.Super.Data).Table,
            Downcast (Right.Super.Data).Table,
            Equivalent => Equivalent_Node'Access);
      end if;
   end Overlap;

   function Is_Subset (Subset : Set; Of_Set : Set) return Boolean is
   begin
      if Is_Empty (Subset) or else Subset.Super.Data = Of_Set.Super.Data then
         return True;
      elsif Is_Empty (Of_Set) then
         return False;
      else
         Unique (Subset'Unrestricted_Access.all, False); -- private
         Unique (Of_Set'Unrestricted_Access.all, False); -- private
         return Hash_Tables.Is_Subset (
            Downcast (Subset.Super.Data).Table,
            Downcast (Of_Set.Super.Data).Table,
            Equivalent => Equivalent_Node'Access);
      end if;
   end Is_Subset;

   function First (Container : Set) return Cursor is
   begin
      if Is_Empty (Container) then
         return null;
      else
         Unique (Container'Unrestricted_Access.all, False);
         return Downcast (Hash_Tables.First (
            Downcast (Container.Super.Data).Table));
      end if;
   end First;

   function Next (Position : Cursor) return Cursor is
   begin
      return Downcast (Position.Super.Next);
   end Next;

   procedure Next (Position : in out Cursor) is
   begin
      Position := Downcast (Position.Super.Next);
   end Next;

   function Find (Container : Set; Item : Element_Type) return Cursor is
   begin
      return Find (Container, Hash (Item), Item);
   end Find;

   function Contains (Container : Set; Item : Element_Type) return Boolean is
   begin
      return Find (Container, Item) /= null;
   end Contains;

   function Equivalent_Elements (Left, Right : Cursor)
      return Boolean is
   begin
      return Equivalent_Elements (Left.Element, Right.Element);
   end Equivalent_Elements;

   function Equivalent_Elements (Left : Cursor; Right : Element_Type)
      return Boolean is
   begin
      return Equivalent_Elements (Left.Element, Right);
   end Equivalent_Elements;

   procedure Iterate (
      Container : Set'Class;
      Process : not null access procedure (Position : Cursor))
   is
      type P1 is access procedure (Position : Cursor);
      type P2 is access procedure (Position : Hash_Tables.Node_Access);
      function Cast is new Unchecked_Conversion (P1, P2);
   begin
      if not Is_Empty (Set (Container)) then
         Unique (Set (Container)'Unrestricted_Access.all, False);
         Hash_Tables.Iterate (
            Downcast (Container.Super.Data).Table,
            Cast (Process));
      end if;
   end Iterate;

   function Iterate (Container : Set'Class)
      return Set_Iterator_Interfaces.Forward_Iterator'Class is
   begin
      return Set_Iterator'(First => First (Set (Container)));
   end Iterate;

   overriding procedure Adjust (Object : in out Set) is
   begin
      Copy_On_Write.Adjust (Object.Super'Access);
   end Adjust;

   overriding function First (Object : Set_Iterator) return Cursor is
   begin
      return Object.First;
   end First;

   overriding function Next (Object : Set_Iterator; Position : Cursor)
      return Cursor
   is
      pragma Unreferenced (Object);
   begin
      return Next (Position);
   end Next;

   package body Generic_Keys is

      type Context_Type is limited record
         Left : not null access Key_Type;
      end record;
      pragma Suppress_Initialization (Context_Type);

      function Equivalent_Key (
         Position : not null Hash_Tables.Node_Access;
         Params : System.Address)
         return Boolean;
      function Equivalent_Key (
         Position : not null Hash_Tables.Node_Access;
         Params : System.Address)
         return Boolean
      is
         Context : Context_Type;
         for Context'Address use Params;
      begin
         return Equivalent_Keys (
            Context.Left.all,
            Key (Downcast (Position).Element));
      end Equivalent_Key;

      function Key (Position : Cursor) return Key_Type is
      begin
         return Key (Position.Element);
      end Key;

      function Element (Container : Set; Key : Key_Type) return Element_Type is
      begin
         return Element (Find (Container, Key));
      end Element;

      procedure Replace (
         Container : in out Set;
         Key : Key_Type;
         New_Item : Element_Type) is
      begin
         Replace_Element (Container, Find (Container, Key), New_Item);
      end Replace;

      procedure Exclude (Container : in out Set; Key : Key_Type) is
         Position : Cursor := Find (Container, Key);
      begin
         if Position /= null then
            Delete (Container, Position);
         end if;
      end Exclude;

      procedure Delete (Container : in out Set; Key : Key_Type) is
         Position : Cursor := Find (Container, Key);
      begin
         Delete (Container, Position);
      end Delete;

      function Find (Container : Set; Key : Key_Type) return Cursor is
      begin
         if Is_Empty (Container) then
            return null;
         else
            Unique (Container'Unrestricted_Access.all, False);
            declare
               Context : Context_Type := (Left => Key'Unrestricted_Access);
            begin
               return Downcast (Hash_Tables.Find (
                  Downcast (Container.Super.Data).Table,
                  Hash (Key),
                  Context'Address,
                  Equivalent => Equivalent_Key'Access));
            end;
         end if;
      end Find;

      function Contains (Container : Set; Key : Key_Type) return Boolean is
      begin
         return Find (Container, Key) /= null;
      end Contains;

      procedure Update_Element_Preserving_Key (
         Container : in out Set;
         Position : Cursor;
         Process : not null access procedure (
            Element : in out Element_Type)) is
      begin
         Process (Reference_Preserving_Key (Container, Position).Element.all);
      end Update_Element_Preserving_Key;

      function Reference_Preserving_Key (
         Container : aliased in out Set;
         Position : Cursor)
         return Reference_Type is
      begin
         Unique (Container, True);
--  diff
         return (Element => Position.all.Element'Access); -- [gcc-6] .all
      end Reference_Preserving_Key;

      function Constant_Reference (Container : aliased Set; Key : Key_Type)
         return Constant_Reference_Type is
      begin
         return Constant_Reference (Container, Find (Container, Key));
      end Constant_Reference;

      function Reference_Preserving_Key (
         Container : aliased in out Set;
         Key : Key_Type)
         return Reference_Type is
      begin
         return Reference_Preserving_Key (Container, Find (Container, Key));
      end Reference_Preserving_Key;

   end Generic_Keys;

   package body Streaming is

      procedure Read (
         Stream : not null access Streams.Root_Stream_Type'Class;
         Item : out Set)
      is
         Length : Count_Type'Base;
      begin
         Count_Type'Base'Read (Stream, Length);
         Clear (Item);
         for I in 1 .. Length loop
            declare
               New_Item : Element_Type;
            begin
               Element_Type'Read (Stream, New_Item);
               Include (Item, New_Item);
            end;
         end loop;
      end Read;

      procedure Write (
         Stream : not null access Streams.Root_Stream_Type'Class;
         Item : Set)
      is
         Length : constant Count_Type := Hashed_Sets.Length (Item);
      begin
         Count_Type'Write (Stream, Length);
         if Length > 0 then
            declare
               Position : Cursor := Position := First (Item);
            begin
               while Position /= null loop
                  Element_Type'Write (Stream, Position.Element);
                  Next (Position);
               end loop;
            end;
         end if;
      end Write;

   end Streaming;

end Ada.Containers.Hashed_Sets;
