with Ada.Exceptions.Finally;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System;
package body Ada.Containers.Indefinite_Doubly_Linked_Lists is
   use type Linked_Lists.Node_Access;
   use type Copy_On_Write.Data_Access;
   use type System.Address;

   function Upcast is
      new Unchecked_Conversion (Cursor, Linked_Lists.Node_Access);
   function Downcast is
      new Unchecked_Conversion (Linked_Lists.Node_Access, Cursor);

   function Upcast is
      new Unchecked_Conversion (Data_Access, Copy_On_Write.Data_Access);
   function Downcast is
      new Unchecked_Conversion (Copy_On_Write.Data_Access, Data_Access);

   procedure Free is new Unchecked_Deallocation (Node, Cursor);
   procedure Free is new Unchecked_Deallocation (Element_Type, Element_Access);

   type Context_Type is limited record
      Left : not null access Element_Type;
   end record;
   pragma Suppress_Initialization (Context_Type);

   function Equivalent_Element (
      Right : not null Linked_Lists.Node_Access;
      Params : System.Address)
      return Boolean;
   function Equivalent_Element (
      Right : not null Linked_Lists.Node_Access;
      Params : System.Address)
      return Boolean
   is
      Context : Context_Type;
      for Context'Address use Params;
   begin
      return Context.Left.all = Downcast (Right).Element.all;
   end Equivalent_Element;

   procedure Allocate_Element (
      Item : out not null Element_Access;
      New_Item : Element_Type);
   procedure Allocate_Element (
      Item : out not null Element_Access;
      New_Item : Element_Type) is
   begin
      Item := new Element_Type'(New_Item);
   end Allocate_Element;

   procedure Allocate_Node (Item : out Cursor; New_Item : Element_Type);
   procedure Allocate_Node (Item : out Cursor; New_Item : Element_Type) is
      package Holder is
         new Exceptions.Finally.Scoped_Holder (Cursor, Free);
      X : aliased Cursor := new Node;
   begin
      Holder.Assign (X);
      Allocate_Element (X.Element, New_Item);
      Holder.Clear;
      Item := X;
   end Allocate_Node;

   procedure Copy_Node (
      Target : out Linked_Lists.Node_Access;
      Source : not null Linked_Lists.Node_Access);
   procedure Copy_Node (
      Target : out Linked_Lists.Node_Access;
      Source : not null Linked_Lists.Node_Access)
   is
      New_Node : Cursor;
   begin
      Allocate_Node (New_Node, Downcast (Source).Element.all);
      Target := Upcast (New_Node);
   end Copy_Node;

   procedure Free_Node (Object : in out Linked_Lists.Node_Access);
   procedure Free_Node (Object : in out Linked_Lists.Node_Access) is
      X : Cursor := Downcast (Object);
   begin
      Free (X.Element);
      Free (X);
      Object := null;
   end Free_Node;

   procedure Allocate_Data (
      Target : out not null Copy_On_Write.Data_Access;
      New_Length : Count_Type;
      Capacity : Count_Type);
   procedure Allocate_Data (
      Target : out not null Copy_On_Write.Data_Access;
      New_Length : Count_Type;
      Capacity : Count_Type)
   is
      pragma Unreferenced (New_Length);
      pragma Unreferenced (Capacity);
      New_Data : constant Data_Access :=
         new Data'(Super => <>, First => null, Last => null, Length => 0);
   begin
      Target := Upcast (New_Data);
   end Allocate_Data;

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
      Capacity : Count_Type)
   is
      pragma Unreferenced (Length);
      pragma Unreferenced (New_Length);
      pragma Unreferenced (Capacity);
   begin
      Allocate_Data (Target, 0, 0);
      Base.Copy (
         Downcast (Target).First,
         Downcast (Target).Last,
         Downcast (Target).Length,
         Source_Last => Downcast (Source).Last,
         Copy => Copy_Node'Access);
   end Copy_Data;

   procedure Free is new Unchecked_Deallocation (Data, Data_Access);

   procedure Free_Data (Data : in out Copy_On_Write.Data_Access);
   procedure Free_Data (Data : in out Copy_On_Write.Data_Access) is
      X : Data_Access := Downcast (Data);
   begin
      Linked_Lists.Free (
         X.First,
         X.Last,
         X.Length,
         Free => Free_Node'Access);
      Free (X);
      Data := null;
   end Free_Data;

   procedure Unique (Container : in out List; To_Update : Boolean);
   procedure Unique (Container : in out List; To_Update : Boolean) is
   begin
      if Copy_On_Write.Shared (Container.Super.Data) then
         Copy_On_Write.Unique (
            Target => Container.Super'Access,
            To_Update => To_Update,
            Allocate => Allocate_Data'Access,
            Move => Copy_Data'Access,
            Copy => Copy_Data'Access,
            Free => Free_Data'Access);
      end if;
   end Unique;

   --  implementation

   function Empty_List return List is
   begin
      return (Finalization.Controlled with Super => (null, null));
   end Empty_List;

   function Has_Element (Position : Cursor) return Boolean is
   begin
      return Position /= null;
   end Has_Element;

   overriding function "=" (Left, Right : List) return Boolean is
      function Equivalent (Left, Right : not null Linked_Lists.Node_Access)
         return Boolean;
      function Equivalent (Left, Right : not null Linked_Lists.Node_Access)
         return Boolean is
      begin
         return Downcast (Left).Element.all = Downcast (Right).Element.all;
      end Equivalent;
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
         return Linked_Lists.Equivalent (
            Downcast (Left.Super.Data).Last,
            Downcast (Right.Super.Data).Last,
            Equivalent => Equivalent'Access);
      end if;
   end "=";

   function Length (Container : List) return Count_Type is
      Data : constant Data_Access := Downcast (Container.Super.Data);
   begin
      if Data = null then
         return 0;
      else
         return Data.Length;
      end if;
   end Length;

   function Is_Empty (Container : List) return Boolean is
      Data : constant Data_Access := Downcast (Container.Super.Data);
   begin
      return Data = null or else Data.Last = null;
   end Is_Empty;

   procedure Clear (Container : in out List) is
   begin
      Copy_On_Write.Clear (Container.Super'Access, Free => Free_Data'Access);
   end Clear;

   function Element (Position : Cursor) return Element_Type is
   begin
      return Position.Element.all;
   end Element;

   procedure Replace_Element (
      Container : in out List;
      Position : Cursor;
      New_Item : Element_Type) is
   begin
      Unique (Container, True);
      Free (Position.Element);
      Allocate_Element (Position.Element, New_Item);
   end Replace_Element;

   procedure Query_Element (
      Position : Cursor;
      Process : not null access procedure (Element : Element_Type)) is
   begin
      Process (Position.Element.all);
   end Query_Element;

   procedure Update_Element (
      Container : in out List'Class;
      Position : Cursor;
      Process : not null access procedure (Element : in out Element_Type)) is
   begin
      Process (Reference (List (Container), Position).Element.all);
   end Update_Element;

   function Constant_Reference (Container : aliased List; Position : Cursor)
      return Constant_Reference_Type
   is
      pragma Unreferenced (Container);
   begin
      return (Element => Position.Element.all'Access);
   end Constant_Reference;

   function Reference (Container : aliased in out List; Position : Cursor)
      return Reference_Type is
   begin
--  diff
      Unique (Container, True);
      return (Element => Position.Element.all'Access);
   end Reference;

   procedure Assign (Target : in out List; Source : List) is
   begin
      Copy_On_Write.Assign (
         Target.Super'Access,
         Source.Super'Access,
         Free => Free_Data'Access);
   end Assign;

   function Copy (Source : List) return List is
   begin
      return Result : List do
         Copy_On_Write.Copy (
            Result.Super'Access,
            Source.Super'Access,
            Allocate => Allocate_Data'Access,
            Copy => Copy_Data'Access);
      end return;
   end Copy;

   procedure Move (Target : in out List; Source : in out List) is
   begin
      Copy_On_Write.Move (
         Target.Super'Access,
         Source.Super'Access,
         Free => Free_Data'Access);
--  diff
--  diff
--  diff
--  diff
--  diff
   end Move;

   procedure Insert (
      Container : in out List;
      Before : Cursor;
      New_Item : Element_Type;
      Count : Count_Type := 1)
   is
      Position : Cursor;
   begin
      Insert (Container, Before, New_Item, Position, Count);
   end Insert;

   procedure Insert (
      Container : in out List;
      Before : Cursor;
      New_Item : Element_Type;
      Position : out Cursor;
      Count : Count_Type := 1) is
   begin
      Position := Before;
      if Count > 0 then
         Unique (Container, True);
         for I in 1 .. Count loop
            declare
               Data : constant Data_Access := Downcast (Container.Super.Data);
--  diff
               New_Node : Cursor;
            begin
               Allocate_Node (New_Node, New_Item);
--  diff
--  diff
               Base.Insert (
                  Data.First,
                  Data.Last,
                  Data.Length,
                  Before => Upcast (Position),
                  New_Item => Upcast (New_Node));
               Position := New_Node;
            end;
         end loop;
      end if;
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
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--

   procedure Prepend (
      Container : in out List;
      New_Item : Element_Type;
      Count : Count_Type := 1) is
   begin
      Insert (Container, First (Container), New_Item, Count);
   end Prepend;

   procedure Append (
      Container : in out List;
      New_Item : Element_Type;
      Count : Count_Type := 1) is
   begin
      Insert (Container, null, New_Item, Count);
   end Append;

   procedure Delete (
      Container : in out List;
      Position : in out Cursor;
      Count : Count_Type := 1) is
   begin
      if Count > 0 then
         Unique (Container, True);
         for I in 1 .. Count loop
            declare
               Data : constant Data_Access := Downcast (Container.Super.Data);
               X : Linked_Lists.Node_Access;
               Next : Linked_Lists.Node_Access;
            begin
               X := Upcast (Position);
               Next := Position.Super.Next;
               Base.Remove (
                  Data.First,
                  Data.Last,
                  Data.Length,
                  Position => X,
                  Next => Next);
               Free_Node (X);
               Position := Downcast (Next);
            end;
         end loop;
         Position := No_Element;
      end if;
   end Delete;

   procedure Delete_First (
      Container : in out List'Class;
      Count : Count_Type := 1)
   is
      Position : Cursor := First (List (Container));
   begin
      Delete (List (Container), Position, Count);
   end Delete_First;

   procedure Delete_Last (
      Container : in out List'Class;
      Count : Count_Type := 1)
   is
      Position : Cursor := Last (List (Container));
   begin
      for I in 1 .. Count loop
         declare
            Previous_Position : constant Cursor := Previous (Position);
         begin
            Delete (List (Container), Position);
            Position := Previous_Position;
         end;
      end loop;
   end Delete_Last;

   procedure Reverse_Elements (Container : in out List) is
   begin
      if Length (Container) > 1 then
         Unique (Container, True);
         declare
            Data : constant Data_Access := Downcast (Container.Super.Data);
         begin
            Base.Reverse_Elements (
               Data.First,
               Data.Last,
               Data.Length);
         end;
      end if;
   end Reverse_Elements;

   procedure Swap (Container : in out List; I, J : Cursor) is
   begin
      Unique (Container, True);
      declare
         Temp : constant Element_Access := I.Element;
      begin
         I.Element := J.Element;
         J.Element := Temp;
      end;
   end Swap;

   procedure Swap_Links (Container : in out List; I, J : Cursor) is
   begin
      Unique (Container, True);
      declare
         Data : constant Data_Access := Downcast (Container.Super.Data);
      begin
         Base.Swap_Links (
            Data.First,
            Data.Last,
            Upcast (I),
            Upcast (J));
      end;
   end Swap_Links;

   procedure Splice (
      Target : in out List;
      Before : Cursor;
      Source : in out List) is
   begin
      if Target'Address /= Source'Address then
         Unique (Target, True);
         Unique (Source, True);
         declare
            Target_Data : constant Data_Access := Downcast (Target.Super.Data);
            Source_Data : constant Data_Access := Downcast (Source.Super.Data);
         begin
            Base.Splice (
               Target_Data.First,
               Target_Data.Last,
               Target_Data.Length,
               Upcast (Before),
               Source_Data.First,
               Source_Data.Last,
               Source_Data.Length);
         end;
      end if;
   end Splice;

   procedure Splice (
      Target : in out List;
      Before : Cursor;
      Source : in out List;
      Position : in out Cursor) is
   begin
      if Before /= Position then -- RM A.18.3(114/3)
         Unique (Target, True);
         Unique (Source, True);
         declare
            Target_Data : constant Data_Access := Downcast (Target.Super.Data);
            Source_Data : constant Data_Access := Downcast (Source.Super.Data);
         begin
            Base.Remove (
               Source_Data.First,
               Source_Data.Last,
               Source_Data.Length,
               Upcast (Position),
               Position.Super.Next);
            Base.Insert (
               Target_Data.First,
               Target_Data.Last,
               Target_Data.Length,
               Upcast (Before),
               Upcast (Position));
         end;
      end if;
   end Splice;

   procedure Splice (
      Container : in out List;
      Before : Cursor;
      Position : Cursor) is
   begin
      if Before /= Position then -- RM A.18.3(116/3)
         Unique (Container, True);
         declare
            Data : constant Data_Access := Downcast (Container.Super.Data);
         begin
            Base.Remove (
               Data.First,
               Data.Last,
               Data.Length,
               Upcast (Position),
               Position.Super.Next);
            Base.Insert (
               Data.First,
               Data.Last,
               Data.Length,
               Upcast (Before),
               Upcast (Position));
         end;
      end if;
   end Splice;

   function First (Container : List) return Cursor is
   begin
      if Is_Empty (Container) then
         return null;
      else
         Unique (Container'Unrestricted_Access.all, False);
         return Downcast (Downcast (Container.Super.Data).First);
      end if;
   end First;

   function First_Element (Container : List'Class)
      return Element_Type is
   begin
      return Element (First (List (Container)));
   end First_Element;

   function Last (Container : List) return Cursor is
   begin
      if Is_Empty (Container) then
         return null;
      else
         Unique (Container'Unrestricted_Access.all, False);
         return Downcast (Downcast (Container.Super.Data).Last);
      end if;
   end Last;

   function Last_Element (Container : List'Class)
      return Element_Type is
   begin
      return Element (Last (List (Container)));
   end Last_Element;

   function Next (Position : Cursor) return Cursor is
   begin
      return Downcast (Position.Super.Next);
   end Next;

   function Previous (Position : Cursor) return Cursor is
   begin
      return Downcast (Position.Super.Super.Previous);
   end Previous;

   procedure Next (Position : in out Cursor) is
   begin
      Position := Downcast (Position.Super.Next);
   end Next;

   procedure Previous (Position : in out Cursor) is
   begin
      Position := Downcast (Position.Super.Super.Previous);
   end Previous;

   function Find (
      Container : List;
      Item : Element_Type)
      return Cursor is
   begin
      if Is_Empty (Container) then
         return null;
      else
         Unique (Container'Unrestricted_Access.all, False);
         declare
            Context : Context_Type := (Left => Item'Unrestricted_Access);
         begin
            return Downcast (Base.Find (
               Downcast (Container.Super.Data).First,
               Context'Address,
               Equivalent => Equivalent_Element'Access));
         end;
      end if;
   end Find;

   function Find (
      Container : List;
      Item : Element_Type;
      Position : Cursor)
      return Cursor
   is
      pragma Unreferenced (Container);
      Context : Context_Type := (Left => Item'Unrestricted_Access);
   begin
      return Downcast (Base.Find (
         Upcast (Position),
         Context'Address,
         Equivalent => Equivalent_Element'Access));
   end Find;

   function Reverse_Find (
      Container : List;
      Item : Element_Type)
      return Cursor is
   begin
      if Is_Empty (Container) then
         return null;
      else
         Unique (Container'Unrestricted_Access.all, False);
         declare
            Context : Context_Type := (Left => Item'Unrestricted_Access);
         begin
            return Downcast (Linked_Lists.Reverse_Find (
               Downcast (Container.Super.Data).Last,
               Context'Address,
               Equivalent => Equivalent_Element'Access));
         end;
      end if;
   end Reverse_Find;

   function Reverse_Find (
      Container : List;
      Item : Element_Type;
      Position : Cursor)
      return Cursor
   is
      pragma Unreferenced (Container);
      Context : Context_Type := (Left => Item'Unrestricted_Access);
   begin
      return Downcast (Linked_Lists.Reverse_Find (
         Upcast (Position),
         Context'Address,
         Equivalent => Equivalent_Element'Access));
   end Reverse_Find;

   function Contains (Container : List; Item : Element_Type) return Boolean is
   begin
      return Find (Container, Item) /= null;
   end Contains;

   function "<" (Left, Right : Cursor) return Boolean is
   begin
      return Base.Is_Before (Upcast (Left), Upcast (Right));
   end "<";

   procedure Iterate (
      Container : List'Class;
      Process : not null access procedure (Position : Cursor))
   is
      type P1 is access procedure (Position : Cursor);
      type P2 is access procedure (Position : Linked_Lists.Node_Access);
      function Cast is new Unchecked_Conversion (P1, P2);
   begin
      if not Is_Empty (List (Container)) then
         Unique (List (Container)'Unrestricted_Access.all, False);
         Base.Iterate (
            Downcast (Container.Super.Data).First,
            Cast (Process));
      end if;
   end Iterate;

   procedure Reverse_Iterate (
      Container : List'Class;
      Process : not null access procedure (Position : Cursor))
   is
      type P1 is access procedure (Position : Cursor);
      type P2 is access procedure (Position : Linked_Lists.Node_Access);
      function Cast is new Unchecked_Conversion (P1, P2);
   begin
      if not Is_Empty (List (Container)) then
         Unique (List (Container)'Unrestricted_Access.all, False);
         Linked_Lists.Reverse_Iterate (
            Downcast (Container.Super.Data).Last,
            Cast (Process));
      end if;
   end Reverse_Iterate;

   function Iterate (Container : List'Class)
      return List_Iterator_Interfaces.Reversible_Iterator'Class is
   begin
      return List_Iterator'(
         First => First (List (Container)),
         Last => Last (List (Container)));
   end Iterate;

   function Iterate (Container : List'Class; First, Last : Cursor)
      return List_Iterator_Interfaces.Reversible_Iterator'Class
   is
      pragma Unreferenced (Container);
      Actual_First : Cursor := First;
      Actual_Last : Cursor := Last;
   begin
      if Actual_First = No_Element
         or else Actual_Last = No_Element
         or else Actual_Last < Actual_First
      then
         Actual_First := No_Element;
         Actual_Last := No_Element;
      end if;
      return List_Iterator'(First => Actual_First, Last => Actual_Last);
   end Iterate;

   overriding procedure Adjust (Object : in out List) is
   begin
      Copy_On_Write.Adjust (Object.Super'Access);
   end Adjust;

   overriding function First (Object : List_Iterator) return Cursor is
   begin
      return Object.First;
   end First;

   overriding function Next (Object : List_Iterator; Position : Cursor)
      return Cursor is
   begin
      if Position = Object.Last then
         return No_Element;
      else
         return Next (Position);
      end if;
   end Next;

   overriding function Last (Object : List_Iterator) return Cursor is
   begin
      return Object.Last;
   end Last;

   overriding function Previous (Object : List_Iterator; Position : Cursor)
      return Cursor is
   begin
      if Position = Object.First then
         return No_Element;
      else
         return Previous (Position);
      end if;
   end Previous;

   package body Generic_Sorting is

      function LT (Left, Right : not null Linked_Lists.Node_Access)
         return Boolean;
      function LT (Left, Right : not null Linked_Lists.Node_Access)
         return Boolean is
      begin
         return Downcast (Left).Element.all <
            Downcast (Right).Element.all;
      end LT;

      function Is_Sorted (Container : List) return Boolean is
      begin
         if Length (Container) <= 1 then
            return True;
         else
            Unique (Container'Unrestricted_Access.all, False); -- private
            return Linked_Lists.Is_Sorted (
               Downcast (Container.Super.Data).Last,
               LT => LT'Access);
         end if;
      end Is_Sorted;

      procedure Sort (Container : in out List) is
      begin
         if Length (Container) > 1 then
            Unique (Container, True);
            declare
               Data : constant Data_Access := Downcast (Container.Super.Data);
            begin
               Base.Merge_Sort (
                  Data.First,
                  Data.Last,
                  Data.Length,
                  LT => LT'Access);
            end;
         end if;
      end Sort;

      procedure Merge (Target : in out List; Source : in out List) is
         pragma Check (Pre,
            Check =>
               Target'Address /= Source'Address
               or else Is_Empty (Target)
               or else raise Program_Error);
               --  RM A.18.3(151/3), same nonempty container
      begin
         if not Is_Empty (Source) then
            if Is_Empty (Target) then
               Move (Target, Source);
            else
               Unique (Target, True);
               Unique (Source, True);
               declare
                  Target_Data : constant Data_Access :=
                     Downcast (Target.Super.Data);
                  Source_Data : constant Data_Access :=
                     Downcast (Source.Super.Data);
               begin
                  Base.Merge (
                     Target_Data.First,
                     Target_Data.Last,
                     Target_Data.Length,
                     Source_Data.First,
                     Source_Data.Last,
                     Source_Data.Length,
                     LT => LT'Access);
               end;
            end if;
         end if;
      end Merge;

   end Generic_Sorting;

   package body Streaming is

      procedure Read (
         Stream : not null access Streams.Root_Stream_Type'Class;
         Item : out List)
      is
         Length : Count_Type'Base;
      begin
         Count_Type'Base'Read (Stream, Length);
         Clear (Item);
         for I in 1 .. Length loop
            declare
               New_Item : constant Element_Type := Element_Type'Input (Stream);
            begin
               Insert (Item, null, New_Item);
--  diff
            end;
         end loop;
      end Read;

      procedure Write (
         Stream : not null access Streams.Root_Stream_Type'Class;
         Item : List)
      is
         Length : constant Count_Type :=
            Indefinite_Doubly_Linked_Lists.Length (Item);
      begin
         Count_Type'Write (Stream, Length);
         if Length > 0 then
            declare
               Position : Cursor := First (Item);
            begin
               while Position /= null loop
                  Element_Type'Output (Stream, Position.Element.all);
                  Next (Position);
               end loop;
            end;
         end if;
      end Write;

   end Streaming;

end Ada.Containers.Indefinite_Doubly_Linked_Lists;
