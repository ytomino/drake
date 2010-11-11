with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
package body Ada.Containers.Indefinite_Doubly_Linked_Lists is
   use type Linked_Lists.Node_Access;
   use type Copy_On_Write.Data_Access;

   function Upcast is new Unchecked_Conversion (
      Cursor,
      Linked_Lists.Node_Access);
   function Downcast is new Unchecked_Conversion (
      Linked_Lists.Node_Access,
      Cursor);

   function Upcast is new Unchecked_Conversion (
      Data_Access,
      Copy_On_Write.Data_Access);
   function Downcast is new Unchecked_Conversion (
      Copy_On_Write.Data_Access,
      Data_Access);

   type Direction_Type is (Forward, Backward);
   pragma Discard_Names (Direction_Type);

   function Find (
      Direction : Direction_Type;
      Start : Linked_Lists.Node_Access;
      Item : Element_Type) return Linked_Lists.Node_Access;
   function Find (
      Direction : Direction_Type;
      Start : Linked_Lists.Node_Access;
      Item : Element_Type) return Linked_Lists.Node_Access
   is
      function Equivalent (Right : not null Linked_Lists.Node_Access)
         return Boolean;
      function Equivalent (Right : not null Linked_Lists.Node_Access)
         return Boolean is
      begin
         return Item = Downcast (Right).Element.all;
      end Equivalent;
   begin
      case Direction is
         when Forward =>
            return Base.Find (Start, Equivalent'Access);
         when Backward =>
            return Linked_Lists.Reverse_Find (Start, Equivalent'Access);
      end case;
   end Find;

   procedure Copy_Node (
      Target : out Linked_Lists.Node_Access;
      Source : not null Linked_Lists.Node_Access);
   procedure Copy_Node (
      Target : out Linked_Lists.Node_Access;
      Source : not null Linked_Lists.Node_Access)
   is
      New_Node : constant Cursor := new Node'(Super => <>,
         Element => new Element_Type'(Downcast (Source).Element.all));
   begin
      Target := Upcast (New_Node);
   end Copy_Node;

   procedure Free is new Unchecked_Deallocation (Node, Cursor);
   procedure Free is new Unchecked_Deallocation (Element_Type, Element_Access);

   procedure Free_Node (Object : in out Linked_Lists.Node_Access);
   procedure Free_Node (Object : in out Linked_Lists.Node_Access) is
      X : Cursor := Downcast (Object);
   begin
      Free (X.Element);
      Free (X);
      Object := null;
   end Free_Node;

   procedure Allocate_Data (
      Target : out Copy_On_Write.Data_Access);
   procedure Allocate_Data (
      Target : out Copy_On_Write.Data_Access)
   is
      New_Data : constant Data_Access := new Data'(
         Super => <>,
         First => null,
         Last => null,
         Length => 0);
   begin
      Target := Upcast (New_Data);
   end Allocate_Data;

   procedure Copy_Data (
      Target : out Copy_On_Write.Data_Access;
      Source : not null Copy_On_Write.Data_Access;
      Capacity : Count_Type);
   procedure Copy_Data (
      Target : out Copy_On_Write.Data_Access;
      Source : not null Copy_On_Write.Data_Access;
      Capacity : Count_Type)
   is
      pragma Unreferenced (Capacity);
      New_Data : Data_Access := new Data'(
         Super => <>,
         First => null,
         Last => null,
         Length => 0);
   begin
      Linked_Lists.Copy (
         New_Data.First,
         New_Data.Last,
         New_Data.Length,
         Source_Last => Downcast (Source).Last,
         Copy => Copy_Node'Access,
         Insert => Base.Insert'Access);
      Target := Upcast (New_Data);
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
      Copy_On_Write.Unique (
         Container.Super'Access,
         To_Update,
         0,
         Allocate => Allocate_Data'Access,
         Copy => Copy_Data'Access);
   end Unique;

   procedure Adjust (Object : in out List) is
   begin
      Copy_On_Write.Adjust (Object.Super'Access);
   end Adjust;

   procedure Assign (Target : in out List; Source : List) is
   begin
      Copy_On_Write.Assign (
         Target.Super'Access,
         Source.Super'Access,
         Free => Free_Data'Access);
   end Assign;

   procedure Append (
      Container : in out List;
      New_Item : Element_Type;
      Count : Count_Type := 1) is
   begin
      Insert (Container, null, New_Item, Count);
   end Append;

   procedure Clear (Container : in out List) is
   begin
      Copy_On_Write.Clear (
         Container.Super'Access,
         Free => Free_Data'Access);
   end Clear;

   function Constant_Reference (
      Container : not null access constant List;
      Position : Cursor)
      return Constant_Reference_Type
   is
      pragma Unreferenced (Container);
   begin
      return (Element => Position.Element);
   end Constant_Reference;

   function Contains (Container : List; Item : Element_Type) return Boolean is
   begin
      return Find (Container, Item) /= null;
   end Contains;

   function Copy (Source : List) return List is
   begin
      return (Finalization.Controlled with
         Super => Copy_On_Write.Copy (
            Source.Super'Access,
            0,
            Copy => Copy_Data'Access));
   end Copy;

   procedure Delete (
      Container : in out List;
      Position : in out Cursor;
      Count : Count_Type := 1)
   is
      X : Linked_Lists.Node_Access;
      Next : Linked_Lists.Node_Access;
   begin
      Unique (Container, True);
      for I in 1 .. Count loop
         X := Upcast (Position);
         Next := Position.Super.Next;
         Base.Remove (
            Downcast (Container.Super.Data).First,
            Downcast (Container.Super.Data).Last,
            Downcast (Container.Super.Data).Length,
            Position => X,
            Next => Next);
         Free_Node (X);
         Position := Downcast (Next);
      end loop;
   end Delete;

   procedure Delete_First (Container : in out List; Count : Count_Type := 1) is
      Position : Cursor;
   begin
      for I in 1 .. Count loop
         Position := Downcast (Downcast (Container.Super.Data).First);
         Delete (Container, Position);
      end loop;
   end Delete_First;

   procedure Delete_Last (Container : in out List; Count : Count_Type := 1) is
      Position : Cursor;
   begin
      for I in 1 .. Count loop
         Position := Downcast (Downcast (Container.Super.Data).Last);
         Delete (Container, Position);
      end loop;
   end Delete_Last;

   function Element (Position : Cursor) return Element_Type is
   begin
      return Position.Element.all;
   end Element;

   function Empty_List return List is
   begin
      return (Finalization.Controlled with Super => (null, null));
   end Empty_List;

   function Find (Container : List; Item : Element_Type) return Cursor is
   begin
      if Is_Empty (Container) then
         return null;
      else
         Unique (Container'Unrestricted_Access.all, False);
         return Downcast (Find (
            Forward,
            Downcast (Container.Super.Data).First,
            Item));
      end if;
   end Find;

   function Find (Container : List; Item : Element_Type; Position : Cursor)
      return Cursor
   is
      pragma Unreferenced (Container);
   begin
      return Downcast (Find (Forward, Upcast (Position), Item));
   end Find;

   function First (Container : List) return Cursor is
   begin
      if Is_Empty (Container) then
         return null;
      else
         Unique (Container'Unrestricted_Access.all, False);
         return Downcast (Downcast (Container.Super.Data).First);
      end if;
   end First;

   function First_Element (Container : List) return Element_Type is
   begin
      return First (Container).Element.all;
   end First_Element;

   function Has_Element (Position : Cursor) return Boolean is
   begin
      return Position /= null;
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
--
--
--
--
--
--
--
--

   procedure Insert (
      Container : in out List;
      Before : Cursor;
      New_Item : Element_Type;
      Position : out Cursor;
      Count : Count_Type := 1) is
   begin
      Unique (Container, True);
      for I in 1 .. Count loop
         Position := new Node'(
            Super => <>,
            Element => new Element_Type'(New_Item));
         Base.Insert (
            Downcast (Container.Super.Data).First,
            Downcast (Container.Super.Data).Last,
            Downcast (Container.Super.Data).Length,
            Before => Upcast (Before),
            New_Item => Upcast (Position));
      end loop;
   end Insert;

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

   function Is_Empty (Container : List) return Boolean is
   begin
      return Container.Super.Data = null
         or else Downcast (Container.Super.Data).Last = null;
   end Is_Empty;

   procedure Iterate (Container : List;
      Process : not null access procedure (Position : Cursor))
   is
      procedure Process_2 (Position : not null Linked_Lists.Node_Access);
      procedure Process_2 (Position : not null Linked_Lists.Node_Access) is
      begin
         Process (Downcast (Position));
      end Process_2;
   begin
      if not Is_Empty (Container) then
         Unique (Container'Unrestricted_Access.all, False);
         Base.Iterate (
            Downcast (Container.Super.Data).First,
            Process_2'Access);
      end if;
   end Iterate;

   function Last (Container : List) return Cursor is
   begin
      if Is_Empty (Container) then
         return null;
      else
         Unique (Container'Unrestricted_Access.all, False);
         return Downcast (Downcast (Container.Super.Data).Last);
      end if;
   end Last;

   function Last_Element (Container : List) return Element_Type is
   begin
      return Last (Container).Element.all;
   end Last_Element;

   function Length (Container : List) return Count_Type is
   begin
      if Container.Super.Data = null then
         return 0;
      else
         return Downcast (Container.Super.Data).Length;
      end if;
   end Length;

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

   function Next (Position : Cursor) return Cursor is
   begin
      return Downcast (Position.Super.Next);
   end Next;

   procedure Next (Position : in out Cursor) is
   begin
      Position := Downcast (Position.Super.Next);
   end Next;

   function No_Element return Cursor is
   begin
      return null;
   end No_Element;

   procedure Prepend (
      Container : in out List;
      New_Item : Element_Type;
      Count : Count_Type := 1) is
   begin
      Insert (
         Container,
         Downcast (Downcast (Container.Super.Data).First),
         New_Item,
         Count);
   end Prepend;

   function Previous (Position : Cursor) return Cursor is
   begin
      return Downcast (Position.Super.Super.Previous);
   end Previous;

   procedure Previous (Position : in out Cursor) is
   begin
      Position := Downcast (Position.Super.Super.Previous);
   end Previous;

   procedure Query_Element (
      Position : Cursor;
      Process : not null access procedure (Element : Element_Type)) is
   begin
      Process (Position.Element.all);
   end Query_Element;

   function Reference (
      Container : not null access List;
      Position : Cursor)
      return Reference_Type is
   begin
      Unique (Container.all, True);
      return (Element => Position.Element);
   end Reference;

   procedure Replace_Element (
      Container : in out List;
      Position : Cursor;
      New_Item : Element_Type) is
   begin
      Unique (Container, True);
      Position.Element := new Element_Type'(New_Item);
   end Replace_Element;

   procedure Reverse_Elements (Container : in out List) is
   begin
      if not Is_Empty (Container) then
         Unique (Container, True);
         Linked_Lists.Reverse_Elements (
            Downcast (Container.Super.Data).First,
            Downcast (Container.Super.Data).Last,
            Downcast (Container.Super.Data).Length,
            Insert => Base.Insert'Access,
            Remove => Base.Remove'Access);
      end if;
   end Reverse_Elements;

   function Reverse_Find (Container : List; Item : Element_Type)
      return Cursor is
   begin
      if Is_Empty (Container) then
         return null;
      else
         Unique (Container'Unrestricted_Access.all, False);
         return Downcast (Find (
            Backward,
            Downcast (Container.Super.Data).First, Item));
      end if;
   end Reverse_Find;

   function Reverse_Find (
      Container : List;
      Item : Element_Type;
      Position : Cursor) return Cursor
   is
      pragma Unreferenced (Container);
   begin
      return Downcast (Find (Backward, Upcast (Position), Item));
   end Reverse_Find;

   procedure Reverse_Iterate (
      Container : List;
      Process : not null access procedure (Position : Cursor))
   is
      procedure Process_2 (Position : not null Linked_Lists.Node_Access);
      procedure Process_2 (Position : not null Linked_Lists.Node_Access) is
      begin
         Process (Downcast (Position));
      end Process_2;
   begin
      if not Is_Empty (Container) then
         Unique (Container'Unrestricted_Access.all, False);
         Linked_Lists.Reverse_Iterate (
            Downcast (Container.Super.Data).Last,
            Process_2'Access);
      end if;
   end Reverse_Iterate;

   procedure Splice (
      Target : in out List;
      Before : Cursor;
      Source : in out List)
   is
      type List_Access is access all List;
   begin
      if List_Access'(Target'Access) /= List_Access'(Source'Access) then
         Unique (Target, True);
         Unique (Source, True);
         Base.Splice (
            Downcast (Target.Super.Data).First,
            Downcast (Target.Super.Data).Last,
            Downcast (Target.Super.Data).Length,
            Upcast (Before),
            Downcast (Source.Super.Data).First,
            Downcast (Source.Super.Data).Last,
            Downcast (Source.Super.Data).Length);
      end if;
   end Splice;

   procedure Splice (
      Target : in out List;
      Before : Cursor;
      Source : in out List;
      Position : in out Cursor) is
   begin
      Unique (Target, True);
      Unique (Source, True);
      Base.Remove (
         Downcast (Source.Super.Data).First,
         Downcast (Source.Super.Data).Last,
         Downcast (Source.Super.Data).Length,
         Upcast (Position),
         Position.Super.Next);
      Base.Insert (
         Downcast (Target.Super.Data).First,
         Downcast (Target.Super.Data).Last,
         Downcast (Target.Super.Data).Length,
         Upcast (Before),
         Upcast (Position));
   end Splice;

   procedure Splice (
      Container : in out List;
      Before : Cursor;
      Position : Cursor) is
   begin
      Unique (Container, True);
      Base.Remove (
         Downcast (Container.Super.Data).First,
         Downcast (Container.Super.Data).Last,
         Downcast (Container.Super.Data).Length,
         Upcast (Position),
         Position.Super.Next);
      Base.Insert (
         Downcast (Container.Super.Data).First,
         Downcast (Container.Super.Data).Last,
         Downcast (Container.Super.Data).Length,
         Upcast (Before),
         Upcast (Position));
   end Splice;

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
      Base.Swap_Links (
         Downcast (Container.Super.Data).First,
         Downcast (Container.Super.Data).Last,
         Upcast (I),
         Upcast (J));
   end Swap_Links;

   procedure Update_Element (
      Container : in out List;
      Position : Cursor;
      Process : not null access procedure (Element : in out Element_Type)) is
--  diff
   begin
      Unique (Container, True);
      Process (Position.Element.all);
   end Update_Element;

   function "=" (Left, Right : List) return Boolean is
      function Equivalent (Left, Right : not null Linked_Lists.Node_Access)
         return Boolean;
      function Equivalent (Left, Right : not null Linked_Lists.Node_Access)
         return Boolean is
      begin
         return Downcast (Left).Element.all =
            Downcast (Right).Element.all;
      end Equivalent;
   begin
      if Is_Empty (Left) then
         return Is_Empty (Right);
      elsif Left.Super.Data = Right.Super.Data then
         return True;
      elsif Length (Left) = Length (Right) then
         Unique (Left'Unrestricted_Access.all, False);
         Unique (Right'Unrestricted_Access.all, False);
         return Linked_Lists.Equivalent (
            Downcast (Left.Super.Data).Last,
            Downcast (Right.Super.Data).Last,
            Equivalent'Access);
      else
         return False;
      end if;
   end "=";

   function "<=" (Left, Right : Cursor) return Boolean is
   begin
      return Left /= null
         and then not Base.Is_Before (Upcast (Right), Upcast (Left));
   end "<=";

   function ">=" (Left, Right : Cursor) return Boolean is
   begin
      return Left /= null
         and then not Base.Is_Before (Upcast (Left), Upcast (Right));
   end ">=";

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
         if Is_Empty (Container) then
            return True;
         else
            Unique (Container'Unrestricted_Access.all, False);
            return Linked_Lists.Is_Sorted (
               Downcast (Container.Super.Data).Last, LT'Access);
         end if;
      end Is_Sorted;

      procedure Sort (Container : in out List) is
      begin
         if not Is_Empty (Container) then
            Unique (Container, True);
            Linked_Lists.Merge_Sort (
               Downcast (Container.Super.Data).First,
               Downcast (Container.Super.Data).Last,
               Downcast (Container.Super.Data).Length,
               LT => LT'Access,
               Splice => Base.Splice'Access,
               Split => Base.Split'Access,
               Insert => Base.Insert'Access,
               Remove => Base.Remove'Access);
         end if;
      end Sort;

      procedure Merge (Target : in out List; Source : in out List) is
      begin
         if not Is_Empty (Source) then
            Unique (Target, True);
            Linked_Lists.Merge (
               Downcast (Target.Super.Data).First,
               Downcast (Target.Super.Data).Last,
               Downcast (Target.Super.Data).Length,
               Downcast (Source.Super.Data).First,
               Downcast (Source.Super.Data).Last,
               Downcast (Source.Super.Data).Length,
               LT => LT'Access,
               Insert => Base.Insert'Access,
               Remove => Base.Remove'Access);
         end if;
      end Merge;

   end Generic_Sorting;

   package body No_Primitives is

      procedure Read (
         Stream : not null access Streams.Root_Stream_Type'Class;
         Container : out List)
      is
         Length : Count_Type'Base;
      begin
         Count_Type'Read (Stream, Length);
         Clear (Container);
         Unique (Container, True);
         for I in 1 .. Length loop
            declare
               Position : constant Cursor := new Node'(
                  Super => <>,
                  Element => new Element_Type'(Element_Type'Input (Stream)));
            begin
               Base.Insert (
                  Downcast (Container.Super.Data).First,
                  Downcast (Container.Super.Data).Last,
                  Downcast (Container.Super.Data).Length,
                  Before => null,
                  New_Item => Upcast (Position));
            end;
         end loop;
      end Read;

      procedure Write (
         Stream : not null access Streams.Root_Stream_Type'Class;
         Container : List)
      is
         Position : Cursor;
      begin
         Count_Type'Write (Stream, Container.Length);
         Position := First (Container);
         while Position /= null loop
            Element_Type'Output (Stream, Position.Element.all);
            Next (Position);
         end loop;
      end Write;

   end No_Primitives;

end Ada.Containers.Indefinite_Doubly_Linked_Lists;
