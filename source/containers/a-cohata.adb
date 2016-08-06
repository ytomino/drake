with Ada.Unchecked_Deallocation;
package body Ada.Containers.Hash_Tables is

   procedure Free is new Unchecked_Deallocation (Table, Table_Access);

   subtype Positive_Hash_Type is Hash_Type range 1 .. Hash_Type'Last;

   procedure Allocate (
      New_Container : out Table_Access;
      Capacity : Count_Type);
   procedure Allocate (
      New_Container : out Table_Access;
      Capacity : Count_Type) is
   begin
      New_Container := new Table'(
         Last_Index => Hash_Type (Capacity) - 1,
         First => null,
         Entries => (others => (First => null, Previous => null)));
   end Allocate;

   function Find_Node (
      Container : Table_Access;
      Node : not null Node_Access;
      Equivalent : not null access function (
         Left, Right : not null Node_Access)
         return Boolean)
      return Node_Access;
   function Find_Node (
      Container : Table_Access;
      Node : not null Node_Access;
      Equivalent : not null access function (
         Left, Right : not null Node_Access)
         return Boolean)
      return Node_Access is
   begin
      if Container /= null then
         declare
            Index : constant Hash_Type :=
               Node.Hash rem Positive_Hash_Type'(Container.Entries'Length);
            Position : Node_Access := Container.Entries (Index).First;
         begin
            if Position /= null then
               loop
                  if Equivalent (Node, Position) then
                     return Position;
                  end if;
                  exit when Position.Next = null
                     or else Position.Next.Index /= Index;
                  Position := Position.Next;
               end loop;
            end if;
         end;
      end if;
      return null;
   end Find_Node;

   procedure Insert_No_Rebuild (
      Container : not null Table_Access;
      Hash : Hash_Type;
      New_Item : not null Node_Access);
   procedure Insert_No_Rebuild (
      Container : not null Table_Access;
      Hash : Hash_Type;
      New_Item : not null Node_Access)
   is
      Index : constant Hash_Type :=
         Hash rem Positive_Hash_Type'(Container.Entries'Length);
   begin
      New_Item.Hash := Hash;
      New_Item.Index := Index;
      if Container.Entries (Index).First /= null then
         if Container.First = Container.Entries (Index).First then
            Container.First := New_Item;
         else
            Container.Entries (Index).Previous.Next := New_Item;
         end if;
         New_Item.Next := Container.Entries (Index).First;
         Container.Entries (Index).First := New_Item;
      else
         if Container.First /= null then
            Container.Entries (Container.First.Index).Previous := New_Item;
         end if;
         New_Item.Next := Container.First;
         Container.First := New_Item;
         Container.Entries (Index).First := New_Item;
      end if;
   end Insert_No_Rebuild;

   --  implementation

   function First (Container : Table_Access) return Node_Access is
   begin
      if Container /= null then
         return Container.First;
      else
         return null;
      end if;
   end First;

   procedure Iterate (
      Container : Table_Access;
      Process : not null access procedure (Position : not null Node_Access))
   is
      Position : Node_Access := First (Container);
   begin
      while Position /= null loop
         Process (Position);
         Position := Position.Next;
      end loop;
   end Iterate;

   function Is_Before (Before, After : Node_Access) return Boolean is
   begin
      if Before = After then
         return False;
      elsif Before.Index < After.Index then
         return True;
      elsif Before.Index > After.Index then
         return False;
      else
         declare
            Index : constant Hash_Type := Before.Index;
            I : Node_Access := Before.Next;
            J : Node_Access := After.Next;
         begin
            loop
               if J = null or else I = After or else J.Index /= Index then
                  return True;
               elsif I = null or else J = Before or else I.Index /= Index then
                  return False;
               else
                  I := I.Next;
                  J := J.Next;
               end if;
            end loop;
         end;
      end if;
   end Is_Before;

   function Find (
      Container : Table_Access;
      Hash : Hash_Type;
      Params : System.Address;
      Equivalent : not null access function (
         Position : not null Node_Access;
         Params : System.Address)
         return Boolean)
      return Node_Access is
   begin
      if Container /= null then
         declare
            Index : constant Hash_Type :=
               Hash rem Positive_Hash_Type'(Container.Entries'Length);
            Position : Node_Access := Container.Entries (Index).First;
         begin
            if Position /= null then
               loop
                  if Equivalent (Position, Params) then
                     return Position;
                  end if;
                  exit when Position.Next = null
                     or else Position.Next.Index /= Index;
                  Position := Position.Next;
               end loop;
            end if;
         end;
      end if;
      return null;
   end Find;

   function Equivalent (
      Left : Table_Access;
      Left_Length : Count_Type;
      Right : Table_Access;
      Right_Length : Count_Type;
      Equivalent : not null access function (
         Left, Right : not null Node_Access)
         return Boolean)
      return Boolean is
   begin
      return Left_Length = Right_Length
         and then Is_Subset (Left, Right, Equivalent);
   end Equivalent;

   function Overlap (
      Left, Right : Table_Access;
      Equivalent : not null access function (
         Left, Right : not null Node_Access)
         return Boolean)
      return Boolean is
   begin
      if Left = null or else Right = null then
         return False;
      else
         declare
            Left_Position : Node_Access := Left.First;
         begin
            while Left_Position /= null loop
               if Find_Node (Right, Left_Position, Equivalent) /= null then
                  return True;
               end if;
               Left_Position := Left_Position.Next;
            end loop;
         end;
         return False;
      end if;
   end Overlap;

   function Is_Subset (
      Subset, Of_Set : Table_Access;
      Equivalent : not null access function (
         Left, Right : not null Node_Access)
         return Boolean)
      return Boolean is
   begin
      if Subset = null or else Of_Set = null then
         return Subset = null;
      else
         declare
            Left_Position : Node_Access := Subset.First;
         begin
            while Left_Position /= null loop
               if Find_Node (Of_Set, Left_Position, Equivalent) = null then
                  return False;
               end if;
               Left_Position := Left_Position.Next;
            end loop;
         end;
         return True;
      end if;
   end Is_Subset;

   function Capacity (Container : Table_Access) return Count_Type is
   begin
      if Container = null then
         return 0;
      else
         return Container.Entries'Length;
      end if;
   end Capacity;

   procedure Free (
      Container : in out Table_Access;
      Length : in out Count_Type;
      Free : not null access procedure (Object : in out Node_Access)) is
   begin
      if Container /= null then
         declare
            Position : Node_Access := Container.First;
         begin
            while Position /= null loop
               declare
                  Next : constant Node_Access := Position.Next;
               begin
                  Free (Position);
                  Position := Next;
               end;
            end loop;
         end;
         Hash_Tables.Free (Container);
      end if;
      Length := 0;
   end Free;

   procedure Copy (
      Target : out Table_Access;
      Length : out Count_Type;
      Source : Table_Access;
      New_Capacity : Count_Type;
      Copy : not null access procedure (
         Target : out Node_Access;
         Source : not null Node_Access)) is
   begin
      pragma Assert (New_Capacity > 0 or else First (Source) = null);
      Length := 0;
      if New_Capacity > 0 then
         Allocate (Target, New_Capacity);
         declare
            Position : Node_Access := First (Source);
         begin
            while Position /= null loop
               declare
                  New_Node : Node_Access;
               begin
                  Copy (New_Node, Position);
                  Insert_No_Rebuild (Target, Position.Hash, New_Node);
               end;
               Length := Length + 1;
               Position := Position.Next;
            end loop;
         end;
      end if;
   end Copy;

   procedure Rebuild (
      Container : in out Table_Access;
      New_Capacity : Count_Type) is
   begin
      if New_Capacity /= Capacity (Container) then
         declare
            Position : Node_Access;
         begin
            if Container /= null then
               Position := Container.First;
               Free (Container);
            end if;
            pragma Assert (New_Capacity > 0 or else Position = null);
            if New_Capacity > 0 then
               Allocate (Container, New_Capacity);
               while Position /= null loop
                  declare
                     Next : constant Node_Access := Position.Next;
                  begin
                     Insert_No_Rebuild (Container, Position.Hash, Position);
                     Position := Next;
                  end;
               end loop;
            end if;
         end;
      end if;
   end Rebuild;

   procedure Insert (
      Container : in out Table_Access;
      Length : in out Count_Type;
      Hash : Hash_Type;
      New_Item : not null Node_Access) is
   begin
      if Container = null or else Length >= Container.Entries'Length then
         declare
            New_Length : constant Count_Type :=
               Count_Type'Max (13, Length * 2 + 1);
         begin
            Rebuild (Container, New_Length);
         end;
      end if;
      Insert_No_Rebuild (Container, Hash, New_Item);
      Length := Length + 1;
   end Insert;

   procedure Remove (
      Container : Table_Access;
      Length : in out Count_Type;
      Item : not null Node_Access)
   is
      Previous : Node_Access;
   begin
      if Container.Entries (Item.Index).First = Item then
         Previous := Container.Entries (Item.Index).Previous;
         if Item.Next /= null and then Item.Next.Index = Item.Index then
            Container.Entries (Item.Index).First := Item.Next;
         else
            Container.Entries (Item.Index).First := null;
            Container.Entries (Item.Index).Previous := null;
         end if;
         if Previous = null then -- Item = Container.First
            Container.First := Item.Next;
         else
            Previous.Next := Item.Next;
         end if;
      else
         Previous := Container.Entries (Item.Index).First;
         while Previous.Next /= Item loop
            Previous := Previous.Next;
         end loop;
         Previous.Next := Item.Next;
      end if;
      if Item.Next /= null and then Item.Next.Index /= Item.Index then
         Container.Entries (Item.Next.Index).Previous := Previous;
      end if;
      Length := Length - 1;
   end Remove;

   procedure Merge (
      Target : in out Table_Access;
      Length : in out Count_Type;
      Source : Table_Access;
      Source_Length : Count_Type;
      Filter : Filter_Type;
      Equivalent : not null access function (
         Left, Right : not null Node_Access)
         return Boolean;
      Copy : access procedure (
         Target : out Node_Access;
         Source : not null Node_Access);
      Free : access procedure (Object : in out Node_Access)) is
   begin
      if Length = 0 then
         if Filter (In_Only_Right) and then Source_Length > 0 then
            Hash_Tables.Free (Target);
            Hash_Tables.Copy (Target, Length, Source, Source_Length, Copy);
         end if;
      elsif Source_Length = 0 then
         if not Filter (In_Only_Left) then -- Length > 0
            Hash_Tables.Free (Target, Length, Free);
         end if;
      else
         declare
            I, Next : Node_Access;
            New_Node : Node_Access;
            From_Right : Node_Access := null;
         begin
            if Filter (In_Only_Right) then
               I := Source.First;
               while I /= null loop
                  if Find_Node (Target, I, Equivalent) = null then
                     Copy (New_Node, I);
                     New_Node.Hash := I.Hash;
                     New_Node.Next := From_Right;
                     From_Right := New_Node;
                  end if;
                  I := I.Next;
               end loop;
            end if;
            I := Target.First;
            while I /= null loop
               Next := I.Next;
               if Find_Node (Source, I, Equivalent) /= null then
                  if not Filter (In_Both) then
                     Remove (Target, Length, I);
                     Free (I);
                  end if;
               else
                  if not Filter (In_Only_Left) then
                     Remove (Target, Length, I);
                     Free (I);
                  end if;
               end if;
               I := Next;
            end loop;
            while From_Right /= null loop
               Next := From_Right.Next;
               Insert (Target, Length, From_Right.Hash, From_Right);
               From_Right := Next;
            end loop;
         end;
      end if;
   end Merge;

   procedure Copying_Merge (
      Target : out Table_Access;
      Length : out Count_Type;
      Left : Table_Access;
      Left_Length : Count_Type;
      Right : Table_Access;
      Right_Length : Count_Type;
      Filter : Filter_Type;
      Equivalent : not null access function (
         Left, Right : not null Node_Access)
         return Boolean;
      Copy : not null access procedure (
         Target : out Node_Access;
         Source : not null Node_Access)) is
   begin
      if Left_Length = 0 then
         if Filter (In_Only_Right) and then Right_Length > 0 then
            Hash_Tables.Copy (Target, Length, Right, Right_Length,
               Copy => Copy);
         else
            Target := null;
            Length := 0;
         end if;
      elsif Right_Length = 0 then
         if Filter (In_Only_Left) then -- Left_Length > 0
            Hash_Tables.Copy (Target, Length, Left, Left_Length, Copy => Copy);
         else
            Target := null;
            Length := 0;
         end if;
      else
         Target := null;
         Length := 0;
         declare
            I : Node_Access;
            New_Node : Node_Access;
         begin
            I := Left.First;
            while I /= null loop
               if Find_Node (Right, I, Equivalent) /= null then
                  if Filter (In_Both) then
                     Copy (New_Node, I);
                     Insert (Target, Length, I.Hash, New_Node);
                  end if;
               else
                  if Filter (In_Only_Left) then
                     Copy (New_Node, I);
                     Insert (Target, Length, I.Hash, New_Node);
                  end if;
               end if;
               I := I.Next;
            end loop;
            if Filter (In_Only_Right) then
               I := Right.First;
               while I /= null loop
                  if Find_Node (Left, I, Equivalent) = null then
                     Copy (New_Node, I);
                     Insert (Target, Length, I.Hash, New_Node);
                  end if;
                  I := I.Next;
               end loop;
            end if;
         end;
      end if;
   end Copying_Merge;

end Ada.Containers.Hash_Tables;
