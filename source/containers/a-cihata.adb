with Ada.Unchecked_Deallocation;
package body Ada.Containers.Inside.Hash_Tables is

   procedure Free is new Unchecked_Deallocation (Table, Table_Access);

   function Find_Node (
      Container : Table_Access;
      Node : not null Node_Access;
      Equivalent : not null access function (
         Left : not null Node_Access;
         Right : not null Node_Access)
         return Boolean)
      return Node_Access;
   function Find_Node (
      Container : Table_Access;
      Node : not null Node_Access;
      Equivalent : not null access function (
         Left : not null Node_Access;
         Right : not null Node_Access)
         return Boolean)
      return Node_Access is
   begin
      if Container /= null then
         declare
            Index : constant Hash_Type :=
               Node.Hash rem Container.Entries'Length;
            Position : Node_Access := Container.Entries (Index).First;
         begin
            if Position /= null then
               loop
                  if Equivalent (Node, Position) then
                     return Position;
                  end if;
                  exit when Position = Container.Entries (Index).Last;
                  pragma Assert (Position.Next /= null);
                  Position := Position.Next;
               end loop;
            end if;
         end;
      end if;
      return null;
   end Find_Node;

   procedure Insert_No_Rebuild (
      Container : Table_Access;
      Hash : Hash_Type;
      New_Item : not null Node_Access);
   procedure Insert_No_Rebuild (
      Container : Table_Access;
      Hash : Hash_Type;
      New_Item : not null Node_Access)
   is
      Index : constant Hash_Type := Hash rem Container.Entries'Length;
   begin
      New_Item.Hash := Hash;
      New_Item.Index := Index;
      if Container.Entries (Index).Last /= null then
         New_Item.Next := Container.Entries (Index).Last.Next;
         Container.Entries (Index).Last.Next := New_Item;
         Container.Entries (Index).Last := New_Item;
      else
         pragma Assert (Container.Entries (Index).First = null);
         New_Item.Next := null;
         if Index > 0 then
            for J in reverse 0 .. Index - 1 loop
               if Container.Entries (J).Last /= null then
                  New_Item.Next := Container.Entries (J).Last.Next;
                  Container.Entries (J).Last.Next := New_Item;
                  goto Linked;
               end if;
            end loop;
         end if;
         for J in Index + 1 .. Container.Entries'Last loop
            if Container.Entries (J).First /= null then
               New_Item.Next := Container.Entries (J).First;
               exit;
            end if;
         end loop;
      <<Linked>>
         Container.Entries (Index).First := New_Item;
         Container.Entries (Index).Last := New_Item;
      end if;
   end Insert_No_Rebuild;

   function First (Container : Table_Access) return Node_Access is
   begin
      if Container /= null then
         for I in Container.Entries'Range loop
            declare
               Position : constant Node_Access := Container.Entries (I).First;
            begin
               if Position /= null then
                  return Position;
               end if;
            end;
         end loop;
      end if;
      return null;
   end First;

   function Last (Container : Table_Access) return Node_Access is
   begin
      if Container /= null then
         for I in reverse Container.Entries'Range loop
            declare
               Position : constant Node_Access := Container.Entries (I).Last;
            begin
               if Position /= null then
                  return Position;
               end if;
            end;
         end loop;
      end if;
      return null;
   end Last;

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
            Index : constant Hash_Type := Hash rem Container.Entries'Length;
            Position : Node_Access := Container.Entries (Index).First;
         begin
            if Position /= null then
               loop
                  if Equivalent (Position, Params) then
                     return Position;
                  end if;
                  exit when Position = Container.Entries (Index).Last;
                  pragma Assert (Position.Next /= null);
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
         Left : not null Node_Access;
         Right : not null Node_Access)
         return Boolean)
      return Boolean is
   begin
      return Left_Length = Right_Length and then
         Is_Subset (Left, Right, Equivalent);
   end Equivalent;

   function Overlap (
      Left, Right : Table_Access;
      Equivalent : not null access function (
         Left : not null Node_Access;
         Right : not null Node_Access)
         return Boolean)
      return Boolean is
   begin
      if Left = null or else Right = null then
         return False;
      else
         declare
            Left_Position : Node_Access := First (Left);
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
         Left : not null Node_Access;
         Right : not null Node_Access)
         return Boolean)
      return Boolean is
   begin
      if Subset = null or else Of_Set = null then
         return Subset = null;
      else
         declare
            Left_Position : Node_Access := First (Subset);
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
      Free : not null access procedure (Object : in out Node_Access))
   is
      Position : Node_Access := First (Container);
   begin
      while Position /= null loop
         declare
            Next : constant Node_Access := Position.Next;
         begin
            Free (Position);
            Position := Next;
         end;
      end loop;
      Hash_Tables.Free (Container);
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
      Length := 0;
      if New_Capacity > 0 then
         Target := new Table'(
            Last => Hash_Type (New_Capacity) - 1,
            Entries => (others => (First => null, Last => null)));
         declare
            Position : Node_Access := First (Source);
            New_Node : Node_Access;
         begin
            while Position /= null loop
               Copy (New_Node, Position);
               Insert_No_Rebuild (Target, Position.Hash, New_Node);
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
            Position : Node_Access := First (Container);
         begin
            Free (Container);
            if New_Capacity > 0 then
               Container := new Table'(
                  Last => Hash_Type (New_Capacity) - 1,
                  Entries => (others => (First => null, Last => null)));
               while Position /= null loop
                  declare
                     Next : constant Node_Access := Position.Next;
                  begin
                     Insert_No_Rebuild (Container, Position.Hash, Position);
                     Position := Next;
                  end;
               end loop;
            else
               pragma Assert (Position = null);
               null;
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
         if Item.Next /= null and then Item.Next.Index = Item.Index then
            Container.Entries (Item.Index).First := Item.Next;
         else
            Container.Entries (Item.Index).First := null;
         end if;
         if Item.Index > 0 then
            for I in reverse 0 .. Item.Index - 1 loop
               if Container.Entries (I).Last /= null then
                  pragma Assert (Container.Entries (I).Last.Next = Item);
                  Container.Entries (I).Last.Next := Item.Next;
                  exit;
               end if;
            end loop;
         end if;
         Previous := null;
      else
         Previous := Container.Entries (Item.Index).First;
         while Previous.Next /= Item loop
            Previous := Previous.Next;
         end loop;
         Previous.Next := Item.Next;
      end if;
      if Item = Container.Entries (Item.Index).Last then
         Container.Entries (Item.Index).Last := Previous;
      end if;
      Length := Length - 1;
   end Remove;

   procedure Merge (
      Target : in out Table_Access;
      Length : in out Count_Type;
      Source : Table_Access;
      Source_Length : Count_Type;
      In_Only_Left : Boolean;
      In_Only_Right : Boolean;
      In_Both : Boolean;
      Equivalent : not null access function (
         Left : not null Node_Access;
         Right : not null Node_Access)
         return Boolean;
      Copy : access procedure (
         Target : out Node_Access;
         Source : not null Node_Access);
      Free : access procedure (Object : in out Node_Access)) is
   begin
      if Length = 0 and then Source_Length = 0 then
         null;
      elsif Length = 0 and then In_Only_Right then
         Hash_Tables.Free (Target);
         Hash_Tables.Copy (Target, Length, Source, Source_Length, Copy);
      elsif Source_Length = 0 and then In_Only_Left then
         null;
      elsif Length = 0 or else Source_Length = 0 then
         Hash_Tables.Free (Target, Length, Free);
      else
         declare
            I, Next : Node_Access;
            New_Node : Node_Access;
            From_Right : Node_Access := null;
         begin
            if In_Only_Right then
               I := First (Source);
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
            I := First (Target);
            while I /= null loop
               Next := I.Next;
               if Find_Node (Source, I, Equivalent) /= null then
                  if not In_Both then
                     Remove (Target, Length, I);
                     Free (I);
                  end if;
               else
                  if not In_Only_Left then
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

   procedure Merge (
      Target : out Table_Access;
      Length : out Count_Type;
      Left : Table_Access;
      Left_Length : Count_Type;
      Right : Table_Access;
      Right_Length : Count_Type;
      In_Only_Left : Boolean;
      In_Only_Right : Boolean;
      In_Both : Boolean;
      Equivalent : not null access function (
         Left : not null Node_Access;
         Right : not null Node_Access)
         return Boolean;
      Copy : not null access procedure (
         Target : out Node_Access;
         Source : not null Node_Access)) is
   begin
      if Left_Length = 0 and then Right_Length = 0 then
         Target := null;
         Length := 0;
      elsif Left_Length = 0 and then In_Only_Right then
         Hash_Tables.Copy (
            Target,
            Length,
            Right,
            Right_Length,
            Copy => Copy);
      elsif Right_Length = 0 and then In_Only_Left then
         Hash_Tables.Copy (
            Target,
            Length,
            Left,
            Left_Length,
            Copy => Copy);
      elsif Left_Length = 0 or else Right_Length = 0 then
         Target := null;
         Length := 0;
      else
         Target := null;
         Length := 0;
         declare
            I : Node_Access;
            New_Node : Node_Access;
         begin
            I := First (Left);
            while I /= null loop
               if Find_Node (Right, I, Equivalent) /= null then
                  if In_Both then
                     Copy (New_Node, I);
                     Insert (Target, Length, I.Hash, New_Node);
                  end if;
               else
                  if In_Only_Left then
                     Copy (New_Node, I);
                     Insert (Target, Length, I.Hash, New_Node);
                  end if;
               end if;
               I := I.Next;
            end loop;
            if In_Only_Right then
               I := First (Right);
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
   end Merge;

end Ada.Containers.Inside.Hash_Tables;
