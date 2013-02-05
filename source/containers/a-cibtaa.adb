--  reference:
--  http://www.geocities.jp/m_hiroi/light/pyalgo53.html
--  http://www.softpedia.com/get/Others/Home-Education/AA-Visual-2007.shtml
pragma Check_Policy (Dump, Off);
pragma Check_Policy (Dump_On_Removing, Off);
pragma Check_Policy (Validate, Off);
with Ada.Containers.Inside.Binary_Trees.Arne_Andersson.Debug;
with Ada.Unchecked_Conversion;
package body Ada.Containers.Inside.Binary_Trees.Arne_Andersson is

   type AA_Node_Access is access Node;

   function Downcast is new Unchecked_Conversion (Node_Access, AA_Node_Access);

   function Skew (T : not null Node_Access) return not null Node_Access;
   --  before:
   --     T
   --    / \
   --    L R
   --   / \
   --   A B
   --  after:
   --    L
   --   / \
   --   A T
   --    / \
   --    B R
   function Skew (T : not null Node_Access) return not null Node_Access is
      pragma Assert (T.Parent /= null); -- not root
      L : constant Node_Access := T.Left;
   begin
      if L /= null and then Downcast (L).Level = Downcast (T).Level then
         if T.Parent.Left = T then
            T.Parent.Left := L;
         else
            T.Parent.Right := L;
         end if;
         L.Parent := T.Parent;
         T.Parent := L;
         T.Left := L.Right;
         if T.Left /= null then
            T.Left.Parent := T;
         end if;
         L.Right := T;
         return L;
      else
         return T;
      end if;
   end Skew;

   procedure Skew (T : not null Node_Access);
   procedure Skew (T : not null Node_Access) is
      Dummy : constant not null Node_Access := Skew (T);
      pragma Unreferenced (Dummy);
   begin
      null;
   end Skew;

   function Split (T : not null Node_Access) return not null Node_Access;
   --  before:
   --    T
   --   / \
   --   A R
   --    / \
   --    B X
   --  after:
   --     R
   --    / \
   --    T X
   --   / \
   --   A B
   function Split (T : not null Node_Access) return not null Node_Access is
      pragma Assert (T.Parent /= null); -- not root
      R : constant Node_Access := T.Right;
   begin
      if R /= null
         and then R.Right /= null
         and then Downcast (T).Level = Downcast (R.Right).Level
      then
         if T.Parent.Left = T then
            T.Parent.Left := R;
         else
            T.Parent.Right := R;
         end if;
         R.Parent := T.Parent;
         T.Parent := R;
         T.Right := R.Left;
         if T.Right /= null then
            T.Right.Parent := T;
         end if;
         R.Left := T;
         Downcast (R).Level := Downcast (R).Level + 1;
         return R;
      else
         return T;
      end if;
   end Split;

   procedure Split (T : not null Node_Access);
   procedure Split (T : not null Node_Access) is
      Dummy : constant not null Node_Access := Split (T);
      pragma Unreferenced (Dummy);
   begin
      null;
   end Split;

   --  implementation

   procedure Insert (
      Container : in out Node_Access;
      Length : in out Count_Type;
      Before : Node_Access;
      New_Item : not null Node_Access)
   is
      Root_Body : Node := (Super => (Left => Container,
                                     Right => null,
                                     Parent => null),
                           Level => -1);
      Root : constant Node_Access := Root_Body.Super'Unrestricted_Access;
      Current : Node_Access;
   begin
      --  insert
      Downcast (New_Item).Level := 0;
      if Container = null then
         New_Item.Parent := null;
         New_Item.Left := null;
         New_Item.Right := null;
         Container := New_Item;
      elsif Before = null then
         New_Item.Parent := Last (Container);
         New_Item.Left := null;
         New_Item.Right := null;
         New_Item.Parent.Right := New_Item;
      elsif Before.Left = null then
         New_Item.Parent := Before;
         New_Item.Left := null;
         New_Item.Right := null;
         New_Item.Parent.Left := New_Item;
      else
         New_Item.Parent := Last (Before.Left);
         New_Item.Left := null;
         New_Item.Right := null;
         New_Item.Parent.Right := New_Item;
      end if;
      --  make virtual root to make it easy to parent access
      if Container /= null then
         pragma Assert (Container.Parent = null);
         Root.Left := Container;
         Container.Parent := Root;
      end if;
      --  rebalance
      Current := New_Item.Parent;
      while Current /= Root loop
         Current := Skew (Current);
         Current := Split (Current);
         Current := Current.Parent;
      end loop;
      --  restore virtual root
      Container := Root.Left;
      if Container /= null then
         pragma Assert (Container.Parent = Root);
         Container.Parent := null;
      end if;
      --  increment
      Length := Length + 1;
      pragma Check (Dump,
         Debug.Dump (Container, New_Item, Message => "inserted"));
      pragma Check (Validate, Debug.Validate (Container, Length));
   end Insert;

   procedure Remove (
      Container : in out Node_Access;
      Length : in out Count_Type;
      Position : not null Node_Access)
   is
      Root_Body : Node := (Super => (Left => Container,
                                     Right => null,
                                     Parent => null),
                           Level => -1);
      Root : constant Node_Access := Root_Body.Super'Unrestricted_Access;
      Leaf : Node_Access;
      Current : Node_Access; -- leveling point
   begin
      --  make virtual root to make it easy to parent access
      if Container /= null then
         pragma Assert (Container.Parent = null);
         Root.Left := Container;
         Container.Parent := Root;
      end if;
      --  removing item and swapping item and leaf
      if Position.Left /= null then
         Leaf := Last (Position.Left);
      elsif Position.Right /= null then
         Leaf := Position.Right;
         pragma Assert (Leaf.Left = null); -- be balanced
      else
         Leaf := Position;
      end if;
      if Leaf.Parent = Position then
         Current := Leaf;
      else
         Current := Leaf.Parent;
      end if;
      if Leaf.Parent.Left = Leaf then
         Leaf.Parent.Left := null;
      else
         Leaf.Parent.Right := null;
      end if;
      if Position /= Leaf then
         Leaf.Parent := Position.Parent;
         if Position.Parent.Left = Position then
            Position.Parent.Left := Leaf;
         else
            Position.Parent.Right := Leaf;
         end if;
         Leaf.Left := Position.Left;
         if Position.Left /= null then
            Position.Left.Parent := Leaf;
         end if;
         Leaf.Right := Position.Right;
         if Position.Right /= null then
            Position.Right.Parent := Leaf;
         end if;
         Downcast (Leaf).Level := Downcast (Position).Level;
      end if;
      --  rebalance
      while Current /= Root loop
         if (Current.Left = null and then Downcast (Current).Level > 0)
            or else (Current.Left /= null
               and then
                  Downcast (Current).Level > Downcast (Current.Left).Level + 1)
            or else (Current.Right = null
               and then Downcast (Current).Level > 0)
            or else (Current.Right /= null
               and then Downcast (Current).Level >
                  Downcast (Current.Right).Level + 1)
         then
            pragma Check (Dump_On_Removing,
               Debug.Dump (Root.Left, Current, "removed a"));
            Downcast (Current).Level := Downcast (Current).Level - 1;
            if Current.Right /= null
               and then
                  Downcast (Current.Right).Level > Downcast (Current).Level
            then
               Downcast (Current.Right).Level := Downcast (Current).Level;
            end if;
            pragma Check (Dump_On_Removing,
               Debug.Dump (Root.Left, Current, "removed b"));
            Current := Skew (Current);
            if Current.Right /= null then
               pragma Check (Dump_On_Removing,
                  Debug.Dump (Root.Left, Current, "removed c"));
               Skew (Current.Right);
               if Current.Right.Right /= null then
                  pragma Check (Dump_On_Removing,
                     Debug.Dump (Root.Left, Current, "removed d"));
                  Skew (Current.Right.Right);
               end if;
            end if;
            pragma Check (Dump_On_Removing,
               Debug.Dump (Root.Left, Current, "removed e"));
            Current := Split (Current);
            if Current.Right /= null then
               pragma Check (Dump_On_Removing,
                  Debug.Dump (Root.Left, Current, "removed f"));
               Split (Current.Right);
            end if;
         end if;
         Current := Current.Parent;
      end loop;
      --  restore virtual root
      Container := Root.Left;
      if Container /= null then
         pragma Assert (Container.Parent = Root);
         Container.Parent := null;
      end if;
      --  decrement
      Length := Length - 1;
      pragma Check (Dump, Debug.Dump (Container, null, Message => "removed"));
      pragma Check (Validate, Debug.Validate (Container, Length));
   end Remove;

   procedure Copy (Target : out Node_Access;
                   Length : out Count_Type;
                   Source : Node_Access;
      Copy : not null access procedure (Target : out Node_Access;
                                        Source : not null Node_Access))
   is
      procedure Process (Target : out Node_Access;
                         Parent : Node_Access;
                         Source : Node_Access);
      procedure Process (Target : out Node_Access;
                         Parent : Node_Access;
                         Source : Node_Access) is
      begin
         if Source = null then
            Target := null;
         else
            Copy (Target, Source);
            Length := Length + 1;
            Target.Parent := Parent;
            Downcast (Target).Level := Downcast (Source).Level;
            Process (Target.Left, Target, Source.Left);
            Process (Target.Right, Target, Source.Right);
         end if;
      end Process;
   begin
      Length := 0;
      Process (Target, null, Source);
   end Copy;

end Ada.Containers.Inside.Binary_Trees.Arne_Andersson;
