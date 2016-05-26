--  reference:
--  http://www.geocities.jp/m_hiroi/light/pyalgo53.html
--  http://www.softpedia.com/get/Others/Home-Education/AA-Visual-2007.shtml
pragma Check_Policy (
   Dump => Disable,
   Dump_On_Removing => Disable,
   Validate => Disable);
--  with Ada.Containers.Binary_Trees.Arne_Andersson.Debug;
with Ada.Unchecked_Conversion;
package body Ada.Containers.Binary_Trees.Arne_Andersson is

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
      L : constant Node_Access := T.Left;
   begin
      if L /= null and then Downcast (L).Level = Downcast (T).Level then
         declare
            Parent : constant Node_Access := T.Parent;
         begin
            if Parent /= null then
               if Parent.Left = T then
                  Parent.Left := L;
               else
                  Parent.Right := L;
               end if;
            end if;
            T.Parent := L;
            L.Parent := Parent;
         end;
         declare
            B : constant Node_Access := L.Right;
         begin
            if B /= null then
               B.Parent := T;
            end if;
            T.Left := B;
            L.Right := T;
         end;
         return L;
      else
         return T;
      end if;
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
      R : constant Node_Access := T.Right;
   begin
      if R /= null
         and then R.Right /= null
         and then Downcast (T).Level = Downcast (R.Right).Level
      then
         declare
            Parent : constant Node_Access := T.Parent;
         begin
            if Parent /= null then
               if Parent.Left = T then
                  Parent.Left := R;
               else
                  Parent.Right := R;
               end if;
            end if;
            T.Parent := R;
            R.Parent := Parent;
         end;
         declare
            B : constant Node_Access := R.Left;
         begin
            if B /= null then
               B.Parent := T;
            end if;
            T.Right := B;
            R.Left := T;
         end;
         Downcast (R).Level := Downcast (R).Level + 1;
         return R;
      else
         return T;
      end if;
   end Split;

   --  implementation

   procedure Insert (
      Container : in out Node_Access;
      Length : in out Count_Type;
      Before : Node_Access;
      New_Item : not null Node_Access) is
   begin
      --  insert
      Downcast (New_Item).Level := 0;
      if Container = null then
         New_Item.Parent := null;
         New_Item.Left := null;
         New_Item.Right := null;
         Container := New_Item;
      else
         pragma Assert (Container.Parent = null);
         declare
            Current : Node_Access;
         begin
            if Before = null then
               Current := Last (Container);
               pragma Assert (Current.Right = null);
               New_Item.Parent := Current;
               New_Item.Left := null;
               New_Item.Right := null;
               Current.Right := New_Item;
            elsif Before.Left = null then
               Current := Before;
               pragma Assert (Current.Left = null);
               New_Item.Parent := Current;
               New_Item.Left := null;
               New_Item.Right := null;
               Current.Left := New_Item;
            else
               Current := Last (Before.Left);
               pragma Assert (Current.Right = null);
               New_Item.Parent := Current;
               New_Item.Left := null;
               New_Item.Right := null;
               Current.Right := New_Item;
            end if;
            --  rebalance
            loop
               Current := Skew (Current);
               Current := Split (Current);
               exit when Current.Parent = null;
               Current := Current.Parent;
            end loop;
            Container := Current;
         end;
      end if;
      --  increment
      Length := Length + 1;
      pragma Check (Dump,
         Check => Debug.Dump (Container, New_Item, Message => "inserted"));
      pragma Check (Validate, Debug.Valid (Container, Length));
   end Insert;

   procedure Remove (
      Container : in out Node_Access;
      Length : in out Count_Type;
      Position : not null Node_Access) is
   begin
      pragma Assert (Container /= null and then Length > 0);
      if Length = 1 then
         pragma Assert (
            Container = Position
            and then Position.Left = null
            and then Position.Right = null
            and then Position.Parent = null);
         Container := null;
      else
         declare
            Leaf : Node_Access;
            Current : Node_Access; -- leveling point
         begin
            --  removing item and swapping item and leaf
            if Position.Left /= null then
               Leaf := Last (Position.Left);
               if Leaf.Parent = Position then
                  Current := Leaf;
               else
                  Current := Leaf.Parent;
               end if;
            elsif Position.Right /= null then
               Leaf := Position.Right;
               pragma Assert (Leaf.Left = null); -- be balanced
               Current := Leaf;
            else
               Leaf := Position;
               Current := Leaf.Parent;
            end if;
            if Leaf.Parent.Left = Leaf then
               Leaf.Parent.Left := null;
            else
               Leaf.Parent.Right := null;
            end if;
            if Position /= Leaf then
               Leaf.Parent := Position.Parent;
               if Position.Parent = null then
                  pragma Assert (Container = Position);
                  Container := Leaf;
               elsif Position.Parent.Left = Position then
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
            loop
               if (Current.Left = null and then Downcast (Current).Level > 0)
                  or else (
                     Current.Left /= null
                     and then Downcast (Current).Level >
                        Downcast (Current.Left).Level + 1)
                  or else (
                     Current.Right = null
                     and then Downcast (Current).Level > 0)
                  or else (
                     Current.Right /= null
                     and then Downcast (Current).Level >
                        Downcast (Current.Right).Level + 1)
               then
                  pragma Check (Dump_On_Removing,
                     Check =>
                        Debug.Dump (
                           Debug.Root (Current),
                           Current,
                           "removed a"));
                  Downcast (Current).Level := Downcast (Current).Level - 1;
                  if Current.Right /= null
                     and then Downcast (Current.Right).Level >
                        Downcast (Current).Level
                  then
                     Downcast (Current.Right).Level :=
                        Downcast (Current).Level;
                  end if;
                  pragma Check (Dump_On_Removing,
                     Check =>
                        Debug.Dump (
                           Debug.Root (Current),
                           Current,
                           "removed b"));
                  Current := Skew (Current);
                  if Current.Right /= null then
                     pragma Check (Dump_On_Removing,
                        Check =>
                           Debug.Dump (
                              Debug.Root (Current),
                              Current,
                              "removed c"));
                     declare
                        Dummy : Node_Access;
                     begin
                        Dummy := Skew (Current.Right);
                     end;
                     if Current.Right.Right /= null then
                        pragma Check (Dump_On_Removing,
                           Check =>
                              Debug.Dump (
                                 Debug.Root (Current),
                                 Current,
                                 "removed d"));
                        declare
                           Dummy : Node_Access;
                        begin
                           Dummy := Skew (Current.Right.Right);
                        end;
                     end if;
                  end if;
                  pragma Check (Dump_On_Removing,
                     Check =>
                        Debug.Dump (
                           Debug.Root (Current),
                           Current,
                           "removed e"));
                  Current := Split (Current);
                  if Current.Right /= null then
                     pragma Check (Dump_On_Removing,
                        Check =>
                           Debug.Dump (
                              Debug.Root (Current),
                              Current,
                              "removed f"));
                     declare
                        Dummy : Node_Access;
                     begin
                        Dummy := Split (Current.Right);
                     end;
                  end if;
               end if;
               exit when Current.Parent = null;
               Current := Current.Parent;
            end loop;
            Container := Current;
         end;
      end if;
      --  decrement
      Length := Length - 1;
      pragma Check (Dump, Debug.Dump (Container, null, Message => "removed"));
      pragma Check (Validate, Debug.Valid (Container, Length));
   end Remove;

   procedure Copy (
      Target : out Node_Access;
      Length : out Count_Type;
      Source : Node_Access;
      Copy : not null access procedure (
         Target : out Node_Access;
         Source : not null Node_Access))
   is
      procedure Process (
         Target : out Node_Access;
         Parent : Node_Access;
         Source : Node_Access);
      procedure Process (
         Target : out Node_Access;
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

end Ada.Containers.Binary_Trees.Arne_Andersson;
