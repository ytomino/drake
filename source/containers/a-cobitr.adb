package body Ada.Containers.Binary_Trees is

   function Nonnull_First (Container : not null Node_Access)
      return not null Node_Access;
   function Nonnull_First (Container : not null Node_Access)
      return not null Node_Access
   is
      I : not null Node_Access := Container;
   begin
      while I.Left /= null loop
         I := I.Left;
      end loop;
      return I;
   end Nonnull_First;

   function Nonnull_Last (Container : not null Node_Access)
      return not null Node_Access;
   function Nonnull_Last (Container : not null Node_Access)
      return not null Node_Access
   is
      I : not null Node_Access := Container;
   begin
      while I.Right /= null loop
         I := I.Right;
      end loop;
      return I;
   end Nonnull_Last;

   function Nonnull_Leaf_To_Root_First (Container : not null Node_Access)
      return not null Node_Access;
   function Nonnull_Leaf_To_Root_First (Container : not null Node_Access)
      return not null Node_Access
   is
      I : not null Node_Access := Container;
   begin
      loop
         if I.Left /= null then
            I := I.Left;
         elsif I.Right /= null then
            I := I.Right;
         else -- I.Left = null and then I.Right = null
            exit;
         end if;
      end loop;
      return I;
   end Nonnull_Leaf_To_Root_First;

   --  implementation

   function First (Container : Node_Access) return Node_Access is
   begin
      if Container = null then
         return null;
      else
         return Nonnull_First (Container);
      end if;
   end First;

   function Next (Item : not null Node_Access) return Node_Access is
   begin
      if Item.Right /= null then
         return Nonnull_First (Item.Right);
      else
         declare
            I : not null Node_Access := Item;
         begin
            while I.Parent /= null and then I.Parent.Right = I loop
               I := I.Parent;
            end loop;
            return I.Parent;
         end;
      end if;
   end Next;

   function Last (Container : Node_Access) return Node_Access is
   begin
      if Container = null then
         return null;
      else
         return Nonnull_Last (Container);
      end if;
   end Last;

   function Previous (Item : not null Node_Access) return Node_Access is
   begin
      if Item.Left /= null then
         return Nonnull_Last (Item.Left);
      else
         declare
            I : not null Node_Access := Item;
         begin
            while I.Parent /= null and then I.Parent.Left = I loop
               I := I.Parent;
            end loop;
            return I.Parent;
         end;
      end if;
   end Previous;

   procedure Iterate (
      Container : Node_Access;
      Process : not null access procedure (Position : not null Node_Access))
   is
      I : Node_Access := First (Container);
   begin
      while I /= null loop
         Process (I);
         I := Next (I);
      end loop;
   end Iterate;

   procedure Iterate (
      Container : Node_Access;
      Params : System.Address;
      Process : not null access procedure (
         Position : not null Node_Access;
         Params : System.Address))
   is
      I : Node_Access := First (Container);
   begin
      while I /= null loop
         Process (I, Params);
         I := Next (I);
      end loop;
   end Iterate;

   procedure Reverse_Iterate (
      Container : Node_Access;
      Process : not null access procedure (Position : not null Node_Access))
   is
      I : Node_Access := Last (Container);
   begin
      while I /= null loop
         Process (I);
         I := Previous (I);
      end loop;
   end Reverse_Iterate;

   function Leaf_To_Root_First (Container : Node_Access) return Node_Access is
   begin
      if Container = null then
         return null;
      else
         return Nonnull_Leaf_To_Root_First (Container);
      end if;
   end Leaf_To_Root_First;

   function Leaf_To_Root_Next (Item : not null Node_Access)
      return Node_Access is
   begin
      if Item.Parent = null then
         return null;
      elsif Item.Parent.Right /= null and then Item.Parent.Right /= Item then
         return Nonnull_Leaf_To_Root_First (Item.Parent.Right);
      else
         return Item.Parent;
      end if;
   end Leaf_To_Root_Next;

   procedure Root_To_Leaf_Next (
      Previous_Source_Item : not null Node_Access;
      Source_Parent_Item : out Node_Access;
      Previous_Target_Item : not null Node_Access;
      Target_Parent_Item : out Node_Access;
      Index : out Index_Type) is
   begin
      Source_Parent_Item := Previous_Source_Item;
      Target_Parent_Item := Previous_Target_Item;
      if Previous_Source_Item.Left /= null then
         Index := Left;
      elsif Previous_Source_Item.Right /= null then
         Index := Right;
      else
         loop
            declare
               P : constant Node_Access := Source_Parent_Item;
            begin
               Source_Parent_Item := Source_Parent_Item.Parent;
               Target_Parent_Item := Target_Parent_Item.Parent;
               exit when Source_Parent_Item = null
                  or else (
                     Source_Parent_Item.Right /= null
                     and then Source_Parent_Item.Right /= P);
            end;
         end loop;
         Index := Right;
      end if;
   end Root_To_Leaf_Next;

   function Find (
      Container : Node_Access;
      Mode : Find_Mode;
      Params : System.Address;
      Compare : not null access function (
         Right : not null Node_Access;
         Params : System.Address)
         return Integer)
      return Node_Access
   is
      Current : Node_Access := Container;
   begin
      if Current = null then
         return null;
      else
         loop
            declare
               Comparison : constant Integer := Compare (Current, Params);
            begin
               if Comparison < 0 then
                  if Current.Left = null then
                     case Mode is
                        when Just => return null;
                        when Floor => return Previous (Current);
                        when Ceiling => return Current;
                     end case;
                  else
                     Current := Current.Left;
                  end if;
               elsif Comparison > 0 then
                  if Current.Right = null then
                     case Mode is
                        when Just => return null;
                        when Floor => return Current;
                        when Ceiling => return Next (Current);
                     end case;
                  else
                     Current := Current.Right;
                  end if;
               else
                  return Current;
               end if;
            end;
         end loop;
      end if;
   end Find;

   function Equivalent (
      Left, Right : Node_Access;
      Equivalent : not null access function (
         Left, Right : not null Node_Access)
         return Boolean)
      return Boolean
   is
      I : Node_Access := First (Left);
      J : Node_Access := First (Right);
   begin
      while I /= null or else J /= null loop
         if I = null or else J = null or else not Equivalent (I, J) then
            return False;
         end if;
         I := Next (I);
         J := Next (J);
      end loop;
      return True;
   end Equivalent;

   function Overlap (Left, Right : Node_Access;
      Compare : not null access function (Left, Right : not null Node_Access)
         return Integer)
      return Boolean
   is
      I : Node_Access := First (Left);
      J : Node_Access := First (Right);
   begin
      while I /= null and then J /= null loop
         declare
            Comparison : constant Integer := Compare (I, J);
         begin
            if Comparison < 0 then
               I := Next (I);
            elsif Comparison > 0 then
               J := Next (J);
            else
               return True;
            end if;
         end;
      end loop;
      return False;
   end Overlap;

   function Is_Subset (Subset, Of_Set : Node_Access;
      Compare : not null access function (Left, Right : not null Node_Access)
         return Integer)
      return Boolean
   is
      I : Node_Access := First (Subset);
      J : Node_Access := First (Of_Set);
   begin
      while I /= null and then J /= null loop
         declare
            Comparison : constant Integer := Compare (I, J);
         begin
            if Comparison < 0 then
               return False;
            elsif Comparison > 0 then
               J := Next (J);
            else
               I := Next (I);
               J := Next (J);
            end if;
         end;
      end loop;
      return I = null;
   end Is_Subset;

   procedure Free (
      Container : in out Node_Access;
      Length : in out Count_Type;
      Free : not null access procedure (Object : in out Node_Access))
   is
      I : Node_Access := Leaf_To_Root_First (Container);
   begin
      while I /= null loop
         declare
            Next : constant Node_Access := Leaf_To_Root_Next (I);
         begin
            Free (I);
            Length := Length - 1;
            I := Next;
         end;
      end loop;
      Container := null;
      pragma Assert (Length = 0);
   end Free;

   procedure Merge (
      Target : in out Node_Access;
      Length : in out Count_Type;
      Source : Node_Access;
      Filter : Filter_Type;
      Compare : not null access function (Left, Right : not null Node_Access)
         return Integer;
      Copy : access procedure (
         Target : out Node_Access;
         Source : not null Node_Access);
      Insert : access procedure (
         Container : in out Node_Access;
         Length : in out Count_Type;
         Before : Node_Access;
         New_Item : not null Node_Access);
      Remove : access procedure (
         Container : in out Node_Access;
         Length : in out Count_Type;
         Position : not null Node_Access);
      Free : access procedure (Object : in out Node_Access))
   is
      New_Node : Node_Access;
      N : Node_Access;
      I : Node_Access := First (Target);
      J : Node_Access := First (Source);
   begin
      while I /= null and then J /= null loop
         declare
            Comparison : constant Integer := Compare (I, J);
         begin
            if Comparison < 0 then
               N := Next (I);
               if not Filter (In_Only_Left) then
                  Remove (Target, Length, I);
                  Free (I);
               end if;
               I := N;
            elsif Comparison > 0 then
               if Filter (In_Only_Right) then
                  Copy (New_Node, J);
                  Insert (Target, Length, I, New_Node);
               end if;
               J := Next (J);
            else
               N := Next (I);
               if not Filter (In_Both) then
                  Remove (Target, Length, I);
                  Free (I);
               end if;
               I := N;
               J := Next (J);
            end if;
         end;
      end loop;
      if not Filter (In_Only_Left) then
         while I /= null loop
            N := Next (I);
            Remove (Target, Length, I);
            Free (I);
            I := N;
         end loop;
      end if;
      if Filter (In_Only_Right) then
         while J /= null loop
            Copy (New_Node, J);
            Insert (Target, Length, null, New_Node);
            J := Next (J);
         end loop;
      end if;
   end Merge;

   procedure Merge (
      Target : out Node_Access;
      Length : out Count_Type;
      Left, Right : Node_Access;
      Filter : Filter_Type;
      Compare : not null access function (Left, Right : not null Node_Access)
         return Integer;
      Copy : not null access procedure (
         Target : out Node_Access;
         Source : not null Node_Access);
      Insert : not null access procedure (
         Container : in out Node_Access;
         Length : in out Count_Type;
         Before : Node_Access;
         New_Item : not null Node_Access))
   is
      New_Node : Node_Access;
      I : Node_Access := First (Left);
      J : Node_Access := First (Right);
   begin
      Length := 0;
      while I /= null and then J /= null loop
         declare
            Comparison : constant Integer := Compare (I, J);
         begin
            if Comparison < 0 then
               if Filter (In_Only_Left) then
                  Copy (New_Node, I);
                  Insert (Target, Length, null, New_Node);
               end if;
               I := Next (I);
            elsif Comparison > 0 then
               if Filter (In_Only_Right) then
                  Copy (New_Node, J);
                  Insert (Target, Length, null, New_Node);
               end if;
               J := Next (J);
            else
               if Filter (In_Both) then
                  Copy (New_Node, I);
                  Insert (Target, Length, null, New_Node);
               end if;
               I := Next (I);
               J := Next (J);
            end if;
         end;
      end loop;
      if Filter (In_Only_Left) then
         while I /= null loop
            Copy (New_Node, I);
            Insert (Target, Length, null, New_Node);
            I := Next (I);
         end loop;
      end if;
      if Filter (In_Only_Right) then
         while J /= null loop
            Copy (New_Node, J);
            Insert (Target, Length, null, New_Node);
            J := Next (J);
         end loop;
      end if;
   end Merge;

end Ada.Containers.Binary_Trees;
