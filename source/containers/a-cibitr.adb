package body Ada.Containers.Inside.Binary_Trees is

   function First (Container : Node_Access) return Node_Access is
   begin
      if Container = null then
         return null;
      else
         declare
            It : not null Node_Access := Container;
         begin
            while It.Left /= null loop
               It := It.Left;
            end loop;
            return It;
         end;
      end if;
   end First;

   function Next (Item : not null Node_Access) return Node_Access is
      It : not null Node_Access := Item;
   begin
      if It.Right /= null then
         It := It.Right;
         while It.Left /= null loop
            It := It.Left;
         end loop;
         return It;
      else
         while It.Parent /= null and then It.Parent.Right = It loop
            It := It.Parent;
         end loop;
         return It.Parent;
      end if;
   end Next;

   function Last (Container : Node_Access) return Node_Access is
   begin
      if Container = null then
         return null;
      else
         declare
            It : not null Node_Access := Container;
         begin
            while It.Right /= null loop
               It := It.Right;
            end loop;
            return It;
         end;
      end if;
   end Last;

   function Previous (Item : not null Node_Access) return Node_Access is
      It : not null Node_Access := Item;
   begin
      if It.Left /= null then
         It := It.Left;
         while It.Right /= null loop
            It := It.Right;
         end loop;
         return It;
      else
         while It.Parent /= null and then It.Parent.Left = It loop
            It := It.Parent;
         end loop;
         return It.Parent;
      end if;
   end Previous;

   procedure Iterate (
      Container : Node_Access;
      Process : not null access procedure (Position : not null Node_Access)) is
   begin
      if Container /= null then
         Iterate (Container.Left, Process);
         Process (Container);
         Iterate (Container.Right, Process);
      end if;
   end Iterate;

   procedure Iterate (
      Container : Node_Access;
      Params : System.Address;
      Process : not null access procedure (
         Position : not null Node_Access;
         Params : System.Address)) is
   begin
      if Container /= null then
         Iterate (Container.Left, Params, Process);
         Process (Container, Params);
         Iterate (Container.Right, Params, Process);
      end if;
   end Iterate;

   procedure Reverse_Iterate (
      Container : Node_Access;
      Process : not null access procedure (Position : not null Node_Access)) is
   begin
      if Container /= null then
         Reverse_Iterate (Container.Right, Process);
         Process (Container);
         Reverse_Iterate (Container.Left, Process);
      end if;
   end Reverse_Iterate;

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
      Free : not null access procedure (Object : in out Node_Access)) is
   begin
      if Container /= null then
         Binary_Trees.Free (Container.Left, Length, Free);
         Binary_Trees.Free (Container.Right, Length, Free);
         Free (Container);
         Length := Length - 1;
      end if;
   end Free;

   procedure Merge (
      Target : in out Node_Access;
      Length : in out Count_Type;
      Source : Node_Access;
      In_Only_Left : Boolean;
      In_Only_Right : Boolean;
      In_Both : Boolean;
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
               if not In_Only_Left then
                  Remove (Target, Length, I);
                  Free (I);
               end if;
               I := N;
            elsif Comparison > 0 then
               if In_Only_Right then
                  Copy (New_Node, J);
                  Insert (Target, Length, I, New_Node);
               end if;
               J := Next (J);
            else
               N := Next (I);
               if not In_Both then
                  Remove (Target, Length, I);
                  Free (I);
               end if;
               I := N;
               J := Next (J);
            end if;
         end;
      end loop;
      if not In_Only_Left then
         while I /= null loop
            N := Next (I);
            Remove (Target, Length, I);
            Free (I);
            I := N;
         end loop;
      end if;
      if In_Only_Right then
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
      Left : Node_Access;
      Right : Node_Access;
      In_Only_Left : Boolean;
      In_Only_Right : Boolean;
      In_Both : Boolean;
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
               if In_Only_Left then
                  Copy (New_Node, I);
                  Insert (Target, Length, null, New_Node);
               end if;
               I := Next (I);
            elsif Comparison > 0 then
               if In_Only_Right then
                  Copy (New_Node, J);
                  Insert (Target, Length, null, New_Node);
               end if;
               J := Next (J);
            else
               if In_Both then
                  Copy (New_Node, I);
                  Insert (Target, Length, null, New_Node);
               end if;
               I := Next (I);
               J := Next (J);
            end if;
         end;
      end loop;
      if In_Only_Left then
         while I /= null loop
            Copy (New_Node, I);
            Insert (Target, Length, null, New_Node);
            I := Next (I);
         end loop;
      end if;
      if In_Only_Right then
         while J /= null loop
            Copy (New_Node, J);
            Insert (Target, Length, null, New_Node);
            J := Next (J);
         end loop;
      end if;
   end Merge;

end Ada.Containers.Inside.Binary_Trees;
