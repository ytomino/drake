package body Ada.Containers.Binary_Trees.Simple is

   procedure Insert (
      Container : in out Node_Access;
      Length : in out Count_Type;
      Before : Node_Access;
      New_Item : not null Node_Access) is
   begin
      Length := Length + 1;
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
   end Insert;

   procedure Remove (
      Container : in out Node_Access;
      Length : in out Count_Type;
      Position : not null Node_Access)
   is
      Alt : Node_Access;
   begin
      Length := Length - 1;
      if Position.Left = null then
         Alt := Position.Right;
      elsif Position.Right = null then
         Alt := Position.Left;
      else
         Alt := Position.Right;
         declare
            Left_Of_Alt : constant not null Node_Access := First (Alt);
         begin
            Left_Of_Alt.Left := Position.Left;
            Position.Left.Parent := Left_Of_Alt;
         end;
      end if;
      if Alt /= null then
         Alt.Parent := Position.Parent;
      end if;
      if Position.Parent /= null then
         if Position.Parent.Left = Position then
            Position.Parent.Left := Alt;
         else
            pragma Assert (Position.Parent.Right = Position);
            Position.Parent.Right := Alt;
         end if;
      else
         pragma Assert (Container = Position);
         Container := Alt;
      end if;
   end Remove;

   procedure Copy (
      Target : out Node_Access;
      Length : out Count_Type;
      Source : Node_Access;
      Copy : not null access procedure (
         Target : out Node_Access;
         Source : not null Node_Access)) is
   begin
      Length := 0;
      if Source /= null then
         Length := Length + 1;
         Copy (Target, Source);
         Target.Parent := null;
         declare
            Source_Item : Node_Access := Source;
            Target_Item : Node_Access := Target;
         begin
            loop
               declare
                  Source_Parent_Item : Node_Access;
                  Target_Parent_Item : Node_Access;
                  Index : Index_Type;
               begin
                  Root_To_Leaf_Next (
                     Previous_Source_Item => Source_Item,
                     Source_Parent_Item => Source_Parent_Item,
                     Previous_Target_Item => Target_Item,
                     Target_Parent_Item => Target_Parent_Item,
                     Index => Index);
                  exit when Source_Parent_Item = null;
                  case Index is
                     when Left =>
                        Source_Item := Source_Parent_Item.Left;
                     when Right =>
                        Source_Item := Source_Parent_Item.Right;
                  end case;
                  Length := Length + 1;
                  Copy (Target_Item, Source_Item);
                  Target_Item.Parent := Target_Parent_Item;
                  case Index is
                     when Left =>
                        Target_Parent_Item.Left := Target_Item;
                     when Right =>
                        Target_Parent_Item.Right := Target_Item;
                  end case;
               end;
            end loop;
         end;
      end if;
   end Copy;

end Ada.Containers.Binary_Trees.Simple;
