package body Ada.Containers.Inside.Binary_Trees.Simple is

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
            Process (Target.Left, Target, Source.Left);
            Process (Target.Right, Target, Source.Right);
         end if;
      end Process;
   begin
      Length := 0;
      Process (Target, null, Source);
   end Copy;

end Ada.Containers.Inside.Binary_Trees.Simple;
