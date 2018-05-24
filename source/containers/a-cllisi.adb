package body Ada.Containers.Linked_Lists.Singly is

   function Previous (Position : not null Node_Access) return Node_Access is
   begin
      return Position.Previous;
   end Previous;

   procedure Reverse_Iterate (
      Last : Node_Access;
      Process : not null access procedure (Position : not null Node_Access))
   is
      procedure Reverse_Iterate_Body is
         new Linked_Lists.Reverse_Iterate (Node, Node_Access);
      pragma Inline_Always (Reverse_Iterate_Body);
   begin
      Reverse_Iterate_Body (Last, Process => Process);
   end Reverse_Iterate;

   function Reverse_Find (
      Last : Node_Access;
      Params : System.Address;
      Equivalent : not null access function (
         Right : not null Node_Access;
         Params : System.Address)
         return Boolean)
      return Node_Access
   is
      function Reverse_Find_Body is
         new Linked_Lists.Reverse_Find (Node, Node_Access);
      pragma Inline_Always (Reverse_Find_Body);
   begin
      return Reverse_Find_Body (Last, Params, Equivalent => Equivalent);
   end Reverse_Find;

   function Is_Before (Before, After : Node_Access) return Boolean is
      I : Node_Access;
      J : Node_Access;
   begin
      if After = Before then
         return False;
      else
         I := After.Previous;
         J := Before.Previous;
         loop
            if J = null or else I = Before then
               return True;
            elsif I = null or else J = After then
               return False;
            else
               I := I.Previous;
               J := J.Previous;
            end if;
         end loop;
      end if;
   end Is_Before;

   function Equivalent (
      Left_Last, Right_Last : Node_Access;
      Equivalent : not null access function (
         Left, Right : not null Node_Access)
         return Boolean)
      return Boolean
   is
      function Equivalent_Body is
         new Linked_Lists.Equivalent (Node, Node_Access);
      pragma Inline_Always (Equivalent_Body);
   begin
      return Equivalent_Body (Left_Last, Right_Last, Equivalent => Equivalent);
   end Equivalent;

   procedure Free (
      First : in out Node_Access;
      Last : in out Node_Access;
      Length : in out Count_Type;
      Free : not null access procedure (Object : in out Node_Access))
   is
      procedure Free_Body is new Linked_Lists.Free (Node, Node_Access);
      pragma Inline_Always (Free_Body);
   begin
      Free_Body (First, Last, Length, Free => Free);
   end Free;

   procedure Insert (
      First : in out Node_Access;
      Last : in out Node_Access;
      Length : in out Count_Type;
      Before : Node_Access;
      New_Item : not null Node_Access) is
   begin
      if Before = null then
         New_Item.Previous := Last;
         Last := New_Item;
      else
         New_Item.Previous := Before.Previous;
         Before.Previous := New_Item;
      end if;
      if First = Before then
         First := New_Item;
      end if;
      Length := Length + 1;
   end Insert;

   procedure Remove (
      First : in out Node_Access;
      Last : in out Node_Access;
      Length : in out Count_Type;
      Position : not null Node_Access;
      Next : Node_Access) is
   begin
      if Next /= null then
         pragma Assert (Last /= Position);
         pragma Assert (Next.Previous = Position);
         Next.Previous := Position.Previous;
         if First = Position then
            First := Next;
         end if;
      else
         pragma Assert (Last = Position);
         Last := Position.Previous;
         if First = Position then
            pragma Assert (Last = null);
            First := null;
         end if;
      end if;
      Length := Length - 1;
   end Remove;

   procedure Split (
      Target_First : out Node_Access;
      Target_Last : out Node_Access;
      Length : out Count_Type;
      Source_First : in out Node_Access;
      Source_Last : in out Node_Access;
      Source_Length : in out Count_Type;
      Count : Count_Type) is
   begin
      if Count = 0 then
         Target_First := null;
         Target_Last := null;
         Length := 0;
      elsif Count = Source_Length then
         Target_First := Source_First;
         Target_Last := Source_Last;
         Length := Source_Length;
         Source_First := null;
         Source_Last := null;
         Source_Length := 0;
      else
         declare
            Before : Node_Access := Source_Last;
         begin
            for I in 1 .. Source_Length - Count - 1 loop
               Before := Before.Previous;
            end loop;
            Target_First := Source_First;
            Target_Last := Before.Previous;
            Source_First := Before;
            Source_First.Previous := null;
            Length := Count;
            Source_Length := Source_Length - Count;
         end;
      end if;
   end Split;

   procedure Copy (
      Target_First : out Node_Access;
      Target_Last : out Node_Access;
      Length : out Count_Type;
      Source_Last : Node_Access;
      Copy : not null access procedure (
         Target : out Node_Access;
         Source : not null Node_Access))
   is
      procedure Copy_Body is new Linked_Lists.Copy (Node, Node_Access);
      pragma Inline_Always (Copy_Body);
   begin
      Copy_Body (Target_First, Target_Last, Length, Source_Last, Copy => Copy);
   end Copy;

   procedure Reverse_Elements (
      Target_First : in out Node_Access;
      Target_Last : in out Node_Access;
      Length : in out Count_Type)
   is
      procedure Reverse_Elements_Body is
         new Linked_Lists.Reverse_Elements (Node, Node_Access);
      pragma Inline_Always (Reverse_Elements_Body);
   begin
      Reverse_Elements_Body (Target_First, Target_Last, Length);
   end Reverse_Elements;

   function Is_Sorted (
      Last : Node_Access;
      LT : not null access function (
         Left, Right : not null Node_Access)
         return Boolean)
      return Boolean
   is
      function Is_Sorted_Body is
         new Linked_Lists.Is_Sorted (Node, Node_Access);
      pragma Inline_Always (Is_Sorted_Body);
   begin
      return Is_Sorted_Body (Last, LT => LT);
   end Is_Sorted;

   procedure Merge (
      Target_First : in out Node_Access;
      Target_Last : in out Node_Access;
      Length : in out Count_Type;
      Source_First : in out Node_Access;
      Source_Last : in out Node_Access;
      Source_Length : in out Count_Type;
      LT : not null access function (
         Left, Right : not null Node_Access)
         return Boolean)
   is
      procedure Merge_Body is new Linked_Lists.Merge (Node, Node_Access);
      pragma Inline_Always (Merge_Body);
   begin
      Merge_Body (
         Target_First,
         Target_Last,
         Length,
         Source_First,
         Source_Last,
         Source_Length,
         LT => LT);
   end Merge;

   procedure Merge_Sort (
      Target_First : in out Node_Access;
      Target_Last : in out Node_Access;
      Length : in out Count_Type;
      LT : not null access function (
         Left, Right : not null Node_Access)
         return Boolean)
   is
      procedure Merge_Sort_Body is
         new Linked_Lists.Merge_Sort (Node, Node_Access);
         --  no inline, Merge_Sort uses recursive calling
   begin
      Merge_Sort_Body (Target_First, Target_Last, Length, LT => LT);
   end Merge_Sort;

end Ada.Containers.Linked_Lists.Singly;
