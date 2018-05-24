with Ada.Containers.Linked_Lists;
package body Ada.Containers.Naked_Doubly_Linked_Lists is

   function Next (Position : not null Node_Access) return Node_Access is
   begin
      return Position.Next;
   end Next;

   function Previous (Position : not null Node_Access) return Node_Access is
   begin
      return Position.Previous;
   end Previous;

   procedure Iterate (
      First : Node_Access;
      Process : not null access procedure (Position : not null Node_Access))
   is
      Position : Node_Access := First;
   begin
      while Position /= null loop
         Process (Position);
         Position := Position.Next;
      end loop;
   end Iterate;

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

   function Find (
      First : Node_Access;
      Params : System.Address;
      Equivalent : not null access function (
         Right : not null Node_Access;
         Params : System.Address)
         return Boolean)
      return Node_Access
   is
      I : Node_Access := First;
   begin
      while I /= null loop
         if Equivalent (I, Params) then
            return I;
         end if;
         I := I.Next;
      end loop;
      return null;
   end Find;

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
      AN : Node_Access;
      BN : Node_Access;
      AP : Node_Access;
      BP : Node_Access;
   begin
      if After = Before then
         return False;
      else
         AN := After;
         BN := Before;
         AP := After.Previous;
         BP := Before.Previous;
         loop
            if BP = null or else AP = BN then
               return True;
            elsif AP = null or else BP = AN then
               return False;
            end if;
            AN := AN.Next;
            BN := BN.Next;
            if AN = null or else BN = AP then
               return True;
            elsif BN = null or else AN = BP then
               return False;
            end if;
            AP := AP.Previous;
            BP := BP.Previous;
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
      if Before /= null then
         New_Item.Previous := Before.Previous;
         Before.Previous := New_Item;
      else
         New_Item.Previous := Last;
         Last := New_Item;
      end if;
      New_Item.Next := Before;
      if First = Before then
         First := New_Item;
      else
         New_Item.Previous.Next := New_Item;
      end if;
      Length := Length + 1;
   end Insert;

   procedure Remove (
      First : in out Node_Access;
      Last : in out Node_Access;
      Length : in out Count_Type;
      Position : not null Node_Access;
      Next : Node_Access)
   is
      pragma Assert (Next = Position.Next);
      Previous : constant Node_Access := Position.Previous;
   begin
      if Previous /= null then
         pragma Assert (First /= Position);
         Previous.Next := Next;
      else
         pragma Assert (First = Position);
         First := Next;
      end if;
      if Next /= null then
         pragma Assert (Last /= Position);
         Next.Previous := Previous;
      else
         pragma Assert (Last = Position);
         Last := Previous;
      end if;
      Length := Length - 1;
   end Remove;

   procedure Swap_Links (
      First : in out Node_Access;
      Last : in out Node_Access;
      I, J : not null Node_Access) is
   begin
      if I /= J then
         declare
            I_Previous : constant Node_Access := I.Previous;
            I_Next : constant Node_Access := I.Next;
            J_Previous : constant Node_Access := J.Previous;
            J_Next : constant Node_Access := J.Next;
         begin
            if I_Previous = J then
               pragma Assert (J_Next = I);
               I.Next := J;
               J.Previous := I;
            else
               I.Next := J_Next;
               J.Previous := I_Previous;
               if I_Previous /= null then
                  I_Previous.Next := J;
               else
                  pragma Assert (I = First);
                  First := J;
               end if;
               if J_Next /= null then
                  J_Next.Previous := I;
               else
                  pragma Assert (J = Last);
                  Last := I;
               end if;
            end if;
            if J_Previous = I then
               pragma Assert (I_Next = J);
               J.Next := I;
               I.Previous := J;
            else
               J.Next := I_Next;
               I.Previous := J_Previous;
               if J_Previous /= null then
                  J_Previous.Next := I;
               else
                  pragma Assert (J = First);
                  First := I;
               end if;
               if I_Next /= null then
                  I_Next.Previous := J;
               else
                  pragma Assert (I = Last);
                  Last := J;
               end if;
            end if;
         end;
      end if;
   end Swap_Links;

   procedure Splice (
      Target_First : in out Node_Access;
      Target_Last : in out Node_Access;
      Length : in out Count_Type;
      Before : Node_Access;
      Source_First : in out Node_Access;
      Source_Last : in out Node_Access;
      Source_Length : in out Count_Type)
   is
      Previous : Node_Access;
   begin
      if Source_Last /= null then
         if Before /= null then
            Previous := Before.Previous;
            Before.Previous := Source_Last;
            Source_Last.Next := Before;
         else
            Previous := Target_Last;
            Target_Last := Source_Last;
            pragma Assert (Source_Last.Next = null);
         end if;
         Source_First.Previous := Previous;
         if Previous /= null then
            Previous.Next := Source_First;
         else
            pragma Assert (Target_First = null);
            Target_First := Source_First;
         end if;
         Length := Length + Source_Length;
         Source_First := null;
         Source_Last := null;
         Source_Length := 0;
      end if;
   end Splice;

   procedure Split (
      Target_First : out Node_Access;
      Target_Last : out Node_Access;
      Length : out Count_Type;
      Source_First : in out Node_Access;
      Source_Last : in out Node_Access;
      Source_Length : in out Count_Type;
      Count : Count_Type)
   is
      Before : Node_Access;
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
         Before := Source_First;
         for I in 1 .. Count loop
            Before := Before.Next;
         end loop;
         Target_First := Source_First;
         Target_Last := Before.Previous;
         Source_First := Before;
         Target_Last.Next := null;
         Source_First.Previous := null;
         Length := Count;
         Source_Length := Source_Length - Count;
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

end Ada.Containers.Naked_Doubly_Linked_Lists;
