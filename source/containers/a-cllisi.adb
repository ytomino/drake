package body Ada.Containers.Linked_Lists.Singly is

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

end Ada.Containers.Linked_Lists.Singly;
