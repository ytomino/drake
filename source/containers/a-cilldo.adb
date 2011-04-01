with Ada.Unchecked_Conversion;
package body Ada.Containers.Inside.Linked_Lists.Doubly is

   type Doubly_Node_Access is access Node;

   function Downcast is new Unchecked_Conversion (
      Node_Access,
      Doubly_Node_Access);

   procedure Iterate (
      First : Node_Access;
      Process : not null access procedure (Position : not null Node_Access))
   is
      Position : Node_Access := First;
   begin
      while Position /= null loop
         Process (Position);
         Position := Downcast (Position).Next;
      end loop;
   end Iterate;

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
         I := Downcast (I).Next;
      end loop;
      return null;
   end Find;

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
            AN := Downcast (AN).Next;
            BN := Downcast (BN).Next;
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
      Downcast (New_Item).Next := Before;
      if First = Before then
         First := New_Item;
      else
         Downcast (New_Item.Previous).Next := New_Item;
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
      pragma Assert (Next = Downcast (Position).Next);
      Previous : constant Node_Access := Position.Previous;
   begin
      if Previous /= null then
         pragma Assert (First /= Position);
         Downcast (Previous).Next := Next;
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
            I_Next : constant Node_Access := Downcast (I).Next;
            J_Previous : constant Node_Access := J.Previous;
            J_Next : constant Node_Access := Downcast (J).Next;
         begin
            if I_Previous = J then
               pragma Assert (J_Next = I);
               Downcast (I).Next := J;
               J.Previous := I;
            else
               Downcast (I).Next := J_Next;
               J.Previous := I_Previous;
               if I_Previous /= null then
                  Downcast (I_Previous).Next := J;
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
               Downcast (J).Next := I;
               I.Previous := J;
            else
               Downcast (J).Next := I_Next;
               I.Previous := J_Previous;
               if J_Previous /= null then
                  Downcast (J_Previous).Next := I;
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
            Downcast (Source_Last).Next := Before;
         else
            Previous := Target_Last;
            Target_Last := Source_Last;
            pragma Assert (Downcast (Source_Last).Next = null);
         end if;
         Source_First.Previous := Previous;
         if Previous /= null then
            Downcast (Previous).Next := Source_First;
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
            Before := Downcast (Before).Next;
         end loop;
         Target_First := Source_First;
         Target_Last := Before.Previous;
         Source_First := Before;
         Downcast (Target_Last).Next := null;
         Source_First.Previous := null;
         Length := Count;
         Source_Length := Source_Length - Count;
      end if;
   end Split;

end Ada.Containers.Inside.Linked_Lists.Doubly;
