package body Ada.Containers.Linked_Lists is

   procedure Reverse_Iterate (
      Last : Node_Access;
      Process : not null access procedure (Position : not null Node_Access))
   is
      Position : Node_Access := Last;
   begin
      while Position /= null loop
         Process (Position);
         Position := Previous (Position);
      end loop;
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
      I : Node_Access := Last;
   begin
      while I /= null loop
         if Equivalent (I, Params) then
            return I;
         end if;
         I := Previous (I);
      end loop;
      return null;
   end Reverse_Find;

   function Equivalent (
      Left_Last, Right_Last : Node_Access;
      Equivalent : not null access function (
         Left, Right : not null Node_Access)
         return Boolean)
      return Boolean
   is
      I : Node_Access := Left_Last;
      J : Node_Access := Right_Last;
   begin
      while I /= null and then J /= null loop
         if not Equivalent (I, J) then
            return False;
         end if;
         I := Previous (I);
         J := Previous (J);
      end loop;
      return I = null and then J = null;
   end Equivalent;

   procedure Free (
      First : in out Node_Access;
      Last : in out Node_Access;
      Length : in out Count_Type;
      Free : not null access procedure (Object : in out Node_Access))
   is
      Position : Node_Access := Last;
   begin
      while Position /= null loop
         declare
            Prev : constant Node_Access := Previous (Position);
         begin
            Free (Position);
            Length := Length - 1;
            Position := Prev;
         end;
      end loop;
      First := null;
      Last := null;
   end Free;

   procedure Copy (
      Target_First : out Node_Access;
      Target_Last : out Node_Access;
      Length : out Count_Type;
      Source_Last : Node_Access;
      Copy : not null access procedure (
         Target : out Node_Access;
         Source : not null Node_Access))
   is
      I : Node_Access := Source_Last;
      New_Node : Node_Access;
   begin
      Target_First := null;
      Target_Last := null;
      Length := 0;
      while I /= null loop
         Copy (New_Node, I);
         Insert (
            First => Target_First,
            Last => Target_Last,
            Length => Length,
            Before => Target_First,
            New_Item => New_Node);
         I := Previous (I);
      end loop;
   end Copy;

   procedure Reverse_Elements (
      Target_First : in out Node_Access;
      Target_Last : in out Node_Access;
      Length : in out Count_Type)
   is
      Source_First : Node_Access := Target_First;
      Source_Last : Node_Access := Target_Last;
      Source_Length : Count_Type := Length;
      Position : Node_Access;
   begin
      Target_First := null;
      Target_Last := null;
      Length := 0;
      while Source_Last /= null loop
         Position := Source_Last;
         Remove (Source_First, Source_Last, Source_Length, Position, null);
         Insert (Target_First, Target_Last, Length, null, Position);
      end loop;
   end Reverse_Elements;

   function Is_Sorted (
      Last : Node_Access;
      LT : not null access function (
         Left, Right : not null Node_Access)
         return Boolean)
      return Boolean
   is
      I : Node_Access := Last;
   begin
      if I = null then
         return True;
      else
         loop
            declare
               Prev : constant Node_Access := Previous (I);
            begin
               exit when Prev = null;
               if LT (I, Prev) then
                  return False;
               end if;
               I := Prev;
            end;
         end loop;
         return True;
      end if;
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
      Left_First : Node_Access := Target_First;
      Left_Last : Node_Access := Target_Last;
      Left_Length : Count_Type := Length;
      I : Node_Access := Left_Last;
      J : Node_Access := Source_Last;
   begin
      Target_First := null;
      Target_Last := null;
      Length := 0;
      while I /= null and then J /= null loop
         if LT (J, I) then
            declare
               Prev : constant Node_Access := Previous (I);
            begin
               Remove (Left_First, Left_Last, Left_Length, I, null);
               Insert (Target_First, Target_Last, Length, Target_First, I);
               I := Prev;
            end;
         else
            declare
               Prev : constant Node_Access := Previous (J);
            begin
               Remove (Source_First, Source_Last, Source_Length, J, null);
               Insert (Target_First, Target_Last, Length, Target_First, J);
               J := Prev;
            end;
         end if;
      end loop;
      while I /= null loop
         declare
            Prev : constant Node_Access := Previous (I);
         begin
            Remove (Left_First, Left_Last, Left_Length, I, null);
            Insert (Target_First, Target_Last, Length, Target_First, I);
            I := Prev;
         end;
      end loop;
      while J /= null loop
         declare
            Prev : constant Node_Access := Previous (J);
         begin
            Remove (Source_First, Source_Last, Source_Length, J, null);
            Insert (Target_First, Target_Last, Length, Target_First, J);
            J := Prev;
         end;
      end loop;
   end Merge;

   procedure Merge_Sort (
      Target_First : in out Node_Access;
      Target_Last : in out Node_Access;
      Length : in out Count_Type;
      LT : not null access function (
         Left, Right : not null Node_Access)
         return Boolean) is
   begin
      if Length >= 2 then
         declare
            Left_First : Node_Access;
            Left_Last : Node_Access;
            Left_Length : Count_Type;
            Right_First : Node_Access := Target_First;
            Right_Last : Node_Access := Target_Last;
            Right_Length : Count_Type := Length;
         begin
            Split (
               Left_First, Left_Last, Left_Length,
               Right_First, Right_Last, Right_Length,
               Length / 2);
            Merge_Sort (
               Left_First, Left_Last, Left_Length,
               LT => LT);
            Merge_Sort (
               Right_First, Right_Last, Right_Length,
               LT => LT);
            Merge (
               Left_First, Left_Last, Left_Length,
               Right_First, Right_Last, Right_Length,
               LT => LT);
            Target_First := Left_First;
            Target_Last := Left_Last;
            Length := Left_Length;
         end;
      end if;
   end Merge_Sort;

end Ada.Containers.Linked_Lists;
