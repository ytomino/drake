package body Ada.Containers.Inside.Linked_Lists is

   procedure Reverse_Iterate (
      Last : Node_Access;
      Process : not null access procedure (Position : not null Node_Access))
   is
      Position : Node_Access := Last;
   begin
      while Position /= null loop
         Process (Position);
         Position := Position.Previous;
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
         I := I.Previous;
      end loop;
      return null;
   end Reverse_Find;

   function Equivalent (
      Left_Last, Right_Last : Node_Access;
      Equivalent : not null access function (
         Left : not null Node_Access;
         Right : not null Node_Access)
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
         I := I.Previous;
         J := J.Previous;
      end loop;
      return I = null and then J = null;
   end Equivalent;

   procedure Copy (
      Target_First : out Node_Access;
      Target_Last : out Node_Access;
      Length : out Count_Type;
      Source_Last : Node_Access;
      Copy : not null access procedure (
         Target : out Node_Access;
         Source : not null Node_Access);
      Insert : not null access procedure (
         First : in out Node_Access;
         Last : in out Node_Access;
         Length : in out Count_Type;
         Before : Node_Access;
         New_Item : not null Node_Access))
   is
      I : Node_Access := Source_Last;
      New_Node : Node_Access;
   begin
      Target_First := null;
      Target_Last := null;
      Length := 0;
      while I /= null loop
         Copy (New_Node, I);
         Insert (First => Target_First,
                 Last => Target_Last,
                 Length => Length,
                 Before => Target_First,
                 New_Item => New_Node);
         I := I.Previous;
      end loop;
   end Copy;

   procedure Free (
      First : in out Node_Access;
      Last : in out Node_Access;
      Length : in out Count_Type;
      Free : not null access procedure (Object : in out Node_Access))
   is
      Position : Node_Access := Last;
      Previous : Node_Access;
   begin
      while Position /= null loop
         Previous := Position.Previous;
         Free (Position);
         Length := Length - 1;
         Position := Previous;
      end loop;
      First := null;
      Last := null;
   end Free;

   procedure Reverse_Elements (
      Target_First : in out Node_Access;
      Target_Last : in out Node_Access;
      Length : in out Count_Type;
      Insert : not null access procedure (
         First : in out Node_Access;
         Last : in out Node_Access;
         Length : in out Count_Type;
         Before : Node_Access;
         New_Item : not null Node_Access);
      Remove : not null access procedure (
         First : in out Node_Access;
         Last : in out Node_Access;
         Length : in out Count_Type;
         Position : not null Node_Access;
         Next : Node_Access))
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
      pragma Assert (Length = Length'Old);
   end Reverse_Elements;

   function Is_Sorted (
      Last : Node_Access;
      LT : not null access function (
         Left : not null Node_Access;
         Right : not null Node_Access)
         return Boolean)
      return Boolean
   is
      I : Node_Access := Last;
   begin
      if I = null then
         return True;
      else
         while I.Previous /= null loop
            if LT (I, I.Previous) then
               return False;
            end if;
            I := I.Previous;
         end loop;
         return True;
      end if;
   end Is_Sorted;

   procedure Merge_Sort (
      Target_First : in out Node_Access;
      Target_Last : in out Node_Access;
      Length : in out Count_Type;
      LT : not null access function (
         Left : not null Node_Access;
         Right : not null Node_Access)
         return Boolean;
      Splice : not null access procedure (
         Target_First : in out Node_Access;
         Target_Last : in out Node_Access;
         Length : in out Count_Type;
         Before : Node_Access;
         Source_First : in out Node_Access;
         Source_Last : in out Node_Access;
         Source_Length : in out Count_Type);
      Split : not null access procedure (
         Target_First : out Node_Access;
         Target_Last : out Node_Access;
         Length : out Count_Type;
         Source_First : in out Node_Access;
         Source_Last : in out Node_Access;
         Source_Length : in out Count_Type;
         Count : Count_Type);
      Insert : not null access procedure (
         First : in out Node_Access;
         Last : in out Node_Access;
         Length : in out Count_Type;
         Before : Node_Access;
         New_Item : not null Node_Access);
      Remove : not null access procedure (
         First : in out Node_Access;
         Last : in out Node_Access;
         Length : in out Count_Type;
         Position : not null Node_Access;
         Next : Node_Access)) is
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
            Split (Left_First, Left_Last, Left_Length,
                   Right_First, Right_Last, Right_Length,
                   Length / 2);
            Merge_Sort (Left_First, Left_Last, Left_Length,
                        LT, Splice, Split, Insert, Remove);
            Merge_Sort (Right_First, Right_Last, Right_Length,
                        LT, Splice, Split, Insert, Remove);
            Merge (Left_First, Left_Last, Left_Length,
                   Right_First, Right_Last, Right_Length,
                   LT, Insert, Remove);
            Target_First := Left_First;
            Target_Last := Left_Last;
            Length := Left_Length;
         end;
      end if;
   end Merge_Sort;

   procedure Merge (
      Target_First : in out Node_Access;
      Target_Last : in out Node_Access;
      Length : in out Count_Type;
      Source_First : in out Node_Access;
      Source_Last : in out Node_Access;
      Source_Length : in out Count_Type;
      LT : not null access function (
         Left : not null Node_Access;
         Right : not null Node_Access)
         return Boolean;
      Insert : not null access procedure (
         First : in out Node_Access;
         Last : in out Node_Access;
         Length : in out Count_Type;
         Before : Node_Access;
         New_Item : not null Node_Access);
      Remove : not null access procedure (
         First : in out Node_Access;
         Last : in out Node_Access;
         Length : in out Count_Type;
         Position : not null Node_Access;
         Next : Node_Access))
   is
      Left_First : Node_Access := Target_First;
      Left_Last : Node_Access := Target_Last;
      Left_Length : Count_Type := Length;
      I : Node_Access := Left_Last;
      J : Node_Access := Source_Last;
      Previous : Node_Access;
   begin
      Target_First := null;
      Target_Last := null;
      Length := 0;
      while I /= null and then J /= null loop
         if LT (J, I) then
            Previous := I.Previous;
            Remove (Left_First, Left_Last, Left_Length, I, null);
            Insert (Target_First, Target_Last, Length, Target_First, I);
            I := Previous;
         else
            Previous := J.Previous;
            Remove (Source_First, Source_Last, Source_Length, J, null);
            Insert (Target_First, Target_Last, Length, Target_First, J);
            J := Previous;
         end if;
      end loop;
      while I /= null loop
         Previous := I.Previous;
         Remove (Left_First, Left_Last, Left_Length, I, null);
         Insert (Target_First, Target_Last, Length, Target_First, I);
         I := Previous;
      end loop;
      while J /= null loop
         Previous := J.Previous;
         Remove (Source_First, Source_Last, Source_Length, J, null);
         Insert (Target_First, Target_Last, Length, Target_First, J);
         J := Previous;
      end loop;
   end Merge;

end Ada.Containers.Inside.Linked_Lists;
