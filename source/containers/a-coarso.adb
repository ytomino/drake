--  reference:
--  http://thomas.baudel.name/Visualisation/VisuTri/inplacestablesort.html
package body Ada.Containers.Array_Sorting is

   function GCD (X, Y : Positive) return Positive;
   function GCD (X, Y : Positive) return Positive is
      X2 : Natural := X;
      Y2 : Natural := Y;
   begin
      if X2 < Y2 then
         declare
            T : constant Natural := X2;
         begin
            X2 := Y2;
            Y2 := T;
         end;
      end if;
      while Y2 /= 0 loop
         pragma Assert (X2 >= Y2);
         declare
            R : constant Natural := X2 rem Y2;
         begin
            X2 := Y2;
            Y2 := R;
         end;
      end loop;
      return X2;
   end GCD;

   --  implementation

   function Is_Sorted (
      First, Last : Integer;
      Params : System.Address;
      LT : not null access function (
         Left, Right : Integer;
         Params : System.Address)
         return Boolean)
      return Boolean is
   begin
      if First < Last then
         for I in First .. Last - 1 loop
            if LT (I + 1, I, Params) then
               return False;
            end if;
         end loop;
      end if;
      return True;
   end Is_Sorted;

   procedure Insertion_Sort (
      First, Last : Integer;
      Params : System.Address;
      LT : not null access function (
         Left, Right : Integer;
         Params : System.Address)
         return Boolean;
      Swap : not null access procedure (
         I, J : Integer;
         Params : System.Address)) is
   begin
      for I in First + 1 .. Last loop
         for J in reverse First .. I - 1 loop
            exit when not LT (J + 1, J, Params);
            Swap (J, J + 1, Params);
         end loop;
      end loop;
   end Insertion_Sort;

   procedure In_Place_Merge_Sort (
      First, Last : Integer;
      Params : System.Address;
      LT : not null access function (
         Left, Right : Integer;
         Params : System.Address)
         return Boolean;
      Swap : not null access procedure (
         I, J : Integer;
         Params : System.Address)) is
   begin
      if First < Last then
         declare
            Before : constant Integer := First + (Last - First) / 2 + 1;
         begin
            In_Place_Merge_Sort (First, Before - 1, Params, LT, Swap);
            In_Place_Merge_Sort (Before, Last, Params, LT, Swap);
            In_Place_Merge (First, Before, Last, Params, LT, Swap);
         end;
      end if;
   end In_Place_Merge_Sort;

   procedure In_Place_Merge (
      First, Before, Last : Integer;
      Params : System.Address;
      LT : not null access function (
         Left, Right : Integer;
         Params : System.Address)
         return Boolean;
      Swap : not null access procedure (
         I, J : Integer;
         Params : System.Address))
   is
      First_Cut, Second_Cut, New_Before, L, H, M : Integer;
   begin
      if First < Before and then Before <= Last then
         if First + 1 = Last then
            if LT (Last, First, Params) then
               Swap (First, Last, Params);
            end if;
         else
            if Before - First > Last - Before then -- >= Last - Before + 1
               First_Cut := First + (Before - First) / 2;
               L := Before;
               H := Last;
               loop
                  M := L + (H + 1 - L) / 2;
                  if LT (M, First_Cut, Params) then
                     L := M;
                     exit when L >= H;
                  else
                     H := M - 1; -- not includes equiv.
                     exit when H < Before;
                  end if;
               end loop;
               Second_Cut := H;
            else
               Second_Cut := Before + (Last - Before) / 2;
               L := First;
               H := Before - 1;
               loop
                  M := L + (H - L) / 2;
                  if LT (Second_Cut, M, Params) then
                     H := M;
                     exit when L >= H;
                  else
                     L := M + 1; -- not includes equiv.
                     exit when L >= Before;
                  end if;
               end loop;
               First_Cut := L;
            end if;
            --  swap with Reverse_Rotate or Juggling_Rotate
            Juggling_Rotate (First_Cut, Before, Second_Cut, Params, Swap);
            --  merge
            New_Before := First_Cut + (Second_Cut - Before) + 1;
            In_Place_Merge (
               First,
               First_Cut,
               New_Before - 1,
               Params,
               LT => LT,
               Swap => Swap);
            In_Place_Merge (
               New_Before,
               Second_Cut + 1,
               Last,
               Params,
               LT => LT,
               Swap => Swap);
         end if;
      end if;
   end In_Place_Merge;

   procedure In_Place_Reverse (
      First, Last : Integer;
      Params : System.Address;
      Swap : not null access procedure (
         I, J : Integer;
         Params : System.Address))
   is
      I : Integer := First;
      J : Integer := Last;
   begin
      while I < J loop
         Swap (I, J, Params);
         I := I + 1;
         J := J - 1;
      end loop;
   end In_Place_Reverse;

   procedure Reverse_Rotate (
      First, Before, Last : Integer;
      Params : System.Address;
      Swap : not null access procedure (
         I, J : Integer;
         Params : System.Address)) is
   begin
      if First < Before and then Before <= Last then
         In_Place_Reverse (First, Before - 1, Params, Swap);
         In_Place_Reverse (Before, Last, Params, Swap);
         In_Place_Reverse (First, Last, Params, Swap);
      end if;
   end Reverse_Rotate;

   procedure Juggling_Rotate (
      First, Before, Last : Integer;
      Params : System.Address;
      Swap : not null access procedure (
         I, J : Integer;
         Params : System.Address))
   is
      Left_Length : constant Integer := Before - First;
      Length : constant Integer := Last - First + 1;
   begin
      if Left_Length > 0 and then Length > Left_Length then
         declare
            Cycles : constant Positive := GCD (Length, Left_Length);
            P : Integer := First;
         begin
            loop
               declare
                  Q : Integer;
               begin
                  if P > Last - Left_Length then
                     Q := P - (Length - Left_Length);
                  else
                     Q := P + Left_Length;
                  end if;
                  exit when Q = First;
                  for I in 0 .. Cycles - 1 loop
                     Swap (P + I, Q + I, Params);
                  end loop;
                  P := Q;
               end;
            end loop;
         end;
      end if;
   end Juggling_Rotate;

end Ada.Containers.Array_Sorting;
