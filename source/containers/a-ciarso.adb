--  reference:
--  http://thomas.baudel.name/Visualisation/VisuTri/inplacestablesort.html
pragma Check_Policy (Trace, Off);
package body Ada.Containers.Inside.Array_Sorting is

   function Is_Sorted (First, Last : Integer;
      LT : not null access function (Left, Right : Integer) return Boolean)
      return Boolean is
   begin
      if First < Last then
         for I in First .. Last - 1 loop
            if LT (I + 1, I) then
               return False;
            end if;
         end loop;
      end if;
      return True;
   end Is_Sorted;

   procedure Insertion_Sort (First, Last : Integer;
      LT : not null access function (Left, Right : Integer) return Boolean;
      Swap : not null access procedure (I, J : Integer)) is
   begin
      for I in First + 1 .. Last loop
         for J in reverse First .. I - 1 loop
            exit when not LT (J + 1, J);
            Swap (J, J + 1);
         end loop;
      end loop;
   end Insertion_Sort;

   procedure In_Place_Merge_Sort (First, Last : Integer;
      LT : not null access function (Left, Right : Integer) return Boolean;
      Swap : not null access procedure (I, J : Integer))
   is
   begin
      if First < Last then
         declare
            Middle : constant Integer := (First + Last) / 2;
         begin
            In_Place_Merge_Sort (First, Middle, LT, Swap);
            In_Place_Merge_Sort (Middle + 1, Last, LT, Swap);
            In_Place_Merge (First, Middle, Last, LT, Swap);
         end;
      end if;
   end In_Place_Merge_Sort;

   procedure In_Place_Merge (First, Middle, Last : Integer;
      LT : not null access function (Left, Right : Integer) return Boolean;
      Swap : not null access procedure (I, J : Integer))
   is
      First_Cut, Second_Cut, New_Middle, L, H, M : Integer;
   begin
      if First <= Middle and then Middle < Last then
         if First + 1 = Last then
            if LT (Last, First) then
               pragma Check (Trace,
                  Debug.Put ("Swap " & First'Img & Last'Img));
               Swap (First, Last);
            end if;
         else
            pragma Check (Trace,
               Debug.Put ("MA " & First'Img & Middle'Img & Last'Img));
            if Middle - First >= Last - Middle then
               First_Cut := (First + Middle + 1) / 2;
               L := Middle + 1;
               H := Last;
               loop
                  M := (L + H + 1) / 2;
                  pragma Check (Trace,
                     Debug.Put ("Mf " & M'Img));
                  if LT (M, First_Cut) then
                     L := M;
                     exit when L >= H;
                  else
                     H := M - 1; --  not includes equiv.
                     exit when H <= Middle;
                  end if;
               end loop;
               Second_Cut := H;
            else
               Second_Cut := (Middle + 1 + Last) / 2;
               L := First;
               H := Middle;
               loop
                  M := (L + H) / 2;
                  pragma Check (Trace,
                     Debug.Put ("Ms " & M'Img));
                  if LT (Second_Cut, M) then
                     H := M;
                     exit when L >= H;
                  else
                     L := M + 1; --  not includes equiv.
                     exit when L > Middle;
                  end if;
               end loop;
               First_Cut := L;
            end if;
            pragma Check (Trace,
               Debug.Put ("MB " & First_Cut'Img & Second_Cut'Img));
            if First_Cut <= Middle and then Middle < Second_Cut then
               --  swap [First_Cut .. Middle] and [Middle + 1.. Second_Cut]
               In_Place_Reverse (First_Cut, Middle, Swap);
               In_Place_Reverse (Middle + 1, Second_Cut, Swap);
               In_Place_Reverse (First_Cut, Second_Cut, Swap);
            end if;
            --  merge
            New_Middle := First_Cut + (Second_Cut - (Middle + 1));
            pragma Check (Trace, Debug.Put ("MC " & New_Middle'Img));
            In_Place_Merge (First, First_Cut - 1, New_Middle, LT, Swap);
            In_Place_Merge (New_Middle + 1, Second_Cut, Last, LT, Swap);
            pragma Check (Trace, Debug.Put ("ME " & First'Img & Last'Img));
         end if;
      end if;
   end In_Place_Merge;

   procedure In_Place_Reverse (First, Last : Integer;
      Swap : not null access procedure (I, J : Integer))
   is
      I : Integer := First;
      J : Integer := Last;
   begin
      while I < J loop
         Swap (I, J);
         I := I + 1;
         J := J - 1;
      end loop;
   end In_Place_Reverse;

end Ada.Containers.Inside.Array_Sorting;
