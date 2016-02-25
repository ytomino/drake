package body Ada.Strings.Naked_Maps.Debug is

   --  implementation of sets

   function Valid (Set : Character_Set_Data) return Boolean is
   begin
      for I in Set.Items'First .. Set.Items'Last loop
         if Set.Items (I).High < Set.Items (I).Low then
            return False;
         end if;
      end loop;
      for I in Set.Items'First .. Set.Items'Last - 1 loop
         if Set.Items (I).High >=
            Wide_Wide_Character'Pred (Set.Items (I + 1).Low)
         then
            return False;
         end if;
      end loop;
      return True;
   end Valid;

   --  implementation of maps

   function Valid (Map : Character_Mapping_Data) return Boolean is
   begin
      for I in Map.From'First .. Map.From'Last - 1 loop
         if Map.From (I) >= Map.From (I + 1) then
            return False;
         end if;
      end loop;
      return True;
   end Valid;

end Ada.Strings.Naked_Maps.Debug;
