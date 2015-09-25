with Ada.Strings.Canonical_Composites;
with System.Once;
with System.Reference_Counting;
package body Ada.Strings.Naked_Maps.Canonical_Composites is

   --  Base_Set

   type Character_Set_Access is access Character_Set;

   Base_Set_Data : Character_Set_Access;
   Base_Set_Flag : aliased System.Once.Flag := 0;

   procedure Base_Set_Init;
   procedure Base_Set_Init is
   begin
      Strings.Canonical_Composites.Initialize_D;
      declare
         Ranges : Character_Ranges (
            1 .. Strings.Canonical_Composites.D_Map_Array'Length + 1);
         Ranges_Last : Natural := 1;
         Next : Character_Type;
      begin
         Ranges (1).Low := Character_Type'Val (0);
         Ranges (1).High := Character_Type'Pred (
            Strings.Canonical_Composites.D_Map (1).From);
         Next := Character_Type'Succ (
            Strings.Canonical_Composites.D_Map (1).From);
         for I in 2 .. Strings.Canonical_Composites.D_Map_Array'Last loop
            declare
               Item : constant Character_Type :=
                  Strings.Canonical_Composites.D_Map (I).From;
            begin
               if Item > Next then
                  Ranges_Last := Ranges_Last + 1;
                  Ranges (Ranges_Last).Low := Next;
                  Ranges (Ranges_Last).High := Character_Type'Pred (Item);
               end if;
               Next := Character_Type'Succ (Item);
            end;
         end loop;
         Base_Set_Data := new Character_Set'(
            Length => Ranges_Last,
            Reference_Count => System.Reference_Counting.Static,
            Items => Ranges (1 .. Ranges_Last));
      end;
   end Base_Set_Init;

   --  implementation of Base_Set

   function Base_Set return not null access Character_Set is
   begin
      System.Once.Initialize (Base_Set_Flag'Access, Base_Set_Init'Access);
      return Base_Set_Data;
   end Base_Set;

   --  Base_Map

   type Character_Mapping_Access is access Character_Mapping;

   Base_Mapping : Character_Mapping_Access;
   Base_Mapping_Flag : aliased System.Once.Flag := 0;

   procedure Base_Mapping_Init;
   procedure Base_Mapping_Init is
   begin
      Strings.Canonical_Composites.Initialize_D;
      Base_Mapping := new Character_Mapping'(
         Length => Strings.Canonical_Composites.D_Map_Array'Length,
         Reference_Count => System.Reference_Counting.Static,
         From => <>,
         To => <>);
      for I in Strings.Canonical_Composites.D_Map_Array'Range loop
         declare
            Item : Strings.Canonical_Composites.D_Map_Element
               renames Strings.Canonical_Composites.D_Map (I);
         begin
            Base_Mapping.From (I) := Item.From;
            Base_Mapping.To (I) := Item.To (1);
         end;
      end loop;
   end Base_Mapping_Init;

   --  implementation of Base_Map

   function Base_Map return not null access Character_Mapping is
   begin
      System.Once.Initialize (
         Base_Mapping_Flag'Access,
         Base_Mapping_Init'Access);
      return Base_Mapping;
   end Base_Map;

end Ada.Strings.Naked_Maps.Canonical_Composites;
