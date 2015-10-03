pragma Check_Policy (Validate => Disable);
with Ada.Strings.Naked_Maps.Canonical_Composites;
--  with Ada.Strings.Naked_Maps.Debug;
with Ada.Strings.Naked_Maps.Set_Constants;
with System.Once;
with System.Reference_Counting;
package body Ada.Strings.Naked_Maps.Basic is

   --  Basic_Set

   type Character_Set_Access is access Character_Set;

   Basic_Set_Data : Character_Set_Access;
   Basic_Set_Flag : aliased System.Once.Flag := 0;

   procedure Basic_Set_Init;
   procedure Basic_Set_Init is
      Letter_Set : constant not null access Character_Set :=
         Set_Constants.Letter_Set;
      Base_Set : constant not null access Character_Set :=
         Canonical_Composites.Base_Set;
      Ranges : Character_Ranges (1 .. Letter_Set.Length + Base_Set.Length);
      Ranges_Last : Natural;
   begin
      Intersection (Ranges, Ranges_Last, Letter_Set.Items, Base_Set.Items);
      Basic_Set_Data := new Character_Set'(
         Length => Ranges_Last,
         Reference_Count => System.Reference_Counting.Static,
         Items => Ranges (1 .. Ranges_Last));
      pragma Check (Validate, Debug.Valid (Basic_Set_Data.all));
   end Basic_Set_Init;

   --  implementation of Basic_Set

   function Basic_Set return not null access Character_Set is
   begin
      System.Once.Initialize (Basic_Set_Flag'Access, Basic_Set_Init'Access);
      return Basic_Set_Data;
   end Basic_Set;

   --  Basic_Map

   type Character_Mapping_Access is access Character_Mapping;

   Basic_Mapping : Character_Mapping_Access;
   Basic_Mapping_Flag : aliased System.Once.Flag := 0;

   procedure Basic_Mapping_Init;
   procedure Basic_Mapping_Init is
      Letter_Set : constant not null access Character_Set :=
         Set_Constants.Letter_Set;
      Base_Map : constant not null access Character_Mapping :=
         Canonical_Composites.Base_Map;
      From : Character_Sequence (Base_Map.From'Range);
      To : Character_Sequence (From'Range);
      Last : Natural := 0;
   begin
      for I in Base_Map.From'Range loop
         if Is_In (Base_Map.From (I), Letter_Set.all) then
            Last := Last + 1;
            From (Last) := Base_Map.From (I);
            To (Last) := Base_Map.To (I);
         end if;
      end loop;
      Basic_Mapping := new Character_Mapping'(
         Length => Last,
         Reference_Count => System.Reference_Counting.Static,
         From => From (1 .. Last),
         To => To (1 .. Last));
      pragma Check (Validate, Debug.Valid (Basic_Mapping.all));
   end Basic_Mapping_Init;

   --  implementation of Basic_Map

   function Basic_Map return not null access Character_Mapping is
   begin
      System.Once.Initialize (
         Basic_Mapping_Flag'Access,
         Basic_Mapping_Init'Access);
      return Basic_Mapping;
   end Basic_Map;

end Ada.Strings.Naked_Maps.Basic;
