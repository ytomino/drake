pragma Check_Policy (Validate => Disable);
with Ada.Strings.Naked_Maps.Canonical_Composites;
--  with Ada.Strings.Naked_Maps.Debug;
with Ada.Strings.Naked_Maps.Set_Constants;
with System.Once;
with System.Reference_Counting;
package body Ada.Strings.Naked_Maps.Basic is

   --  Basic_Set

   type Character_Set_Access_With_Pool is access Character_Set_Data;

   Basic_Set_Data : Character_Set_Access_With_Pool;
   Basic_Set_Flag : aliased System.Once.Flag := 0;

   procedure Basic_Set_Init;
   procedure Basic_Set_Init is
      Letter_Set : constant not null Character_Set_Access :=
         Set_Constants.Letter_Set;
      Base_Set : constant not null Character_Set_Access :=
         Canonical_Composites.Base_Set;
      Ranges : Character_Ranges (1 .. Letter_Set.Length + Base_Set.Length);
      Ranges_Last : Natural;
   begin
      Intersection (Ranges, Ranges_Last, Letter_Set.Items, Base_Set.Items);
      Basic_Set_Data := new Character_Set_Data'(
         Length => Ranges_Last,
         Reference_Count => System.Reference_Counting.Static,
         Items => Ranges (1 .. Ranges_Last));
      pragma Check (Validate, Debug.Valid (Basic_Set_Data.all));
   end Basic_Set_Init;

   --  implementation of Basic_Set

   function Basic_Set return not null Character_Set_Access is
   begin
      System.Once.Initialize (Basic_Set_Flag'Access, Basic_Set_Init'Access);
      return Character_Set_Access (Basic_Set_Data);
   end Basic_Set;

end Ada.Strings.Naked_Maps.Basic;
