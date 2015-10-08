pragma License (Unrestricted);
--  implementation unit
with Ada.UCD.Normalization;
private package Ada.Strings.Canonical_Composites is
   pragma Preelaborate;

   --  decomposition

   Expanding : constant := 4; -- same as Ada.Strings.Normalization.Expanding

   subtype Decomposed_Wide_Wide_String is Wide_Wide_String (1 .. Expanding);

   function Decomposed_Length (Item : Decomposed_Wide_Wide_String)
      return Natural;

   type D_Map_Element is record
      From : Wide_Wide_Character;
      To : Decomposed_Wide_Wide_String;
   end record;
   pragma Suppress_Initialization (D_Map_Element);

   type D_Map_Array is
      array (1 .. UCD.Normalization.NFD_Total) of D_Map_Element;
   pragma Suppress_Initialization (D_Map_Array);
   type D_Map_Array_Access is access D_Map_Array;

   D_Map : D_Map_Array_Access;

   function D_Find (Item : Wide_Wide_Character) return Natural;

   procedure Initialize_D;

   Unexpanded_D_Map : D_Map_Array_Access;

   procedure Initialize_Unexpanded_D;

   --  composition

   subtype Composing_Wide_Wide_String is Wide_Wide_String (1 .. 2);

   type C_Map_Element is record
      From : Composing_Wide_Wide_String;
      To : Wide_Wide_Character;
   end record;
   pragma Suppress_Initialization (C_Map_Element);

   type C_Map_Array is
      array (1 .. UCD.Normalization.NFC_Total) of C_Map_Element;
   pragma Suppress_Initialization (C_Map_Array);
   type C_Map_Array_Access is access C_Map_Array;

   C_Map : C_Map_Array_Access;

   function C_Find (Item : Composing_Wide_Wide_String) return Natural;

   procedure Initialize_C;

end Ada.Strings.Canonical_Composites;
