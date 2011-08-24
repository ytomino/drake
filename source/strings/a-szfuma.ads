pragma License (Unrestricted);
--  extended unit
with Ada.Characters.Conversions;
with Ada.Strings.Wide_Wide_Maps;
package Ada.Strings.Wide_Wide_Functions.Maps is new Generic_Maps (
   Expanding => 1,
   Put => Characters.Conversions.Put,
   Get => Characters.Conversions.Get,
   Get_Reverse => Characters.Conversions.Get_Reverse,
   Character_Set => Wide_Wide_Maps.Wide_Wide_Character_Set,
   Is_In => Wide_Wide_Maps.Is_In,
   Character_Mapping => Wide_Wide_Maps.Wide_Wide_Character_Mapping,
   Value => Wide_Wide_Maps.Value);
pragma Preelaborate (Ada.Strings.Wide_Wide_Functions.Maps);
