pragma License (Unrestricted);
--  extended unit
with Ada.Characters.Conversions;
with Ada.Strings.Wide_Maps;
package Ada.Strings.Wide_Functions.Maps is new Generic_Maps (
   Expanding => 2, -- System.UTF_Conversions.UTF_16_Max_Length
   Put => Characters.Conversions.Put,
   Get => Characters.Conversions.Get,
   Get_Reverse => Characters.Conversions.Get_Reverse,
   Character_Set => Wide_Maps.Wide_Character_Set,
   Is_In => Wide_Maps.Overloaded_Is_In,
   Character_Mapping => Wide_Maps.Wide_Character_Mapping,
   Value => Wide_Maps.Overloaded_Value);
pragma Preelaborate (Ada.Strings.Wide_Functions.Maps);
