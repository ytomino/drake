pragma License (Unrestricted);
--  extended package
with Ada.Strings.Wide_Wide_Maps;
with System.UTF_Conversions;
package Ada.Strings.Wide_Wide_Functions.Maps is new Generic_Maps (
   UTF_Max_Length => 1,
   To_UTF => System.UTF_Conversions.To_UTF_32,
   From_UTF => System.UTF_Conversions.From_UTF_32,
   From_UTF_Reverse => System.UTF_Conversions.From_UTF_32_Reverse,
   Character_Set => Wide_Wide_Maps.Wide_Wide_Character_Set,
   Is_In => Wide_Wide_Maps.Is_In,
   Character_Mapping => Wide_Wide_Maps.Wide_Wide_Character_Mapping,
   Value => Wide_Wide_Maps.Value);
pragma Preelaborate (Ada.Strings.Wide_Wide_Functions.Maps);
