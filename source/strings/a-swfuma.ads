pragma License (Unrestricted);
--  extended package
with Ada.Strings.Wide_Maps;
with System.UTF_Conversions;
package Ada.Strings.Wide_Functions.Maps is new Generic_Maps (
   UTF_Max_Length => System.UTF_Conversions.UTF_16_Max_Length,
   To_UTF => System.UTF_Conversions.To_UTF_16,
   From_UTF => System.UTF_Conversions.From_UTF_16,
   From_UTF_Reverse => System.UTF_Conversions.From_UTF_16_Reverse,
   Character_Set => Wide_Maps.Wide_Character_Set,
   Is_In => Wide_Maps.Overloaded_Is_In,
   Character_Mapping => Wide_Maps.Wide_Character_Mapping,
   Value => Wide_Maps.Overloaded_Value);
pragma Preelaborate (Ada.Strings.Wide_Functions.Maps);
