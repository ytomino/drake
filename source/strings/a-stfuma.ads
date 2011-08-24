pragma License (Unrestricted);
--  extended unit
with Ada.Strings.Maps;
with System.UTF_Conversions;
package Ada.Strings.Functions.Maps is new Generic_Maps (
   UTF_Max_Length => System.UTF_Conversions.UTF_8_Max_Length,
   To_UTF => System.UTF_Conversions.To_UTF_8,
   From_UTF => System.UTF_Conversions.From_UTF_8,
   From_UTF_Reverse => System.UTF_Conversions.From_UTF_8_Reverse,
   Character_Set => Strings.Maps.Character_Set,
   Is_In => Strings.Maps.Overloaded_Is_In,
   Character_Mapping => Strings.Maps.Character_Mapping,
   Value => Strings.Maps.Overloaded_Value);
pragma Preelaborate (Ada.Strings.Functions.Maps);
