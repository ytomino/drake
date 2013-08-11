pragma License (Unrestricted);
--  extended unit
with Ada.Strings.Wide_Wide_Functions.Maps;
with Ada.Strings.Wide_Wide_Maps;
package Ada.Strings.Bounded_Wide_Wide_Strings.Functions.Maps is
   new Generic_Maps (
      Character_Set => Wide_Wide_Maps.Wide_Wide_Character_Set,
      Character_Mapping => Wide_Wide_Maps.Wide_Wide_Character_Mapping,
      Fixed_Index_Mapping_From => Wide_Wide_Functions.Maps.Index,
      Fixed_Index_Mapping => Wide_Wide_Functions.Maps.Index,
      Fixed_Index_Mapping_Function_From => Wide_Wide_Functions.Maps.Index,
      Fixed_Index_Mapping_Function => Wide_Wide_Functions.Maps.Index,
      Fixed_Index_Mapping_Function_Per_Element_From =>
         Wide_Wide_Functions.Maps.Index_Per_Element,
      Fixed_Index_Mapping_Function_Per_Element =>
         Wide_Wide_Functions.Maps.Index_Per_Element,
      Fixed_Index_Set_From => Wide_Wide_Functions.Maps.Index,
      Fixed_Index_Set => Wide_Wide_Functions.Maps.Index,
      Fixed_Count_Mapping => Wide_Wide_Functions.Maps.Count,
      Fixed_Count_Mapping_Function => Wide_Wide_Functions.Maps.Count,
      Fixed_Count_Mapping_Function_Per_Element =>
         Wide_Wide_Functions.Maps.Count_Per_Element,
      Fixed_Count_Set => Wide_Wide_Functions.Maps.Count,
      Fixed_Find_Token_From => Wide_Wide_Functions.Maps.Find_Token,
      Fixed_Find_Token => Wide_Wide_Functions.Maps.Find_Token,
      Fixed_Translate_Mapping => Wide_Wide_Functions.Maps.Translate,
      Fixed_Translate_Mapping_Function => Wide_Wide_Functions.Maps.Translate,
      Fixed_Translate_Mapping_Function_Per_Element =>
         Wide_Wide_Functions.Maps.Translate_Per_Element,
      Fixed_Trim_Set => Wide_Wide_Functions.Maps.Trim);
pragma Preelaborate (Ada.Strings.Bounded_Wide_Wide_Strings.Functions.Maps);
