pragma License (Unrestricted);
--  extended unit
with Ada.Strings.Functions.Maps;
with Ada.Strings.Maps;
package Ada.Strings.Unbounded_Strings.Functions.Maps is new Generic_Maps (
   Character_Set => Strings.Maps.Character_Set,
   Character_Mapping => Strings.Maps.Character_Mapping,
   Fixed_Index_Mapping_From => Strings.Functions.Maps.Index,
   Fixed_Index_Mapping => Strings.Functions.Maps.Index,
   Fixed_Index_Mapping_Function_From => Strings.Functions.Maps.Index,
   Fixed_Index_Mapping_Function => Strings.Functions.Maps.Index,
   Fixed_Index_Mapping_Function_Per_Element_From =>
      Strings.Functions.Maps.Index_Per_Element,
   Fixed_Index_Mapping_Function_Per_Element =>
      Strings.Functions.Maps.Index_Per_Element,
   Fixed_Index_Set_From => Strings.Functions.Maps.Index,
   Fixed_Index_Set => Strings.Functions.Maps.Index,
   Fixed_Count_Mapping => Strings.Functions.Maps.Count,
   Fixed_Count_Mapping_Function => Strings.Functions.Maps.Count,
   Fixed_Count_Mapping_Function_Per_Element =>
      Strings.Functions.Maps.Count_Per_Element,
   Fixed_Count_Set => Strings.Functions.Maps.Count,
   Fixed_Find_Token_From => Strings.Functions.Maps.Find_Token,
   Fixed_Find_Token => Strings.Functions.Maps.Find_Token,
   Fixed_Translate_Mapping => Strings.Functions.Maps.Translate,
   Fixed_Translate_Mapping_Function => Strings.Functions.Maps.Translate,
   Fixed_Translate_Mapping_Function_Per_Element =>
      Strings.Functions.Maps.Translate_Per_Element,
   Fixed_Trim_Set => Strings.Functions.Maps.Trim);
pragma Preelaborate (Ada.Strings.Unbounded_Strings.Functions.Maps);
