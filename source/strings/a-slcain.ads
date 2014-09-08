pragma License (Unrestricted);
--  Ada 2012
function Ada.Strings.Less_Case_Insensitive (Left, Right : String)
   return Boolean;
--  pragma Pure (Ada.Strings.Less_Case_Insensitive);
pragma Preelaborate (Ada.Strings.Less_Case_Insensitive); -- use maps
