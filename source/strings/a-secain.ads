pragma License (Unrestricted);
--  Ada 2012
function Ada.Strings.Equal_Case_Insensitive (Left, Right : String)
   return Boolean;
--  pragma Pure (Ada.Strings.Equal_Case_Insensitive);
pragma Preelaborate (Ada.Strings.Equal_Case_Insensitive); -- use maps
