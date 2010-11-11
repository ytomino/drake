pragma License (Unrestricted);
function Ada.Strings.Less_Case_Insensitive (Left, Right : String)
   return Boolean;
--  pragma Pure (Less_Case_Insensitive);
pragma Preelaborate (Less_Case_Insensitive); --  use maps
