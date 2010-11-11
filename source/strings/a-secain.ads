pragma License (Unrestricted);
function Ada.Strings.Equal_Case_Insensitive (Left, Right : String)
   return Boolean;
--  pragma Pure (Equal_Case_Insensitive);
pragma Preelaborate (Equal_Case_Insensitive); --  use maps
