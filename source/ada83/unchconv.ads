pragma License (Unrestricted);
--  with Ada.Unchecked_Conversion;
generic
   type Source (<>) is limited private;
   type Target (<>) is limited private;
function Unchecked_Conversion (S : Source) return Target;
--  renames Ada.Unchecked_Conversion;
--  generic_renaming_declaration could not be used... (?)
pragma Pure (Unchecked_Conversion);
pragma Import (Intrinsic, Unchecked_Conversion);
