pragma License (Unrestricted);
--  with Ada.Unchecked_Deallocation;
generic
   type Object (<>) is limited private;
   type Name is access Object;
procedure Unchecked_Deallocation (X : in out Name);
--  renames Ada.Unchecked_Deallocation;
--  generic_renaming_declaration could not be used... (?)
pragma Preelaborate (Unchecked_Deallocation);
pragma Import (Intrinsic, Unchecked_Deallocation);
