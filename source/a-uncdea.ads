pragma License (Unrestricted);
generic
   type Object (<>) is limited private;
   type Name is access Object;
procedure Ada.Unchecked_Deallocation (X : in out Name)
   with Import, Convention => Intrinsic;
pragma Preelaborate (Ada.Unchecked_Deallocation);
