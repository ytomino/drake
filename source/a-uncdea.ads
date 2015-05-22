pragma License (Unrestricted);
generic
   type Object (<>) is limited private;
   type Name is access Object;
procedure Ada.Unchecked_Deallocation (X : in out Name);
--  with Convention => Intrinsic; -- [gcc-4.9] cannot import it with aspect
pragma Preelaborate (Ada.Unchecked_Deallocation);
pragma Import (Intrinsic, Ada.Unchecked_Deallocation);
