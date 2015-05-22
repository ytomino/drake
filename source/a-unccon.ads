pragma License (Unrestricted);
generic
   type Source (<>) is limited private;
   type Target (<>) is limited private;
function Ada.Unchecked_Conversion (S : Source) return Target;
--  with Convention => Intrinsic; -- [gcc-4.9] cannot import it with aspect
pragma Pure (Ada.Unchecked_Conversion);
pragma Import (Intrinsic, Ada.Unchecked_Conversion);
