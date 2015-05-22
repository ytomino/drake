pragma License (Unrestricted);
generic
   type Source (<>) is limited private;
   type Target (<>) is limited private;
function Ada.Unchecked_Conversion (S : Source) return Target
   with Import, Convention => Intrinsic;
pragma Pure (Ada.Unchecked_Conversion);
