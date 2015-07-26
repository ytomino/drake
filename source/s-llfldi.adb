package body System.Long_Long_Float_Divisions is
   pragma Suppress (All_Checks);

   function truncl (X : Long_Long_Float) return Long_Long_Float
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_truncl";

   function fmodl (x, y : Long_Long_Float) return Long_Long_Float
      with Import, Convention => Intrinsic, External_Name => "__builtin_fmodl";

   --  implementation

   procedure Divide (
      Left, Right : Long_Long_Float;
      Quotient, Remainder : out Long_Long_Float) is
   begin
      Quotient := truncl (Left / Right);
      Remainder := fmodl (Left, Right);
   end Divide;

end System.Long_Long_Float_Divisions;
