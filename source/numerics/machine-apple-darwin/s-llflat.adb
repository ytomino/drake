package body System.Long_Long_Float_Attributes is
   pragma Suppress (All_Checks);

   --  implementation

   function Unbiased_Rounding (X : Long_Long_Float)
      return Long_Long_Float is
   begin
      return X - Remainder (X, 1.0);
   end Unbiased_Rounding;

end System.Long_Long_Float_Attributes;
