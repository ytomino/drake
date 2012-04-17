package body System.Long_Long_Float_Attributes is
   pragma Suppress (All_Checks);

   function roundl (X : Long_Long_Float) return Long_Long_Float;
   pragma Import (Intrinsic, roundl, "__builtin_roundl");

   function truncl (X : Long_Long_Float) return Long_Long_Float;
   pragma Import (Intrinsic, truncl, "__builtin_truncl");

   --  implementation

   function Unbiased_Rounding (X : Long_Long_Float)
      return Long_Long_Float
   is
      Result : Long_Long_Float := roundl (X);
      Diff : constant Long_Long_Float := Result - X;
   begin
      if Diff = 0.5 then
         --  1.5 -> 2.0, 2.5 -> 3.0, ...
         if truncl (Result / 2.0) * 2.0 /= Result then
            Result := Result - 1.0;
         end if;
      elsif Diff = -0.5 then
         --  -1.5 -> -2.0, -2.5 -> -3.0, ...
         if truncl (Result / 2.0) * 2.0 /= Result then
            Result := Result + 1.0;
         end if;
      end if;
      return Result;
   end Unbiased_Rounding;

end System.Long_Long_Float_Attributes;
