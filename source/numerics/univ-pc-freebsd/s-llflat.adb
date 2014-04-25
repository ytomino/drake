package body System.Long_Long_Float_Attributes is
   pragma Suppress (All_Checks);

   function copysignl (X, Y : Long_Long_Float) return Long_Long_Float;
   pragma Import (Intrinsic, copysignl, "__builtin_copysignl");

   function ldexpl (X : Long_Long_Float; N : Integer) return Long_Long_Float;
   pragma Import (Intrinsic, ldexpl, "__builtin_ldexpl");

   function roundl (X : Long_Long_Float) return Long_Long_Float;
   pragma Import (Intrinsic, roundl, "__builtin_roundl");

   --  implementation

   function Remainder (X, Y : Long_Long_Float) return Long_Long_Float is
      Quotient : constant Long_Long_Float := Unbiased_Rounding (X / Y);
   begin
      return X - Quotient * Y;
   end Remainder;

   function Unbiased_Rounding (X : Long_Long_Float)
      return Long_Long_Float
   is
      Result : Long_Long_Float := roundl (X);
      Diff : constant Long_Long_Float := Result - X;
   begin
      if abs Diff = 0.5 then
         --  1.5 -> 2.0, 2.5 -> 3.0, ...
         --  -1.5 -> -2.0, -2.5 -> -3.0, ...
         declare
            Half_Result : constant Long_Long_Float := ldexpl (Result, -1);
         begin
            if roundl (Half_Result) /= Half_Result then
               Result := Result - copysignl (1.0, Diff);
            end if;
         end;
      end if;
      return Result;
   end Unbiased_Rounding;

end System.Long_Long_Float_Attributes;
