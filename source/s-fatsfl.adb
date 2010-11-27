package body System.Fat_Sflt is
   pragma Suppress (All_Checks);

   function isfinite (X : Short_Float) return Integer;
   pragma Import (Intrinsic, isfinite, "__builtin_isfinite");

   package body Attr_Short_Float is

      function Valid (X : not null access Short_Float) return Boolean is
      begin
         return isfinite (X.all) /= 0;
      end Valid;

   end Attr_Short_Float;

end System.Fat_Sflt;
