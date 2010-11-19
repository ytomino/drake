package body System.Fat_Sflt is
   pragma Suppress (All_Checks);

   function isnormal (X : Short_Float) return Integer;
   pragma Import (Intrinsic, isnormal, "__builtin_isnormal");

   package body Attr_Short_Float is

      function Valid (X : not null access Short_Float) return Boolean is
      begin
         return isnormal (X.all) /= 0;
      end Valid;

   end Attr_Short_Float;

end System.Fat_Sflt;
