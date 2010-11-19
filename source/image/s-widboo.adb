package body System.Wid_Bool is
   pragma Suppress (All_Checks);

   function Width_Boolean (Lo, Hi : Boolean) return Natural is
   begin
      if Lo > Hi then
         return 0;
      elsif Lo then
         return 4; --  "TRUE"
      else
         return 5; --  "FALSE"
      end if;
   end Width_Boolean;

end System.Wid_Bool;
