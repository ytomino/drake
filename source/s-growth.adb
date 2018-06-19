--  reference:
--  http://wiki.freepascal.org/User_Changes_3.0.4
--  http://www.kmonos.net/wlog/111.html
package body System.Growth is

   function Fast_Grow (Capacity : Count_Type) return Count_Type is
   begin
      return 2 * Capacity;
   end Fast_Grow;

   function Good_Grow (Capacity : Count_Type) return Count_Type is
      Result : Count_Type;
   begin
      if Capacity >=
         8 * 1024 * 1024 * Standard'Storage_Unit
            / Count_Type'Base (Component_Size) -- 8MB
      then
         if Capacity >=
            128 * 1024 * 1024 * Standard'Storage_Unit
               / Count_Type'Base (Component_Size) -- 128MB
         then
            Result :=
               Capacity
               + 32 * 1024 * 1024 * Standard'Storage_Unit
                  / Count_Type'Base (Component_Size); -- 32MB
         else
            Result := 3 * Capacity / 2; -- 1.5 < golden ratio (1.618...)
         end if;
      else
         Result := 2 * Capacity;
      end if;
      return Result;
   end Good_Grow;

end System.Growth;
