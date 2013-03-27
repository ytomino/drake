with System.Storage_Elements;
package body System.Generic_Compare_Arrays is
   pragma Suppress (All_Checks);

   function memcmp (
      s1 : Address;
      s2 : Address;
      n : Storage_Elements.Storage_Count)
      return Integer;
   pragma Import (Intrinsic, memcmp, "__builtin_memcmp");

   --  implementation

   function Compare (
      Left : Address;
      Right : Address;
      Left_Len : Natural;
      Right_Len : Natural)
      return Integer is
   begin
      if Element_Type'Size = Standard'Storage_Unit
         and then Element_Type'Enum_Rep (Element_Type'First) = 0
      then
         declare
            Result : constant Integer := memcmp (
               Left,
               Right,
               Storage_Elements.Storage_Count (
                  Integer'Min (Left_Len, Right_Len)));
         begin
            if Result /= 0 then
               return Result;
            end if;
         end;
      else
         declare
            L : array (1 .. Left_Len) of Element_Type;
            for L'Address use Left;
            R : array (1 .. Right_Len) of Element_Type;
            for R'Address use Right;
         begin
            for I in 1 .. Integer'Min (Left_Len, Right_Len) loop
               if L (I) < R (I) then
                  return -1;
               elsif L (I) > R (I) then
                  return 1;
               end if;
            end loop;
         end;
      end if;
      if Left_Len < Right_Len then
         return -1;
      elsif Left_Len > Right_Len then
         return 1;
      else
         return 0;
      end if;
   end Compare;

end System.Generic_Compare_Arrays;
