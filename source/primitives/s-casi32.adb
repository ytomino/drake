package body System.Compare_Array_Signed_32 is
   pragma Suppress (All_Checks);

   function Compare_Array_S32 (
      Left : Address;
      Right : Address;
      Left_Len : Natural;
      Right_Len : Natural)
      return Integer
   is
      type Integer_32 is range -2 ** 31 .. 2 ** 31 - 1;
      L : array (1 .. Left_Len) of Integer_32;
      for L'Address use Left;
      R : array (1 .. Right_Len) of Integer_32;
      for R'Address use Right;
   begin
      for I in 1 .. Integer'Min (Left_Len, Right_Len) loop
         if L (I) < R (I) then
            return -1;
         elsif L (I) > R (I) then
            return 1;
         end if;
      end loop;
      if Left_Len < Right_Len then
         return -1;
      elsif Left_Len > Right_Len then
         return 1;
      else
         return 0;
      end if;
   end Compare_Array_S32;

end System.Compare_Array_Signed_32;
