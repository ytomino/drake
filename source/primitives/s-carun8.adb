with System.Storage_Elements;
package body System.Compare_Array_Unsigned_8 is
   pragma Suppress (All_Checks);

   function memcmp (
      s1 : Address;
      s2 : Address;
      n : Storage_Elements.Storage_Count)
      return Integer;
   pragma Import (Intrinsic, memcmp, "__builtin_memcmp");

   --  implementation

   function Compare_Array_U8_Unaligned (
      Left : Address;
      Right : Address;
      Left_Len : Natural;
      Right_Len : Natural)
      return Integer
   is
      Result : constant Integer := memcmp (
         Left,
         Right,
         Storage_Elements.Storage_Count (Integer'Min (Left_Len, Right_Len)));
   begin
      if Result = 0 then
         if Left_Len < Right_Len then
            return -1;
         elsif Left_Len > Right_Len then
            return 1;
         else
            return 0;
         end if;
      else
         return Result;
      end if;
   end Compare_Array_U8_Unaligned;

end System.Compare_Array_Unsigned_8;
