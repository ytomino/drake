with System.Storage_Elements;
package body System.Compare_Array_Unsigned_8 is
   pragma Suppress (All_Checks);

   function memcmp (
      s1 : Address;
      s2 : Address;
      n : Storage_Elements.Storage_Count)
      return Integer;
   pragma Import (Intrinsic, memcmp, "__builtin_memcmp");

   function Compare_Array_U8_Unaligned (
      Left : Address;
      Right : Address;
      Left_Len : Natural;
      Right_Len : Natural)
      return Integer is
   begin
      if Left_Len < Right_Len then
         declare
            Result : constant Integer := memcmp (
               Left,
               Right,
               Storage_Elements.Storage_Count (Left_Len));
         begin
            if Result = 0 then
               return -1;
            else
               return Result;
            end if;
         end;
      elsif Left_Len > Right_Len then
         declare
            Result : constant Integer := memcmp (
               Left,
               Right,
               Storage_Elements.Storage_Count (Right_Len));
         begin
            if Result = 0 then
               return 1;
            else
               return Result;
            end if;
         end;
      else
         return memcmp (
            Left,
            Right,
            Storage_Elements.Storage_Count (Left_Len));
      end if;
   end Compare_Array_U8_Unaligned;

end System.Compare_Array_Unsigned_8;
