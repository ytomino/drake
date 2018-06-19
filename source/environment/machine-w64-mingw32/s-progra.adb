with System.Address_To_Named_Access_Conversions;
with System.Growth;
with System.Zero_Terminated_WStrings;
with C.basetsd;
with C.winbase;
with C.windef;
with C.winnt;
package body System.Program is
   use type C.basetsd.SSIZE_T;

   package LPWSTR_Conv is
      new Address_To_Named_Access_Conversions (C.winnt.WCHAR, C.winnt.LPWSTR);

   --  implementation

   function Full_Name return String is
      package Holder is
         new Growth.Scoped_Holder (
            C.basetsd.SSIZE_T,
            Component_Size => C.winnt.WCHAR_array'Component_Size);
   begin
      Holder.Reserve_Capacity (1024);
      loop
         declare
            Result_Length : C.basetsd.SSIZE_T;
         begin
            Result_Length :=
               C.basetsd.SSIZE_T (
                  C.winbase.GetModuleFileName (
                     null,
                     LPWSTR_Conv.To_Pointer (Holder.Storage_Address),
                     C.windef.DWORD (Holder.Capacity)));
            if Result_Length < Holder.Capacity then
               return Zero_Terminated_WStrings.Value (
                  LPWSTR_Conv.To_Pointer (Holder.Storage_Address),
                  C.size_t (Result_Length));
            end if;
         end;
         --  growth
         declare
            function Grow is new Growth.Fast_Grow (C.basetsd.SSIZE_T);
         begin
            Holder.Reserve_Capacity (Grow (Holder.Capacity));
         end;
      end loop;
   end Full_Name;

end System.Program;
