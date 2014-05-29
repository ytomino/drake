with C.winbase;
with C.windef;
with C.winternl;
package body System.Dynamic_Libraries is
   pragma Suppress (All_Checks);
   use type C.windef.ULONG;

   function Load_Address return Address is
      PBI : aliased C.winternl.PROCESS_BASIC_INFORMATION;
      ReturnLength : aliased C.windef.ULONG;
      Status : C.winternl.NTSTATUS;
      pragma Unreferenced (Status);
      PEB : C.winternl.PPEB;
   begin
      Status := C.winternl.NtQueryInformationProcess (
         C.winbase.GetCurrentProcess,
         C.winternl.ProcessBasicInformation,
         C.windef.LPVOID (PBI'Address),
         C.windef.ULONG'(
            C.winternl.PROCESS_BASIC_INFORMATION'Size / Standard'Storage_Unit),
         ReturnLength'Access);
      PEB := PBI.PebBaseAddress; -- process environment block
      return PEB.Reserved3 (1);
   end Load_Address;

end System.Dynamic_Libraries;
