with Ada.Unchecked_Conversion;
with C.winbase;
with C.winnt;
with C.winternl;
package body System.Storage_Map is
   pragma Suppress (All_Checks);
   use type C.char_array;
   use type C.size_t;
   use type C.windef.ULONG;

   A_NTDLL_DLL : aliased constant Wide_String (1 .. 10) :=
      "NTDLL.DLL" & Wide_Character'Val (0);
   C_NTDLL_DLL : aliased C.wchar_t_array (0 .. A_NTDLL_DLL'Length - 1);
   for C_NTDLL_DLL'Address use A_NTDLL_DLL'Address;

   type NtQueryInformationProcess_Type is access function (
      ProcessHandle : C.winnt.HANDLE;
      ProcessInformationClass : C.winternl.PROCESSINFOCLASS;
      ProcessInformation : C.winnt.PVOID;
      ProcessInformationLength : C.windef.ULONG;
      ReturnLength : access C.windef.ULONG)
      return C.winternl.NTSTATUS
      with Convention => WINAPI;

   NtQueryInformationProcess_Name : constant C.char_array (0 .. 25) :=
      "NtQueryInformationProcess" & C.char'Val (0);

   --  implementation

   function Load_Address return Address is
      function To_NtQueryInformationProcess_Type is
         new Ada.Unchecked_Conversion (
            C.windef.FARPROC,
            NtQueryInformationProcess_Type);
      NtQueryInformationProcess : NtQueryInformationProcess_Type;
   begin
      NtQueryInformationProcess :=
         To_NtQueryInformationProcess_Type (
            C.winbase.GetProcAddress (
               NTDLL,
               NtQueryInformationProcess_Name (0)'Access));
      if NtQueryInformationProcess = null then
         return Null_Address; -- ???
      else
         declare
            PBI : aliased C.winternl.PROCESS_BASIC_INFORMATION;
            ReturnLength : aliased C.windef.ULONG;
            Status : C.winternl.NTSTATUS;
            pragma Unreferenced (Status);
            PEB : C.winternl.PPEB;
         begin
            Status := NtQueryInformationProcess (
               C.winbase.GetCurrentProcess,
               C.winternl.ProcessBasicInformation,
               C.windef.LPVOID (PBI'Address),
               C.winternl.PROCESS_BASIC_INFORMATION'Size
                  / Standard'Storage_Unit,
               ReturnLength'Access);
            PEB := PBI.PebBaseAddress; -- process environment block
            return Address (PEB.Reserved3 (1));
         end;
      end if;
   end Load_Address;

   function NTDLL return C.windef.HMODULE is
   begin
      return C.winbase.GetModuleHandle (C_NTDLL_DLL (0)'Access);
   end NTDLL;

end System.Storage_Map;
