with Ada.Unchecked_Conversion;
with System.Storage_Map;
with C.basetsd;
with C.winternl;
package body System.Native_Tasks is
   use type C.char_array;
   use type C.windef.DWORD;
   use type C.windef.WINBOOL;
   use type C.winnt.HANDLE; -- C.void_ptr

   type struct_THREAD_BASIC_INFORMATION is record
      ExitStatus : C.winternl.NTSTATUS;
      TebBaseAddress : C.winnt.struct_TEB_ptr; -- PVOID;
      ClientId : C.winternl.CLIENT_ID;
      AffinityMask : C.basetsd.KAFFINITY;
      Priority : C.winternl.KPRIORITY;
      BasePriority : C.winternl.KPRIORITY;
   end record
      with Convention => C;
   pragma Suppress_Initialization (struct_THREAD_BASIC_INFORMATION);

   type NtQueryInformationThread_Type is access function (
      ThreadHandle : C.winnt.HANDLE;
      ThreadInformationClass : C.winternl.THREADINFOCLASS;
      ThreadInformation : C.winnt.PVOID;
      ThreadInformationLength : C.windef.ULONG;
      ReturnLength : access C.windef.ULONG)
      return C.winternl.NTSTATUS
      with Convention => WINAPI;

   NtQueryInformationThread_Name : constant C.char_array (0 .. 24) :=
      "NtQueryInformationThread" & C.char'Val (0);

   Installed_Abort_Handler : Abort_Handler := null;

   --  implementation of thread

   procedure Create (
      Handle : aliased out Handle_Type;
      Parameter : Parameter_Type;
      Thread_Body : Thread_Body_Type;
      Error : out Boolean)
   is
      Id : aliased C.windef.DWORD;
   begin
      Handle := C.winbase.CreateThread (
         lpThreadAttributes => null,
         dwStackSize => 0,
         lpStartAddress => Thread_Body,
         lpParameter => Parameter,
         dwCreationFlags => 0,
         lpThreadId => Id'Access);
      Error := Handle = C.winbase.INVALID_HANDLE_VALUE;
   end Create;

   procedure Join (
      Handle : Handle_Type;
      Current_Abort_Event : access Synchronous_Objects.Event;
      Result : aliased out Result_Type;
      Error : out Boolean)
   is
      Signaled : C.windef.DWORD;
   begin
      Error := False;
      if Current_Abort_Event /= null then
         declare
            Handles : aliased array (0 .. 1) of aliased C.winnt.HANDLE :=
               (Handle, Synchronous_Objects.Handle (Current_Abort_Event.all));
         begin
            Signaled :=
               C.winbase.WaitForMultipleObjects (
                  2,
                  Handles (0)'Access,
                  0,
                  C.winbase.INFINITE);
            if Signaled /= C.winbase.WAIT_OBJECT_0 + 1 then
               goto Done;
            end if;
            Installed_Abort_Handler.all; -- may abort child tasks
         end;
      end if;
      Signaled := C.winbase.WaitForSingleObject (Handle, C.winbase.INFINITE);
   <<Done>>
      case Signaled is
         when C.winbase.WAIT_OBJECT_0 =>
            if C.winbase.GetExitCodeThread (Handle, Result'Access) =
               C.windef.FALSE
            then
               Error := True;
            end if;
         when others =>
            Error := True;
      end case;
      if C.winbase.CloseHandle (Handle) = C.windef.FALSE then
         Error := True;
      end if;
   end Join;

   procedure Detach (
      Handle : in out Handle_Type;
      Error : out Boolean) is
   begin
      Error := False;
      if C.winbase.CloseHandle (Handle) /= C.windef.FALSE then
         Handle := C.winbase.GetCurrentThread;
         --  magic value meaning current thread
         pragma Assert (
            Handle = C.winnt.HANDLE (System'To_Address (16#fffffffe#)));
      else
         Error := True;
      end if;
   end Detach;

   --  implementation of stack

   function Info_Block (Handle : Handle_Type) return C.winnt.struct_TEB_ptr is
      function To_NtQueryInformationThread_Type is
         new Ada.Unchecked_Conversion (
            C.windef.FARPROC,
            NtQueryInformationThread_Type);
      NtQueryInformationThread : NtQueryInformationThread_Type;
   begin
      NtQueryInformationThread :=
         To_NtQueryInformationThread_Type (
            C.winbase.GetProcAddress (
               Storage_Map.NTDLL,
               NtQueryInformationThread_Name (0)'Access));
      if NtQueryInformationThread = null then
         return null; -- ???
      else
         declare
            TBI : aliased struct_THREAD_BASIC_INFORMATION;
            ReturnLength : aliased C.windef.ULONG;
            Dummy_Status : C.winternl.NTSTATUS;
         begin
            Dummy_Status := NtQueryInformationThread (
               Handle,
               C.winternl.ThreadBasicInformation,
               C.windef.LPVOID (TBI'Address),
               struct_THREAD_BASIC_INFORMATION'Size / Standard'Storage_Unit,
               ReturnLength'Access);
            return TBI.TebBaseAddress;
         end;
      end if;
   end Info_Block;

   --  implementation of signals

   procedure Install_Abort_Handler (Handler : Abort_Handler) is
   begin
      Installed_Abort_Handler := Handler;
   end Install_Abort_Handler;

   procedure Uninstall_Abort_Handler is
   begin
      Installed_Abort_Handler := null;
   end Uninstall_Abort_Handler;

   procedure Send_Abort_Signal (
      Handle : Handle_Type;
      Abort_Event : in out Synchronous_Objects.Event;
      Error : out Boolean)
   is
      pragma Unreferenced (Handle);
   begin
      Synchronous_Objects.Set (Abort_Event);
      Error := False;
   end Send_Abort_Signal;

   procedure Block_Abort_Signal (Abort_Event : Synchronous_Objects.Event) is
   begin
      --  check aborted
      if Synchronous_Objects.Get (Abort_Event) then
         Installed_Abort_Handler.all;
      end if;
   end Block_Abort_Signal;

   procedure Yield is
   begin
      C.winbase.Sleep (0);
   end Yield;

end System.Native_Tasks;
