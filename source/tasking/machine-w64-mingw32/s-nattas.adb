with System.Debug;
with C.winerror;
package body System.Native_Tasks is
   use type C.void_ptr; -- C.void_ptr
   use type C.windef.DWORD;
   use type C.windef.WINBOOL;

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
      Abort_Current : access Task_Attribute_Of_Abort;
      Result : aliased out Result_Type;
      Error : out Boolean)
   is
      R : C.windef.DWORD;
   begin
      Error := False;
      if Abort_Current /= null and then not Abort_Current.Blocked then
         declare
            Handles : aliased array (0 .. 1) of aliased C.winnt.HANDLE :=
               (Handle, Abort_Current.Event);
         begin
            R := C.winbase.WaitForMultipleObjects (
               2,
               Handles (0)'Access,
               0,
               C.winbase.INFINITE);
            if R /= C.winbase.WAIT_OBJECT_0 + 1 then
               goto Done;
            end if;
            Installed_Abort_Handler.all; -- may abort child tasks
         end;
      end if;
      R := C.winbase.WaitForSingleObject (Handle, C.winbase.INFINITE);
   <<Done>>
      case R is
         when C.winbase.WAIT_OBJECT_0 =>
            if C.winbase.GetExitCodeThread (Handle, Result'Access) = 0 then
               Error := True;
            end if;
         when others =>
            Error := True;
      end case;
      if C.winbase.CloseHandle (Handle) = 0 then
         Error := True;
      end if;
   end Join;

   procedure Detach (
      Handle : in out Handle_Type;
      Error : out Boolean) is
   begin
      Error := False;
      if C.winbase.CloseHandle (Handle) /= 0 then
         Handle := C.winbase.GetCurrentThread;
         --  magic value meaning current thread
         pragma Assert (
            Handle = C.winnt.HANDLE (System'To_Address (16#fffffffe#)));
      else
         Error := True;
      end if;
   end Detach;

   --  implementation of stack

   procedure Initialize (Attr : in out Task_Attribute_Of_Stack) is
   begin
      Attr := C.winnt.NtCurrentTeb;
   end Initialize;

   function Info_Block (
      Handle : Handle_Type;
      Attr : Task_Attribute_Of_Stack)
      return Info_Block_Type
   is
      pragma Unreferenced (Handle);
   begin
      return Attr;
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

   procedure Initialize (Attr : in out Task_Attribute_Of_Abort) is
   begin
      Attr.Event := C.winbase.CreateEvent (null, 1, 0, null); -- manual
      Attr.Blocked := False;
   end Initialize;

   procedure Finalize (Attr : in out Task_Attribute_Of_Abort) is
      Closing_Handle : constant Handle_Type := Attr.Event;
   begin
      Attr.Event := C.winbase.INVALID_HANDLE_VALUE;
      declare
         R : C.windef.WINBOOL;
      begin
         R := C.winbase.CloseHandle (Closing_Handle);
         pragma Check (Debug,
            Check =>
               R /= 0 or else Debug.Runtime_Error ("CloseHandle failed"));
      end;
   end Finalize;

   procedure Send_Abort_Signal (
      Handle : Handle_Type;
      Attr : Task_Attribute_Of_Abort;
      Error : out Boolean)
   is
      pragma Unreferenced (Handle);
   begin
      Error := False;
      if C.winbase.SetEvent (Attr.Event) = 0 then
         Error := C.winbase.GetLastError /=
            C.winerror.ERROR_INVALID_HANDLE; -- already terminated
      end if;
   end Send_Abort_Signal;

   procedure Block_Abort_Signal (Attr : in out Task_Attribute_Of_Abort) is
   begin
      Attr.Blocked := True;
      --  check aborted
      if C.winbase.WaitForSingleObject (Attr.Event, 0) =
         C.winbase.WAIT_OBJECT_0
      then
         Installed_Abort_Handler.all;
      end if;
   end Block_Abort_Signal;

   procedure Unblock_Abort_Signal (Attr : in out Task_Attribute_Of_Abort) is
   begin
      Attr.Blocked := False;
   end Unblock_Abort_Signal;

end System.Native_Tasks;
