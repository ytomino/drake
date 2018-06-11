with System.Debug; -- assertions
with C.errno;
with C.sched;
with C.signal;
package body System.Native_Tasks is
   use type C.signed_int;
   use type C.unsigned_int;

   type sigaction_Wrapper is record -- ??? for No_Elaboration_Code
      Handle : aliased C.signal.struct_sigaction;
   end record;
   pragma Suppress_Initialization (sigaction_Wrapper);

   Old_SIGTERM_Action : aliased sigaction_Wrapper; -- uninitialized
   Installed_Abort_Handler : Abort_Handler;

   procedure SIGTERM_Handler (
      Signal_Number : C.signed_int;
      Info : access C.signal.siginfo_t;
      Context : C.void_ptr)
      with Convention => C;

   procedure SIGTERM_Handler (
      Signal_Number : C.signed_int;
      Info : access C.signal.siginfo_t;
      Context : C.void_ptr)
   is
      pragma Unreferenced (Signal_Number);
      pragma Unreferenced (Info);
      pragma Unreferenced (Context);
   begin
      Installed_Abort_Handler.all;
   end SIGTERM_Handler;

   procedure Mask_SIGTERM (How : C.signed_int);
   procedure Mask_SIGTERM (How : C.signed_int) is
      Mask : aliased C.signal.sigset_t;
      errno : C.signed_int;
      Dummy_R : C.signed_int;
   begin
      Dummy_R := C.signal.sigemptyset (Mask'Access);
      Dummy_R := C.signal.sigaddset (Mask'Access, C.signal.SIGTERM);
      errno := C.pthread.pthread_sigmask (How, Mask'Access, null);
      pragma Check (Debug,
         Check =>
            errno = 0 or else Debug.Runtime_Error ("pthread_sigmask failed"));
   end Mask_SIGTERM;

   --  implementation of thread

   procedure Create (
      Handle : aliased out Handle_Type;
      Parameter : Parameter_Type;
      Thread_Body : Thread_Body_Type;
      Error : out Boolean) is
   begin
      Error := C.pthread.pthread_create (
         Handle'Access,
         null,
         Thread_Body.all'Access, -- type is different between platforms
         Parameter) /= 0;
   end Create;

   procedure Join (
      Handle : Handle_Type; -- of target thread
      Current_Abort_Event : access Synchronous_Objects.Event;
      Result : aliased out Result_Type;
      Error : out Boolean)
   is
      pragma Unreferenced (Current_Abort_Event);
   begin
      Error := C.pthread.pthread_join (Handle, Result'Access) /= 0;
   end Join;

   procedure Detach (
      Handle : Handle_Type;
      Error : out Boolean) is
   begin
      Error := C.pthread.pthread_detach (Handle) /= 0;
   end Detach;

   --  implementation of stack

   function Info_Block (Handle : Handle_Type) return C.pthread.pthread_t is
   begin
      return Handle;
   end Info_Block;

   --  implementation of signals

   procedure Install_Abort_Handler (Handler : Abort_Handler) is
      act : aliased C.signal.struct_sigaction := (
         (Unchecked_Tag => 1, sa_sigaction => SIGTERM_Handler'Access),
         others => <>); -- uninitialized
      R : C.signed_int;
      Dummy_R : C.signed_int;
   begin
      Installed_Abort_Handler := Handler;
      act.sa_flags := C.signal.SA_SIGINFO;
      Dummy_R := C.signal.sigemptyset (act.sa_mask'Access);
      R := C.signal.sigaction (
         C.signal.SIGTERM,
         act'Access,
         Old_SIGTERM_Action.Handle'Access);
      pragma Check (Debug,
         Check =>
            not (R < 0) or else Debug.Runtime_Error ("sigaction failed"));
   end Install_Abort_Handler;

   procedure Uninstall_Abort_Handler is
      R : C.signed_int;
   begin
      R := C.signal.sigaction (
         C.signal.SIGTERM,
         Old_SIGTERM_Action.Handle'Access,
         null);
      pragma Check (Debug,
         Check =>
            not (R < 0) or else Debug.Runtime_Error ("sigaction failed"));
   end Uninstall_Abort_Handler;

   procedure Send_Abort_Signal (
      Handle : Handle_Type;
      Abort_Event : in out Synchronous_Objects.Event;
      Error : out Boolean) is
   begin
      --  write to the pipe
      Synchronous_Objects.Set (Abort_Event);
      --  send SIGTERM
      Resend_Abort_Signal (Handle, Error => Error);
   end Send_Abort_Signal;

   procedure Resend_Abort_Signal (Handle : Handle_Type; Error : out Boolean) is
   begin
      case C.pthread.pthread_kill (Handle, C.signal.SIGTERM) is
         when 0 =>
            Yield;
            Error := False;
         when C.errno.ESRCH =>
            Error := False; -- it is already terminated, C9A003A
         when others =>
            Error := True;
      end case;
   end Resend_Abort_Signal;

   procedure Block_Abort_Signal (Abort_Event : Synchronous_Objects.Event) is
      pragma Unreferenced (Abort_Event);
   begin
      Mask_SIGTERM (C.signal.SIG_BLOCK);
   end Block_Abort_Signal;

   procedure Unblock_Abort_Signal is
   begin
      Mask_SIGTERM (C.signal.SIG_UNBLOCK);
   end Unblock_Abort_Signal;

   procedure Yield is
      R : C.signed_int;
   begin
      R := C.sched.sched_yield;
      pragma Check (Debug,
         Check =>
            not (R < 0) or else Debug.Runtime_Error ("sched_yield failed"));
   end Yield;

end System.Native_Tasks;
