pragma License (Unrestricted);
--  implementation unit specialized for Windows
with System.Synchronous_Objects;
with C.winbase;
with C.windef;
with C.winnt;
package System.Native_Tasks is
   pragma Preelaborate;

   --  thread

   subtype Handle_Type is C.winnt.HANDLE;

   function Current return Handle_Type
      renames C.winbase.GetCurrentThread;

   subtype Parameter_Type is C.windef.LPVOID;
   subtype Result_Type is C.windef.DWORD;

   subtype Thread_Body_Type is C.winbase.PTHREAD_START_ROUTINE;
   pragma Convention_Identifier (Thread_Body_CC, WINAPI);
      --  WINAPI is stdcall convention on 32bit, or C convention on 64bit.

   procedure Create (
      Handle : aliased out Handle_Type;
      Parameter : Parameter_Type;
      Thread_Body : Thread_Body_Type;
      Error : out Boolean);

   procedure Join (
      Handle : Handle_Type; -- of target thread
      Current_Abort_Event : access Synchronous_Objects.Event;
      Result : aliased out Result_Type;
      Error : out Boolean);
   procedure Detach (
      Handle : in out Handle_Type;
      Error : out Boolean);

   --  stack

   function Info_Block (Handle : Handle_Type) return C.winnt.struct_TEB_ptr;

   --  signals

   type Abort_Handler is access procedure;
   pragma Favor_Top_Level (Abort_Handler);

   procedure Install_Abort_Handler (Handler : Abort_Handler);
   procedure Uninstall_Abort_Handler;

   pragma Inline (Install_Abort_Handler);
   pragma Inline (Uninstall_Abort_Handler);

   procedure Send_Abort_Signal (
      Handle : Handle_Type;
      Abort_Event : in out Synchronous_Objects.Event;
      Error : out Boolean);
   procedure Resend_Abort_Signal (Handle : Handle_Type; Error : out Boolean) is
      null;

   pragma Inline (Resend_Abort_Signal);
      --  [gcc-7] can not skip calling null procedure

   procedure Block_Abort_Signal (Abort_Event : Synchronous_Objects.Event);
   procedure Unblock_Abort_Signal is null;

   pragma Inline (Unblock_Abort_Signal);
      --  [gcc-7] can not skip calling null procedure

   --  scheduling

   procedure Yield;

end System.Native_Tasks;
