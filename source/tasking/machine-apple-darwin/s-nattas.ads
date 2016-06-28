pragma License (Unrestricted);
--  implementation unit specialized for POSIX (Darwin, FreeBSD, or Linux)
with System.Synchronous_Objects;
with C.pthread;
package System.Native_Tasks is
   pragma Preelaborate;

   --  thread

   subtype Handle_Type is C.pthread.pthread_t;

   function Current return Handle_Type
      renames C.pthread.pthread_self;

   subtype Parameter_Type is C.void_ptr;
   subtype Result_Type is C.void_ptr;

   type Thread_Body_Type is access
      function (Parameter : Parameter_Type) return Result_Type
      with Convention => C;
   pragma Convention_Identifier (Thread_Body_CC, C);

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
      Handle : Handle_Type;
      Error : out Boolean);

   --  stack

   function Info_Block (Handle : Handle_Type) return C.pthread.pthread_t;
   pragma Inline (Info_Block);

   --  signals

   type Abort_Handler is access procedure;

   procedure Install_Abort_Handler (Handler : Abort_Handler);
   procedure Uninstall_Abort_Handler;

   procedure Send_Abort_Signal (
      Handle : Handle_Type;
      Abort_Event : in out Synchronous_Objects.Event;
      Error : out Boolean);

   procedure Block_Abort_Signal (Abort_Event : Synchronous_Objects.Event);
   procedure Unblock_Abort_Signal;

end System.Native_Tasks;
