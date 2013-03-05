pragma License (Unrestricted);
--  implementation unit
with C.pthread;
package System.Tasking.Native_Tasks is
   pragma Preelaborate;

   --  thread

   subtype Handle_Type is C.pthread.pthread_t;

   function Current return Handle_Type
      renames C.pthread.pthread_self;

   subtype Parameter_Type is C.void_ptr;
   subtype Result_Type is C.void_ptr;

   type Thread_Body_Type is access
      function (Parameter : Parameter_Type) return Result_Type;
   pragma Convention (C, Thread_Body_Type);

   procedure Create (
      Handle : not null access Handle_Type;
      Parameter : Parameter_Type;
      Thread_Body : Thread_Body_Type;
      Error : out Boolean);

   procedure Join (
      Handle : Handle_Type;
      Result : not null access Result_Type;
      Error : out Boolean);
   procedure Detach (
      Handle : Handle_Type;
      Error : out Boolean);

   --  signals

   type Abort_Handler is access procedure;

   procedure Install_Abort_Handler (Handler : Abort_Handler);
   procedure Uninstall_Abort_Handler;

   procedure Send_Abort_Signal (Handle : Handle_Type; Error : out Boolean);

   procedure Block_Abort_Signal;
   procedure Unblock_Abort_Signal;

end System.Tasking.Native_Tasks;
