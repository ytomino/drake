pragma License (Unrestricted);
--  implementation unit required by compiler
with System.Tasking.Protected_Objects.Entries;
package System.Tasking.Protected_Objects.Operations is

   --  required by compiler
   procedure Service_Entries (
      Object : not null access Entries.Protection_Entries'Class) is
      null;

   --  unimplemented subprograms required by compiler
   --  Communication_Block
   --  Protected_Entry_Call
   --  Cancel_Protected_Entry_Call
   --  Enqueued
   --  Cancelled
   --  Complete_Entry_Body
   --  Exceptional_Complete_Entry_Body
   --  Requeue_Protected_Entry
   --  Requeue_Task_To_Protected_Entry
   --  Protected_Count
   --  Protected_Entry_Caller
   --  Timed_Protected_Entry_Call

end System.Tasking.Protected_Objects.Operations;
