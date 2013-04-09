pragma License (Unrestricted);
--  implementation unit required by compiler
with Ada.Exceptions;
with System.Tasking.Protected_Objects.Entries;
package System.Tasking.Protected_Objects.Operations is

   --  required by compiler
   type Communication_Block is null record;
   pragma Suppress_Initialization (Communication_Block);

   --  required by compiler
   --  protected subprograms are sandwiched between
   --    Lock_Entries and Service_Entries
   procedure Service_Entries (
      Object : not null access Entries.Protection_Entries'Class);

   --  required by compiler
   procedure Complete_Entry_Body (
      Object : not null access Entries.Protection_Entries'Class);

   --  required by compiler
   procedure Exceptional_Complete_Entry_Body (
      Object : not null access Entries.Protection_Entries'Class;
      Id : Ada.Exceptions.Exception_Id);

   --  required by compiler
   procedure Protected_Entry_Call (
      Object : not null access Entries.Protection_Entries'Class;
      E : Protected_Entry_Index;
      Uninterpreted_Data : System.Address;
      Mode : Call_Modes;
      Block : out Communication_Block);

   --  required by compiler
   procedure Requeue_Protected_Entry (
      Object : not null access Entries.Protection_Entries'Class;
      New_Object : not null access Entries.Protection_Entries'Class;
      E : Protected_Entry_Index;
      With_Abort : Boolean);

   --  unimplemented subprograms required by compiler
   --  Cancel_Protected_Entry_Call
   --  Enqueued
   --  Cancelled
   --  Requeue_Task_To_Protected_Entry
   --  Protected_Count
   --  Protected_Entry_Caller
   --  Timed_Protected_Entry_Call

end System.Tasking.Protected_Objects.Operations;
