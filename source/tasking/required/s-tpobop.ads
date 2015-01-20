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
      Uninterpreted_Data : Address;
      Mode : Call_Modes;
      Block : out Communication_Block);

   --  required for synchronized interface by compiler
   procedure Timed_Protected_Entry_Call (
      Object : not null access Entries.Protection_Entries'Class;
      E : Protected_Entry_Index;
      Uninterpreted_Data : Address;
      Timeout : Duration;
      Mode : Integer; -- Tasking.Delay_Modes;
      Entry_Call_Successful : out Boolean);

   --  required for select else by compiler
   function Enqueued (Block : Communication_Block) return Boolean;

   --  required for synchronized interface by compiler
   function Cancelled (Block : Communication_Block) return Boolean;

   --  required for select then abort by compiler
   procedure Cancel_Protected_Entry_Call (
      Block : in out Communication_Block);

   --  required by compiler
   procedure Requeue_Protected_Entry (
      Object : not null access Entries.Protection_Entries'Class;
      New_Object : not null access Entries.Protection_Entries'Class;
      E : Protected_Entry_Index;
      With_Abort : Boolean);

   --  required for synchronized interface by compiler
   procedure Requeue_Task_To_Protected_Entry (
      New_Object : not null access Entries.Protection_Entries'Class;
      E : Protected_Entry_Index;
      With_Abort : Boolean);

   --  required for 'Caller by compiler
   function Protected_Entry_Caller (
      Object : Entries.Protection_Entries'Class)
      return Task_Id;

   --  required for 'Count by compiler
   function Protected_Count (
      Object : Entries.Protection_Entries'Class;
      E : Protected_Entry_Index)
      return Natural;

end System.Tasking.Protected_Objects.Operations;
