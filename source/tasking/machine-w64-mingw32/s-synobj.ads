pragma License (Unrestricted);
--  implementation unit
with System.Native_Time;
private with C.winnt;
package System.Synchronous_Objects is
   pragma Preelaborate;

   --  mutex

   type Mutex is limited private;

   procedure Initialize (Object : in out Mutex);
   procedure Finalize (Object : in out Mutex);
   procedure Enter (Object : in out Mutex);
   procedure Leave (Object : in out Mutex);

   --  condition variable (pthread only)

   type Condition_Variable is limited private;

   procedure Initialize (Object : in out Condition_Variable);
   procedure Finalize (Object : in out Condition_Variable);
--  procedure Notify_One (Object : in out Condition_Variable);
   procedure Notify_All (Object : in out Condition_Variable);
   procedure Wait (
      Object : in out Condition_Variable;
      Mutex : in out Synchronous_Objects.Mutex);
--  procedure Wait (
--    Object : in out Condition_Variable;
--    Mutex : in out Synchronous_Objects.Mutex;
--    Timeout : Native_Time.Native_Time;
--    Notified : out Boolean);
--  procedure Wait (
--    Object : in out Condition_Variable;
--    Mutex : in out Synchronous_Objects.Mutex;
--    Timeout : Duration;
--    Notified : out Boolean);

   --  queue

   type Queue_Node is limited private;
   type Queue_Node_Access is access all Queue_Node;
   type Queue_Filter is access function (
      Item : not null Queue_Node_Access;
      Params : Address)
      return Boolean;

   type Queue is limited private;

   procedure Initialize (
      Object : in out Queue;
      Mutex : not null access Synchronous_Objects.Mutex);
   procedure Finalize (
      Object : in out Queue);
   function Count (
      Object : Queue;
      Params : Address;
      Filter : Queue_Filter)
      return Natural;
   function Unsynchronized_Count (
      Object : Queue;
      Params : Address;
      Filter : Queue_Filter)
      return Natural;
   function Canceled (Object : Queue) return Boolean;
   procedure Cancel (
      Object : in out Queue;
      Cancel_Node : access procedure (X : in out Queue_Node_Access));
   procedure Unsynchronized_Prepend (
      Object : in out Queue;
      Item : not null Queue_Node_Access;
      Canceled : out Boolean);
   procedure Add (
      Object : in out Queue;
      Item : not null Queue_Node_Access;
      Canceled : out Boolean);
   procedure Take ( -- no waiting
      Object : in out Queue;
      Item : out Queue_Node_Access;
      Params : Address;
      Filter : Queue_Filter);
   procedure Unsynchronized_Take (
      Object : in out Queue;
      Item : out Queue_Node_Access;
      Params : Address;
      Filter : Queue_Filter);

   --  event for Ada.Synchronous_Task_Control

   type Event is limited private;

   procedure Initialize (Object : in out Event; Manual : Boolean := True);
   procedure Finalize (Object : in out Event);
   function Get (Object : Event) return Boolean;
   procedure Set (Object : in out Event);
   procedure Reset (Object : in out Event);
   procedure Wait (
      Object : in out Event);
   procedure Wait (
      Object : in out Event;
      Timeout : Native_Time.Native_Time;
      Value : out Boolean);
   procedure Wait (
      Object : in out Event;
      Timeout : Duration;
      Value : out Boolean);

   --  group-synchronization for Ada.Synchronous_Barriers

   type Barrier is limited private;

   procedure Initialize (
      Object : in out Barrier;
      Release_Threshold : Natural);
   procedure Finalize (
      Object : in out Barrier);
   procedure Wait (
      Object : in out Barrier;
      Notified : out Boolean);

   --  multi-read/exclusive-write lock for protected

   type RW_Lock is limited private;

   procedure Initialize (Object : in out RW_Lock);
   procedure Finalize (Object : in out RW_Lock);
   procedure Enter_Reading (Object : in out RW_Lock);
   procedure Enter_Writing (Object : in out RW_Lock);
   procedure Leave (Object : in out RW_Lock); -- leave reading or writing

private

   type Counter is range -2**31 .. 2**31 - 1;
   for Counter'Size use 32;
   pragma Atomic (Counter);

   function sync_add_and_fetch (A1 : not null access Counter; A2 : Counter)
      return Counter;
   procedure sync_add_and_fetch (A1 : not null access Counter; A2 : Counter);
   pragma Import (Intrinsic, sync_add_and_fetch, "__sync_add_and_fetch_4");

   function sync_sub_and_fetch (A1 : not null access Counter; A2 : Counter)
      return Counter;
   pragma Import (Intrinsic, sync_sub_and_fetch, "__sync_sub_and_fetch_4");

   function sync_bool_compare_and_swap (
      A1 : not null access Counter;
      A2 : Counter;
      A3 : Counter)
      return Boolean;
   pragma Import (Intrinsic, sync_bool_compare_and_swap,
      "__sync_bool_compare_and_swap_4");

   type Mutex is limited record
      Handle : C.winnt.HANDLE;
   end record;
   pragma Suppress_Initialization (Mutex);

   type Condition_Variable is record
      Event : Synchronous_Objects.Event; -- manual
      Reset_Barrier : Synchronous_Objects.Event; -- manual
      Waiters : aliased Counter;
   end record;
   pragma Suppress_Initialization (Condition_Variable);

   type Queue is limited record
      Mutex : not null access Synchronous_Objects.Mutex;
      Event : Synchronous_Objects.Event; -- auto
      Head : aliased Queue_Node_Access;
      Tail : Queue_Node_Access;
      Params : Address;
      Filter : Queue_Filter;
      Waiting : Boolean;
      Canceled : Boolean;
   end record;
   pragma Suppress_Initialization (Queue);

   type Queue_Node is limited record
      Next : aliased Queue_Node_Access;
   end record;
   pragma Suppress_Initialization (Queue_Node);

   procedure Take_No_Sync (
      Object : in out Queue;
      Item : out Queue_Node_Access;
      Params : Address;
      Filter : Queue_Filter;
      Previous : in out Queue_Node_Access;
      Current : in out Queue_Node_Access);
   --  awake Abortable.Take
   procedure Notify_All (
      Object : in out Queue;
      Item : not null Queue_Node_Access);

   type Event is limited record
      Handle : C.winnt.HANDLE;
   end record;
   pragma Suppress_Initialization (Event);

   type Barrier is limited record
      Event : Synchronous_Objects.Event;
      Release_Threshold : Counter;
      Blocked : aliased Counter;
   end record;
   pragma Suppress_Initialization (Barrier);

   type RW_Lock is limited record
      Reader_Barrier : Event; -- manual
      Writer_Barrier : Event; -- auto
      State : aliased Counter; -- positive => reader, negative => writer
   end record;
   pragma Suppress_Initialization (RW_Lock);

end System.Synchronous_Objects;
