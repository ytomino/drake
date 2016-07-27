pragma License (Unrestricted);
--  implementation unit specialized for POSIX (Darwin, FreeBSD, or Linux)
private with C.pthread;
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
   procedure Notify_All (Object : in out Condition_Variable);
   procedure Wait (
      Object : in out Condition_Variable;
      Mutex : in out Synchronous_Objects.Mutex);

   --  queue

   type Mutex_Access is access all Mutex;
   for Mutex_Access'Storage_Size use 0;

   type Queue_Node is limited private;
   type Queue_Node_Access is access all Queue_Node;
   type Queue_Filter is access function (
      Item : not null Queue_Node_Access;
      Params : Address)
      return Boolean;

   type Queue is limited private;

   procedure Initialize (
      Object : in out Queue;
      Mutex : not null Mutex_Access);
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
   procedure Take (
      Object : in out Queue;
      Item : out Queue_Node_Access;
      Params : Address;
      Filter : Queue_Filter);
      --  no waiting
   procedure Unsynchronized_Take (
      Object : in out Queue;
      Item : out Queue_Node_Access;
      Params : Address;
      Filter : Queue_Filter);

   pragma Inline (Canceled);

   --  event

   type Event is limited private;

   procedure Initialize (Object : in out Event);
   procedure Finalize (Object : in out Event);
   function Get (Object : Event) return Boolean;
   procedure Set (Object : in out Event);
   procedure Reset (Object : in out Event);
   procedure Wait (
      Object : in out Event);
   procedure Wait (
      Object : in out Event;
      Timeout : Duration;
      Value : out Boolean);

   --  multi-read/exclusive-write lock for protected

   type RW_Lock is limited private;

   procedure Initialize (Object : in out RW_Lock);
   procedure Finalize (Object : in out RW_Lock);
   procedure Enter_Reading (Object : in out RW_Lock);
   procedure Enter_Writing (Object : in out RW_Lock);
   procedure Leave (Object : in out RW_Lock); -- leave reading or writing

private

   type Mutex is limited record
      Handle : aliased C.pthread.pthread_mutex_t;
   end record;
   pragma Suppress_Initialization (Mutex);

   type Condition_Variable is limited record
      Handle : aliased C.pthread.pthread_cond_t;
   end record;
   pragma Suppress_Initialization (Condition_Variable);

   type Queue is limited record
      Mutex : not null Mutex_Access;
      Pipe : Synchronous_Objects.Event; -- count bytes in the pipe
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
   procedure Notify_All (
      Object : in out Queue;
      Item : not null Queue_Node_Access);
      --  awake Abortable.Take

   type Event is limited record
      Reading_Pipe : C.signed_int;
      Writing_Pipe : C.signed_int;
   end record;
   pragma Suppress_Initialization (Event);

   procedure Read_1 (Reading_Pipe : C.signed_int);
   procedure Write_1 (Writing_Pipe : C.signed_int);

   type RW_Lock is limited record
      Handle : aliased C.pthread.pthread_rwlock_t;
   end record;
   pragma Suppress_Initialization (RW_Lock);

end System.Synchronous_Objects;
