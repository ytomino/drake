pragma License (Unrestricted);
--  implementation package
with System.Soft_Links;
private with C.pthread;
package System.Tasking.Inside is
   pragma Preelaborate;

   type Task_Record (<>) is limited private;

   type Task_Id is access all Task_Record;

   function Get_Current_Task_Id return Task_Id;
   function Get_Main_Task_Id return Task_Id;

   procedure Create (
      T : out Task_Id;
      Params : Address;
      Process : not null access procedure (Params : Address);
      --  activation
      Chain : access Activation_Chain := null;
      --  completion
      Master : Master_Level := Soft_Links.Current_Master.all;
      Parent : Task_Id := null);

   procedure Wait (T : in out Task_Id);
   procedure Detach (T : in out Task_Id);
   function Terminated (T : Task_Id) return Boolean;
   function Activated (T : Task_Id) return Boolean;

   --  for manual activation (Chain /= null)
   procedure Accept_Activation; -- in task
   procedure Activate (Chain : not null access Activation_Chain);
   procedure Move (
      From, To : not null access Activation_Chain;
      New_Master : Master_Level);

   --  for manual completion (Parent /= null)
   function Parent (T : Task_Id) return Task_Id;
   function Master_Level_Of (T : Task_Id) return Master_Level;
   function Master_Within (T : Task_Id) return Master_Level;
   procedure Enter_Master (T : Task_Id);
   procedure Leave_Master (T : Task_Id);

   --  thread local storage
   --  pragma Thread_Local_Storage is valid

--  type TLS_Index is limited private;

--  procedure Allocate (Index : out TLS_Index);
--  procedure Free (Index : in out TLS_Index);
--  function Get (Index : TLS_Index) return Address;
--  procedure Set (Index : TLS_Index; Item : Address);

   --  attribute for Ada.Task_Attributes

   type Attribute_Index is limited private;

   procedure Allocate (Index : in out Attribute_Index);
   procedure Free (Index : in out Attribute_Index);
   function Get (T : Task_Id; Index : Attribute_Index) return Address;
   procedure Set (
      T : Task_Id;
      Index : in out Attribute_Index;
      New_Item : not null access function return Address;
      Finalize : not null access procedure (Item : Address));
   procedure Reference (
      T : Task_Id;
      Index : in out Attribute_Index;
      New_Item : not null access function return Address;
      Finalize : not null access procedure (Item : Address);
      Result : out Address);
   procedure Clear (
      T : Task_Id;
      Index : in out Attribute_Index);

   --  mutex

   type Mutex is limited private;

   procedure Initialize (Object : in out Mutex) is null;
   procedure Finalize (Object : in out Mutex);
   procedure Enter (Object : in out Mutex);
   procedure Leave (Object : in out Mutex);

   --  condition variable (pthread only)

   type Condition_Variable is limited private;

   procedure Initialize (Object : in out Condition_Variable) is null;
   procedure Finalize (Object : in out Condition_Variable);
   procedure Notify_One (Object : in out Condition_Variable);
   procedure Notify_All (Object : in out Condition_Variable);
   procedure Wait (
      Object : in out Condition_Variable;
      Mutex : in out Inside.Mutex);
   procedure Wait (
      Object : in out Condition_Variable;
      Mutex : in out Inside.Mutex;
      Timeout : Duration;
      Notified : out Boolean);

   --  event for Ada.Synchronous_Task_Control

   type Event is limited private;

   procedure Initialize (Object : in out Event) is null;
   procedure Finalize (Object : in out Event);
   procedure Set (Object : in out Event);
   procedure Reset (Object : in out Event);
   function Get (Object : Event) return Boolean;
   procedure Wait (Object : in out Event);
   procedure Wait (
      Object : in out Event;
      Timeout : Duration;
      Value : out Boolean);

   --  semaphore for Ada.Synchronous_Barriers

   type Barrier is limited private;

   procedure Initialize (Object : in out Barrier; Release_Threshold : Natural);
   procedure Finalize (Object : in out Barrier);
   procedure Wait (
      Object : in out Barrier;
      Notified : out Boolean);

   --  multi-read/exclusive-write lock for protected

   type RW_Lock is limited private;

   procedure Initialize (Object : in out RW_Lock) is null;
   procedure Finalize (Object : in out RW_Lock);
   procedure Enter_Reading (Object : in out RW_Lock);
   procedure Leave_Reading (Object : in out RW_Lock);
   procedure Enter_Writing (Object : in out RW_Lock);
   procedure Leave_Writing (Object : in out RW_Lock) renames Leave_Reading;

private

   type Counter is mod 2 ** 32;
   for Counter'Size use 32;

   type Activation_Chain_Data;
   type Activation_Chain_Access is access all Activation_Chain_Data;
   type Activation_Chain_Data is limited record
      List : Task_Id;
      Task_Count : Counter;
      Activated_Count : aliased Counter;
      Mutex : Inside.Mutex;
      Condition_Variable : Inside.Condition_Variable;
      Merged : Activation_Chain_Access;
   end record;
   pragma Suppress_Initialization (Activation_Chain_Data);

   type Master_Data;
   type Master_Access is access all Master_Data;
   type Master_Data is limited record
      Previous : Master_Access;
      Within : Tasking.Master_Level; -- level of stack
      List : Task_Id;
      Mutex : Inside.Mutex;
   end record;
   pragma Suppress_Initialization (Master_Data);

   type Task_Kind is (Main, Sub);
   pragma Discard_Names (Task_Kind);

   type Attribute is record
      Index : access Attribute_Index;
      Item : Address;
      Finalize : access procedure (Item : Address);
      Previous : Task_Id;
      Next : Task_Id;
   end record;
   pragma Suppress_Initialization (Attribute);
   type Attribute_Array is array (Natural) of Attribute;
   pragma Suppress_Initialization (Attribute_Array);
   type Attribute_Array_Access is access Attribute_Array;

   type Termination_State is range 0 .. 2;
   for Termination_State'Size use 8;
   TS_Active : constant Termination_State := 0;
   TS_Detached : constant Termination_State := 1;
   TS_Terminated : constant Termination_State := 2;

   type Task_Record (Kind : Task_Kind) is limited record
      Handle : aliased C.pthread.pthread_t;
      Attributes : Attribute_Array_Access;
      Attributes_Length : Natural;
      --  activation / completion
      Handle_Received : aliased Boolean;
      pragma Atomic (Handle_Received);
      Activated : Boolean;
      pragma Atomic (Activated);
      Parent : Task_Id;
      Master_Level : Tasking.Master_Level; -- level of self
      Master_Top : Master_Access; -- stack
      --  for sub task
      case Kind is
         when Main =>
            null;
         when Sub =>
            Params : Address;
            Process : not null access procedure (Params : Address);
            State : aliased Termination_State;
            pragma Atomic (State);
            --  activation
            Activation_Chain : Activation_Chain_Access;
            Next_Of_Activation_Chain : Task_Id;
            --  completion
            Master_Of_Parent : Master_Access;
            Previous_At_Same_Level : Task_Id;
            Next_At_Same_Level : Task_Id;
      end case;
   end record;
   pragma Suppress_Initialization (Task_Record);

--  type TLS_Index is new C.pthread.pthread_key_t;

   type Attribute_Index is limited record
      Index : Integer range -1 .. Integer'Last;
      List : aliased Task_Id;
      Mutex : aliased Inside.Mutex;
   end record;
   pragma Suppress_Initialization (Attribute_Index);

   type Mutex is limited record
      Handle : aliased C.pthread.pthread_mutex_t :=
         C.pthread.PTHREAD_MUTEX_INITIALIZER;
   end record;

   type Condition_Variable is limited record
      Handle : aliased C.pthread.pthread_cond_t :=
         C.pthread.PTHREAD_COND_INITIALIZER;
   end record;

   type Event is limited record
      Mutex : Inside.Mutex;
      Condition_Variable : Inside.Condition_Variable;
      Value : Boolean := False;
      pragma Atomic (Value);
   end record;

   type Barrier is limited record
      Mutex : Inside.Mutex;
      Condition_Variable : Inside.Condition_Variable;
      Release_Threshold : Natural;
      Blocked : Natural;
   end record;

   type RW_Lock is limited record
      Handle : aliased C.pthread.pthread_rwlock_t :=
         C.pthread.PTHREAD_RWLOCK_INITIALIZER;
   end record;

end System.Tasking.Inside;
