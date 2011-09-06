pragma License (Unrestricted);
--  implementation unit
private with C.pthread;
package System.Tasking.Inside is
   pragma Preelaborate;

   type Task_Record (<>) is limited private;
   type Task_Id is access all Task_Record;

   function Get_Current_Task_Id return Task_Id;
   function Get_Main_Task_Id return Task_Id;

   type Master_Record is limited private;
   type Master_Access is access all Master_Record;

   procedure Create (
      T : out Task_Id;
      Params : Address;
      Process : not null access procedure (Params : Address);
      --  name
      Name : String := "";
      --  activation
      Chain : access Activation_Chain := null;
      Elaborated : access Boolean := null;
      --  completion
      Master : Master_Access := null;
      --  rendezvous
      Entry_Last_Index : Task_Entry_Index := 0);

   procedure Wait (T : in out Task_Id);
   procedure Detach (T : in out Task_Id);
   function Terminated (T : Task_Id) return Boolean;
   function Activated (T : Task_Id) return Boolean;

   --  name
   function Name (T : Task_Id) return String;

   --  abort
   procedure Abort_Task (T : Task_Id);
   procedure Enable_Abort;
   procedure Disable_Abort (Aborted : Boolean); -- check and disable

   --  for manual activation (Chain /= null)
   procedure Accept_Activation; -- current task
   procedure Activate (
      Chain : not null access Activation_Chain;
      Has_Error : out Boolean;
      Aborted : out Boolean);
   procedure Activate (T : Task_Id; Final : Boolean); -- activate single task
   procedure Move (
      From, To : not null access Activation_Chain;
      New_Master : Master_Access);

   --  for manual completion (Master /= null)
   function Parent (T : Task_Id) return Task_Id;
   function Master_Level_Of (T : Task_Id) return Master_Level;
   function Master_Within (T : Task_Id) return Master_Level;
   procedure Enter_Master; -- current task
   procedure Leave_Master; -- current task
   function Get_Master_Of_Parent (Level : Master_Level) return Master_Access;

   --  for rendezvous
   procedure Set_Entry_Name (
      T : Task_Id;
      Index : Task_Entry_Index;
      Name : Entry_Name_Access);
   procedure Set_Entry_Names_To_Deallocate (T : Task_Id);
   type Queue_Node is limited private;
   type Queue_Node_Access is access all Queue_Node;
   type Queue_Filter is access function (
      Item : not null Queue_Node_Access;
      Params : Address)
      return Boolean;
   procedure Call (T : Task_Id; Item : not null Queue_Node_Access);
   procedure Accept_Call (
      Item : out Queue_Node_Access;
      Params : Address;
      Filter : Queue_Filter;
      Aborted : out Boolean);

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
      Mutex : in out Inside.Mutex;
      Aborted : out Boolean);
   procedure Wait (
      Object : in out Condition_Variable;
      Mutex : in out Inside.Mutex;
      Timeout : Duration;
      Notified : out Boolean;
      Aborted : out Boolean);

   --  queue

   type Queue is limited private;
--  type Queue_Node is limited private;
--  type Queue_Node_Access is access all Queue_Node;
--  type Queue_Filter is access function (
--    Item : not null Queue_Node_Access;
--    Params : Address)
--    return Boolean;
   procedure Initialize (Object : in out Queue) is null;
   procedure Finalize (
      Object : in out Queue;
      Free_Node : access procedure (X : in out Queue_Node_Access));
   procedure Add (
      Object : in out Queue;
      Item : not null Queue_Node_Access);
   procedure Take (
      Object : in out Queue;
      Item : out Queue_Node_Access;
      Params : Address;
      Filter : Queue_Filter;
      Aborted : out Boolean);

   --  event for Ada.Synchronous_Task_Control

   type Event is limited private;

   procedure Initialize (Object : in out Event) is null;
   procedure Finalize (Object : in out Event);
   procedure Set (Object : in out Event);
   procedure Reset (Object : in out Event);
   function Get (Object : Event) return Boolean;
   procedure Wait (
      Object : in out Event;
      Aborted : out Boolean);
   procedure Wait (
      Object : in out Event;
      Timeout : Duration;
      Value : out Boolean;
      Aborted : out Boolean);

   --  group-synchronization for Ada.Synchronous_Barriers

   type Barrier is limited private;

   procedure Initialize (Object : in out Barrier; Release_Threshold : Natural);
   procedure Finalize (Object : in out Barrier);
   procedure Wait (
      Object : in out Barrier;
      Notified : out Boolean;
      Aborted : out Boolean);

   --  multi-read/exclusive-write lock for protected

   type RW_Lock is limited private;

   procedure Initialize (Object : in out RW_Lock) is null;
   procedure Finalize (Object : in out RW_Lock);
   procedure Enter_Reading (Object : in out RW_Lock);
   procedure Leave_Reading (Object : in out RW_Lock);
   procedure Enter_Writing (Object : in out RW_Lock);
   procedure Leave_Writing (Object : in out RW_Lock) renames Leave_Reading;

private

   type String_Access is access String;

   type Counter is mod 2 ** 32;
   for Counter'Size use 32;

   type Activation_Chain_Data;
   type Activation_Chain_Access is access all Activation_Chain_Data;
   type Activation_Chain_Data is limited record
      List : Task_Id;
      Task_Count : Counter;
      Activated_Count : aliased Counter;
      Release_Count : aliased Counter;
      Elaboration_Error : Boolean;
      Mutex : Inside.Mutex;
      Condition_Variable : Inside.Condition_Variable;
      Merged : Activation_Chain_Access;
      Self : access Activation_Chain;
   end record;
   pragma Suppress_Initialization (Activation_Chain_Data);

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

   type Entry_Name_Array is
      array (Task_Entry_Index range <>) of Entry_Name_Access;
   pragma Suppress_Initialization (Entry_Name_Array);

   type Rendezvous_Record (Last_Index : Task_Entry_Index) is limited record
      Calling : Queue;
      To_Deallocate_Names : Boolean;
      Names : Entry_Name_Array (1 .. Last_Index);
   end record;
   pragma Suppress_Initialization (Rendezvous_Record);

   type Rendezvous_Access is access Rendezvous_Record;

   type Task_Record (Kind : Task_Kind) is limited record
      Handle : aliased C.pthread.pthread_t;
      Aborted : Boolean;
      Attributes : Attribute_Array_Access;
      Attributes_Length : Natural;
      --  activation / completion
      Activation_State : aliased Boolean;
      pragma Atomic (Activation_State);
      Termination_State : aliased Inside.Termination_State;
      pragma Atomic (Termination_State);
      Master_Level : Tasking.Master_Level; -- level of self
      Master_Top : Master_Access; -- stack
      --  for sub task
      case Kind is
         when Main =>
            null;
         when Sub =>
            Params : Address;
            Process : not null access procedure (Params : Address);
            --  name
            Name : String_Access;
            --  manual activation
            Activation_Chain : Activation_Chain_Access;
            Next_Of_Activation_Chain : Task_Id;
            Elaborated : access Boolean;
            --  manual completion
            Master_Of_Parent : Master_Access;
            Previous_At_Same_Level : Task_Id;
            Next_At_Same_Level : Task_Id;
            --  rendezvous
            Rendezvous : Rendezvous_Access;
      end case;
   end record;
   pragma Suppress_Initialization (Task_Record);

   type Master_Record is limited record
      Previous : Master_Access; -- previous item in stack
      Parent : Task_Id;
      Within : Tasking.Master_Level; -- level of stack
      List : Task_Id; -- ringed list
      Mutex : Inside.Mutex;
   end record;
   pragma Suppress_Initialization (Master_Record);

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

   type Queue is limited record
      Mutex : Inside.Mutex;
      Condition_Variable : Inside.Condition_Variable;
      Head : aliased Queue_Node_Access := null;
      Tail : Queue_Node_Access := null;
      Waiting : Boolean := False;
      Params : Address;
      Filter : Queue_Filter := null;
   end record;

   type Queue_Node is limited record
      Next : aliased Queue_Node_Access;
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
