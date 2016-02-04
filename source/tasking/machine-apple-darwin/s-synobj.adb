with Ada.Exception_Identification.From_Here;
with System.Debug;
with System.Native_Time;
with C.errno;
package body System.Synchronous_Objects is
   use Ada.Exception_Identification.From_Here;
   use type C.signed_int;
   use type C.signed_long;
   use type C.pthread.pthread_cond_t;
   use type C.pthread.pthread_mutex_t;
   use type C.pthread.pthread_rwlock_t;

   --  mutex

   procedure Initialize (Object : in out Mutex) is
   begin
      Object.Handle := C.pthread.PTHREAD_MUTEX_INITIALIZER;
   end Initialize;

   procedure Finalize (Object : in out Mutex) is
      R : C.signed_int;
   begin
      R := C.pthread.pthread_mutex_destroy (Object.Handle'Access);
      pragma Check (Debug,
         Check => R = 0
            or else (
               R = C.errno.EINVAL
               and then Object.Handle = C.pthread.PTHREAD_MUTEX_INITIALIZER)
            or else Debug.Runtime_Error ("pthread_mutex_destroy failed"));
   end Finalize;

   procedure Enter (Object : in out Mutex) is
   begin
      if C.pthread.pthread_mutex_lock (Object.Handle'Access) /= 0 then
         Raise_Exception (Tasking_Error'Identity);
      end if;
   end Enter;

   procedure Leave (Object : in out Mutex) is
   begin
      if C.pthread.pthread_mutex_unlock (Object.Handle'Access) /= 0 then
         Raise_Exception (Tasking_Error'Identity);
      end if;
   end Leave;

   --  condition variable

   procedure Initialize (Object : in out Condition_Variable) is
   begin
      Object.Handle := C.pthread.PTHREAD_COND_INITIALIZER;
   end Initialize;

   procedure Finalize (Object : in out Condition_Variable) is
      R : C.signed_int;
   begin
      R := C.pthread.pthread_cond_destroy (Object.Handle'Access);
      pragma Check (Debug,
         Check => R = 0
            or else (
               R = C.errno.EINVAL
               and then Object.Handle = C.pthread.PTHREAD_COND_INITIALIZER)
            or else Debug.Runtime_Error ("pthread_cond_destroy failed"));
   end Finalize;

   procedure Notify_One (Object : in out Condition_Variable) is
   begin
      if C.pthread.pthread_cond_signal (Object.Handle'Access) /= 0 then
         Raise_Exception (Tasking_Error'Identity);
      end if;
   end Notify_One;

   procedure Notify_All (Object : in out Condition_Variable) is
   begin
      if C.pthread.pthread_cond_broadcast (Object.Handle'Access) /= 0 then
         Raise_Exception (Tasking_Error'Identity);
      end if;
   end Notify_All;

   procedure Wait (
      Object : in out Condition_Variable;
      Mutex : in out Synchronous_Objects.Mutex) is
   begin
      if C.pthread.pthread_cond_wait (
         Object.Handle'Access,
         Mutex.Handle'Access) /= 0
      then
         Raise_Exception (Tasking_Error'Identity);
      end if;
   end Wait;

   procedure Wait (
      Object : in out Condition_Variable;
      Mutex : in out Synchronous_Objects.Mutex;
      Timeout : Duration;
      Notified : out Boolean)
   is
      Timeout_T : Native_Calendar.Native_Time := Native_Calendar.Clock;
   begin
      if Timeout >= 0.0 then
         Timeout_T := Native_Time.To_timespec (
            Native_Time.To_Duration (Timeout_T) + Timeout);
      end if;
      Wait (
         Object,
         Mutex,
         Timeout => Timeout_T,
         Notified => Notified);
   end Wait;

   procedure Wait (
      Object : in out Condition_Variable;
      Mutex : in out Synchronous_Objects.Mutex;
      Timeout : Native_Calendar.Native_Time;
      Notified : out Boolean)
   is
      Actual_Timeout : aliased Native_Calendar.Native_Time;
   begin
      if Timeout.tv_sec < 0 then -- CXD2002
         Actual_Timeout.tv_sec := 0;
         Actual_Timeout.tv_nsec := 0;
      else
         Actual_Timeout := Timeout;
      end if;
      case C.pthread.pthread_cond_timedwait (
         Object.Handle'Access,
         Mutex.Handle'Access,
         Actual_Timeout'Access)
      is
         when 0 =>
            Notified := True;
         when C.errno.ETIMEDOUT =>
            Notified := False;
         when others =>
            Raise_Exception (Tasking_Error'Identity);
      end case;
   end Wait;

   --  queue

   procedure Initialize (
      Object : in out Queue;
      Mutex : not null access Synchronous_Objects.Mutex) is
   begin
      Object.Mutex := Mutex;
      Initialize (Object.Condition_Variable);
      Object.Head := null;
      Object.Tail := null;
      Object.Filter := null;
      Object.Waiting := False;
      Object.Canceled := False;
   end Initialize;

   procedure Finalize (Object : in out Queue) is
   begin
      Finalize (Object.Condition_Variable);
   end Finalize;

   function Count (
      Object : Queue;
      Params : Address;
      Filter : Queue_Filter)
      return Natural
   is
      Result : Natural;
   begin
      Enter (Object.Mutex.all);
      Result := Unsynchronized_Count (Object, Params, Filter);
      Leave (Object.Mutex.all);
      return Result;
   end Count;

   function Unsynchronized_Count (
      Object : Queue;
      Params : Address;
      Filter : Queue_Filter)
      return Natural
   is
      Result : Natural := 0;
      I : Queue_Node_Access := Object.Head;
   begin
      while I /= null loop
         if Filter = null or else Filter (I, Params) then
            Result := Result + 1;
         end if;
         I := I.Next;
      end loop;
      return Result;
   end Unsynchronized_Count;

   function Canceled (Object : Queue) return Boolean is
   begin
      return Object.Canceled;
   end Canceled;

   procedure Cancel (
      Object : in out Queue;
      Cancel_Node : access procedure (X : in out Queue_Node_Access)) is
   begin
      Enter (Object.Mutex.all);
      Object.Canceled := True;
      if Cancel_Node /= null then
         while Object.Head /= null loop
            declare
               Next : constant Queue_Node_Access := Object.Head.Next;
            begin
               Cancel_Node (Object.Head);
               Object.Head := Next;
            end;
         end loop;
      end if;
      Leave (Object.Mutex.all);
   end Cancel;

   procedure Unsynchronized_Prepend (
      Object : in out Queue;
      Item : not null Queue_Node_Access;
      Canceled : out Boolean) is
   begin
      Canceled := Object.Canceled;
      if not Canceled then
         Item.Next := Object.Head;
         Object.Head := Item;
         if Object.Tail = null then
            Object.Tail := Item;
         end if;
         Notify_All (Object, Item);
      end if;
   end Unsynchronized_Prepend;

   procedure Add (
      Object : in out Queue;
      Item : not null Queue_Node_Access;
      Canceled : out Boolean) is
   begin
      Enter (Object.Mutex.all);
      Canceled := Object.Canceled;
      if not Canceled then
         if Object.Head = null then
            Object.Head := Item;
         else
            Object.Tail.Next := Item;
         end if;
         Object.Tail := Item;
         Item.Next := null;
         Notify_All (Object, Item);
      end if;
      Leave (Object.Mutex.all);
   end Add;

   procedure Take (
      Object : in out Queue;
      Item : out Queue_Node_Access;
      Params : Address;
      Filter : Queue_Filter) is
   begin
      Enter (Object.Mutex.all);
      Unsynchronized_Take (Object, Item, Params, Filter);
      Leave (Object.Mutex.all);
   end Take;

   procedure Unsynchronized_Take (
      Object : in out Queue;
      Item : out Queue_Node_Access;
      Params : Address;
      Filter : Queue_Filter)
   is
      Previous : Queue_Node_Access := null;
      I : Queue_Node_Access := Object.Head;
   begin
      Take_No_Sync (Object, Item, Params, Filter, Previous, I);
   end Unsynchronized_Take;

   procedure Take_No_Sync (
      Object : in out Queue;
      Item : out Queue_Node_Access;
      Params : Address;
      Filter : Queue_Filter;
      Previous : in out Queue_Node_Access;
      Current : in out Queue_Node_Access) is
   begin
      Item := null;
      Search : while Current /= null loop
         if Filter = null or else Filter (Current, Params) then
            if Previous /= null then
               Previous.Next := Current.Next;
            else
               Object.Head := Current.Next;
            end if;
            if Current = Object.Tail then
               Object.Tail := Previous;
            end if;
            Item := Current;
            exit Search;
         end if;
         Previous := Current;
         Current := Current.Next;
      end loop Search;
   end Take_No_Sync;

   procedure Notify_All (
      Object : in out Queue;
      Item : not null Queue_Node_Access) is
   begin
      if Object.Waiting
         and then (Object.Filter = null
            or else Object.Filter (Item, Object.Params))
      then
         Notify_All (Object.Condition_Variable);
      end if;
   end Notify_All;

   --  event

   procedure Initialize (Object : in out Event) is
   begin
      Initialize (Object.Mutex);
      Initialize (Object.Condition_Variable);
      Object.Value := False;
   end Initialize;

   procedure Finalize (Object : in out Event) is
   begin
      Finalize (Object.Mutex);
      Finalize (Object.Condition_Variable);
   end Finalize;

   function Get (Object : Event) return Boolean is
   begin
      return Object.Value; -- atomic, is it ok?
   end Get;

   procedure Set (Object : in out Event) is
   begin
      Enter (Object.Mutex);
      Object.Value := True;
      Notify_All (Object.Condition_Variable);
      Leave (Object.Mutex);
   end Set;

   procedure Reset (Object : in out Event) is
   begin
      Enter (Object.Mutex);
      Object.Value := False;
      Leave (Object.Mutex);
   end Reset;

   procedure Wait (Object : in out Event) is
   begin
      Enter (Object.Mutex);
      if not Object.Value then
         loop
            Wait (
               Object.Condition_Variable,
               Object.Mutex);
            exit when Object.Value;
         end loop;
      end if;
      Leave (Object.Mutex);
   end Wait;

   procedure Wait (
      Object : in out Event;
      Timeout : Duration;
      Value : out Boolean) is
   begin
      Enter (Object.Mutex);
      if Object.Value then
         Value := True;
      else
         Wait (
            Object.Condition_Variable,
            Object.Mutex,
            Timeout => Timeout,
            Notified => Value);
      end if;
      Leave (Object.Mutex);
   end Wait;

   --  group-synchronization

   procedure Initialize (
      Object : in out Barrier;
      Release_Threshold : Natural) is
   begin
      Initialize (Object.Mutex);
      Initialize (Object.Condition_Variable);
      Object.Release_Threshold := Release_Threshold;
      Object.Blocked := 0;
      Object.Unblocked := 0;
   end Initialize;

   procedure Finalize (Object : in out Barrier) is
   begin
      Finalize (Object.Mutex);
      Finalize (Object.Condition_Variable);
   end Finalize;

   procedure Wait (
      Object : in out Barrier;
      Notified : out Boolean)
   is
      Order : Natural;
   begin
      Enter (Object.Mutex);
      Object.Blocked := Object.Blocked + 1;
      Order := Object.Blocked rem Object.Release_Threshold;
      Notified := Order = 1;
      if Order = 0 then
         Notify_All (Object.Condition_Variable);
         Object.Unblocked := Object.Unblocked + 1;
      else
         loop
            Wait (Object.Condition_Variable, Object.Mutex);
            exit when Object.Blocked >= Object.Release_Threshold;
         end loop;
         Object.Unblocked := Object.Unblocked + 1;
      end if;
      if Object.Unblocked = Object.Release_Threshold then
         Object.Blocked := Object.Blocked - Object.Release_Threshold;
         Object.Unblocked := 0;
      end if;
      Leave (Object.Mutex);
   end Wait;

   --  multi-read/exclusive-write lock

   procedure Initialize (Object : in out RW_Lock) is
   begin
      Object.Handle := C.pthread.PTHREAD_RWLOCK_INITIALIZER;
   end Initialize;

   procedure Finalize (Object : in out RW_Lock) is
      R : C.signed_int;
   begin
      R := C.pthread.pthread_rwlock_destroy (Object.Handle'Access);
      pragma Check (Debug,
         Check => R = 0
            or else (
               R = C.errno.EINVAL
               and then Object.Handle = C.pthread.PTHREAD_RWLOCK_INITIALIZER)
            or else Debug.Runtime_Error ("pthread_rwlock_destroy failed"));
   end Finalize;

   procedure Enter_Reading (Object : in out RW_Lock) is
   begin
      Again_Loop : loop
         case C.pthread.pthread_rwlock_rdlock (Object.Handle'Access) is
            when 0 =>
               exit Again_Loop;
            when C.errno.EAGAIN =>
               null; -- loop
            when others =>
               Raise_Exception (Tasking_Error'Identity);
         end case;
      end loop Again_Loop;
   end Enter_Reading;

   procedure Enter_Writing (Object : in out RW_Lock) is
   begin
      if C.pthread.pthread_rwlock_wrlock (Object.Handle'Access) /= 0 then
         Raise_Exception (Tasking_Error'Identity);
      end if;
   end Enter_Writing;

   procedure Leave (Object : in out RW_Lock) is
   begin
      if C.pthread.pthread_rwlock_unlock (Object.Handle'Access) /= 0 then
         Raise_Exception (Tasking_Error'Identity);
      end if;
   end Leave;

end System.Synchronous_Objects;
