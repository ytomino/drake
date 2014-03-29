with Ada.Exception_Identification.From_Here;
with C.errno;
package body System.Synchronous_Objects is
   use Ada.Exception_Identification.From_Here;
   use type C.signed_int;

   --  mutex

   procedure Initialize (Object : in out Mutex) is
   begin
      Object.Handle := C.pthread.PTHREAD_MUTEX_INITIALIZER;
   end Initialize;

   procedure Finalize (Object : in out Mutex) is
   begin
      if C.pthread.pthread_mutex_destroy (Object.Handle'Access) /= 0 then
         null; -- raise Tasking_Error;
      end if;
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
   begin
      if C.pthread.pthread_cond_destroy (Object.Handle'Access) /= 0 then
         null; -- raise Tasking_Error;
      end if;
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
      Timeout : Native_Time.Native_Time;
      Notified : out Boolean) is
   begin
      case C.pthread.pthread_cond_timedwait (
         Object.Handle'Access,
         Mutex.Handle'Access,
         Timeout'Unrestricted_Access)
      is
         when 0 =>
            Notified := True;
         when C.errno.ETIMEDOUT =>
            Notified := False;
         when others =>
            Raise_Exception (Tasking_Error'Identity);
      end case;
   end Wait;

   procedure Wait (
      Object : in out Condition_Variable;
      Mutex : in out Synchronous_Objects.Mutex;
      Timeout : Duration;
      Notified : out Boolean) is
   begin
      Wait (
         Object,
         Mutex,
         Native_Time.Clock + Timeout,
         Notified);
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
      Object.Waiting := False;
      Object.Filter := null;
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
      Result : Natural := 0;
   begin
      Enter (Object.Mutex.all);
      declare
         I : Queue_Node_Access := Object.Head;
      begin
         while I /= null loop
            if Filter = null or else Filter (I, Params) then
               Result := Result + 1;
            end if;
            I := I.Next;
         end loop;
      end;
      Leave (Object.Mutex.all);
      return Result;
   end Count;

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

   procedure Add (
      Object : in out Queue;
      Item : not null Queue_Node_Access)
   is
      Error : Boolean;
   begin
      Enter (Object.Mutex.all);
      Error := Object.Canceled;
      if not Error then
         if Object.Head = null then
            Object.Head := Item;
         else
            Object.Tail.Next := Item;
         end if;
         Object.Tail := Item;
         Item.Next := null;
         if Object.Waiting
            and then (Object.Filter = null
               or else Object.Filter (Item, Object.Params))
         then
            Notify_All (Object.Condition_Variable);
         end if;
      end if;
      Leave (Object.Mutex.all);
      if Error then
         Raise_Exception (Tasking_Error'Identity);
      end if;
   end Add;

   procedure Take (
      Object : in out Queue;
      Item : out Queue_Node_Access;
      Params : Address;
      Filter : Queue_Filter) is
   begin
      Enter (Object.Mutex.all);
      declare
         Previous : Queue_Node_Access := null;
         I : Queue_Node_Access := Object.Head;
      begin
         Take_No_Sync (Object, Item, Params, Filter, Previous, I);
      end;
      Leave (Object.Mutex.all);
   end Take;

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
--       pragma Assert (Object.Value >= Value);
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
   end Initialize;

   procedure Finalize (Object : in out Barrier) is
   begin
      Finalize (Object.Mutex);
      Finalize (Object.Condition_Variable);
   end Finalize;

   procedure Wait (
      Object : in out Barrier;
      Notified : out Boolean) is
   begin
      Enter (Object.Mutex);
      Notified := Object.Blocked = 0;
      Object.Blocked := Object.Blocked + 1;
      if Object.Blocked = Object.Release_Threshold then
         Notify_All (Object.Condition_Variable);
         Object.Blocked := 0;
      else
         Wait (
            Object.Condition_Variable,
            Object.Mutex);
      end if;
      Leave (Object.Mutex);
   end Wait;

   --  multi-read/exclusive-write lock

   procedure Initialize (Object : in out RW_Lock) is
   begin
      Object.Handle := C.pthread.PTHREAD_RWLOCK_INITIALIZER;
   end Initialize;

   procedure Finalize (Object : in out RW_Lock) is
   begin
      if C.pthread.pthread_rwlock_destroy (Object.Handle'Access) /= 0 then
         null; -- raise Tasking_Error;
      end if;
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

   --  for Abortable

   function "+" (Left : Native_Time.Native_Time; Right : Duration)
      return Native_Time.Native_Time is
   begin
      return Native_Time.To_Native_Time (
         Native_Time.To_Time (Left) + Right);
   end "+";

end System.Synchronous_Objects;
