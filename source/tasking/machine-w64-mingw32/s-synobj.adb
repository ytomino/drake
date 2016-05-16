with Ada.Exception_Identification.From_Here;
with System.Debug;
with C.winbase;
with C.windef;
with C.winerror;
package body System.Synchronous_Objects is
   use Ada.Exception_Identification.From_Here;
   use type C.windef.DWORD;
   use type C.windef.WINBOOL;

   function atomic_load (
      ptr : not null access constant Counter;
      memorder : Integer := C.ATOMIC_ACQUIRE)
      return Counter
      with Import, Convention => Intrinsic, External_Name => "__atomic_load_4";

   procedure atomic_store (
      ptr : not null access Counter;
      val : Counter;
      memorder : Integer := C.ATOMIC_RELEASE)
      with Import,
         Convention => Intrinsic, External_Name => "__atomic_store_4";

   procedure atomic_add_fetch (
      ptr : not null access Counter;
      val : Counter;
      memorder : Integer := C.ATOMIC_ACQ_REL)
      with Import,
         Convention => Intrinsic, External_Name => "__atomic_add_fetch_4";

   function atomic_sub_fetch (
      ptr : not null access Counter;
      val : Counter;
      memorder : Integer := C.ATOMIC_ACQ_REL)
      return Counter
      with Import,
         Convention => Intrinsic, External_Name => "__atomic_sub_fetch_4";

   function atomic_compare_exchange (
      ptr : not null access Counter;
      expected : not null access Counter;
      desired : Counter;
      weak : Boolean := False;
      success_memorder : Integer := C.ATOMIC_ACQ_REL;
      failure_memorder : Integer := C.ATOMIC_ACQUIRE)
      return Boolean
      with Import,
         Convention => Intrinsic,
         External_Name => "__atomic_compare_exchange_4";

   --  mutex

   procedure Initialize (Object : in out Mutex) is
   begin
      Object.Handle := C.winbase.CreateMutex (null, 0, null);
   end Initialize;

   procedure Finalize (Object : in out Mutex) is
      R : C.windef.WINBOOL;
   begin
      R := C.winbase.CloseHandle (Object.Handle);
      pragma Check (Debug,
         Check => R /= 0 or else Debug.Runtime_Error ("CloseHandle failed"));
   end Finalize;

   procedure Enter (Object : in out Mutex) is
   begin
      if C.winbase.WaitForSingleObject (Object.Handle, C.winbase.INFINITE) /=
         C.winbase.WAIT_OBJECT_0
      then
         Raise_Exception (Tasking_Error'Identity);
      end if;
   end Enter;

   procedure Leave (Object : in out Mutex) is
   begin
      if C.winbase.ReleaseMutex (Object.Handle) = 0 then
         Raise_Exception (Tasking_Error'Identity);
      end if;
   end Leave;

   --  condition variable

   procedure Initialize (Object : in out Condition_Variable) is
   begin
      atomic_store (Object.Waiters'Access, 0);
      Initialize (Object.Event, Manual => True);
      Initialize (Object.Reset_Barrier, Manual => True);
   end Initialize;

   procedure Finalize (Object : in out Condition_Variable) is
   begin
      Finalize (Object.Event);
      Finalize (Object.Reset_Barrier);
   end Finalize;

   procedure Notify_All (Object : in out Condition_Variable) is
   begin
      Reset (Object.Reset_Barrier);
      Set (Object.Event);
   end Notify_All;

   procedure Wait (
      Object : in out Condition_Variable;
      Mutex : in out Synchronous_Objects.Mutex) is
   begin
      atomic_add_fetch (Object.Waiters'Access, 1);
      case C.winbase.SignalObjectAndWait (
         hObjectToSignal => Mutex.Handle,
         hObjectToWaitOn => Object.Event.Handle,
         dwMilliseconds => C.winbase.INFINITE,
         bAlertable => 0)
      is
         when C.winbase.WAIT_OBJECT_0 =>
            null;
         when others =>
            Raise_Exception (Tasking_Error'Identity);
      end case;
      if atomic_sub_fetch (Object.Waiters'Access, 1) = 0 then
         Reset (Object.Event);
         Set (Object.Reset_Barrier);
      else
         Wait (Object.Reset_Barrier);
      end if;
      Enter (Mutex);
   end Wait;

   --  queue

   procedure Initialize (
      Object : in out Queue;
      Mutex : not null Mutex_Access) is
   begin
      Object.Mutex := Mutex;
      Initialize (Object.Event, Manual => False);
      Object.Head := null;
      Object.Tail := null;
      Object.Filter := null;
      Object.Waiting := False;
      Object.Canceled := False;
   end Initialize;

   procedure Finalize (Object : in out Queue) is
   begin
      Finalize (Object.Event);
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
         and then (
            Object.Filter = null or else Object.Filter (Item, Object.Params))
      then
         Set (Object.Event);
      end if;
   end Notify_All;

   --  event for Ada.Synchronous_Task_Control

   procedure Initialize (Object : in out Event; Manual : Boolean := True) is
   begin
      Object.Handle :=
         C.winbase.CreateEvent (null, Boolean'Pos (Manual), 0, null);
   end Initialize;

   procedure Finalize (Object : in out Event) is
      R : C.windef.WINBOOL;
   begin
      R := C.winbase.CloseHandle (Object.Handle);
      pragma Check (Debug,
         Check => R /= 0 or else Debug.Runtime_Error ("CloseHandle failed"));
   end Finalize;

   function Get (Object : Event) return Boolean is
   begin
      return C.winbase.WaitForSingleObject (Object.Handle, 0) =
         C.winbase.WAIT_OBJECT_0;
   end Get;

   procedure Set (Object : in out Event) is
   begin
      if C.winbase.SetEvent (Object.Handle) = 0 then
         Raise_Exception (Tasking_Error'Identity);
      end if;
   end Set;

   procedure Reset (Object : in out Event) is
   begin
      if C.winbase.ResetEvent (Object.Handle) = 0 then
         Raise_Exception (Tasking_Error'Identity);
      end if;
   end Reset;

   procedure Wait (
      Object : in out Event) is
   begin
      if C.winbase.WaitForSingleObject (Object.Handle, C.winbase.INFINITE) /=
         C.winbase.WAIT_OBJECT_0
      then
         Raise_Exception (Tasking_Error'Identity);
      end if;
   end Wait;

   procedure Wait (
      Object : in out Event;
      Timeout : Duration;
      Value : out Boolean)
   is
      Milliseconds : constant C.windef.DWORD :=
         C.windef.DWORD (Duration'Max (0.0, Timeout) * 1_000);
   begin
      case C.winbase.WaitForSingleObject (Object.Handle, Milliseconds) is
         when C.winbase.WAIT_OBJECT_0 =>
            Value := True;
         when C.winerror.WAIT_TIMEOUT =>
            Value := False;
         when others =>
            Raise_Exception (Tasking_Error'Identity);
      end case;
   end Wait;

   --  group-synchronization for Ada.Synchronous_Barriers

   procedure Initialize (
      Object : in out Barrier;
      Release_Threshold : Natural) is
   begin
      Object.Release_Threshold := Release_Threshold;
      Object.Blocked := 0;
      Object.Unblocked := 0;
      Initialize (Object.Mutex);
      Initialize (Object.Event, Manual => True);
   end Initialize;

   procedure Finalize (
      Object : in out Barrier) is
   begin
      Finalize (Object.Mutex);
      Finalize (Object.Event);
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
         Set (Object.Event);
         Object.Unblocked := Object.Unblocked + 1;
      else
         loop
            Leave (Object.Mutex);
            Wait (Object.Event);
            Enter (Object.Mutex);
            exit when Object.Blocked >= Object.Release_Threshold;
         end loop;
         Object.Unblocked := Object.Unblocked + 1;
      end if;
      if Object.Unblocked = Object.Release_Threshold then
         Object.Blocked := Object.Blocked - Object.Release_Threshold;
         Object.Unblocked := 0;
         if Object.Blocked < Object.Release_Threshold then
            Reset (Object.Event);
         end if;
      end if;
      Leave (Object.Mutex);
   end Wait;

   --  multi-read/exclusive-write lock for protected

   procedure Initialize (Object : in out RW_Lock) is
   begin
      atomic_store (Object.State'Access, 0);
      Initialize (Object.Reader_Barrier, Manual => True);
      Initialize (Object.Writer_Barrier, Manual => False);
   end Initialize;

   procedure Finalize (Object : in out RW_Lock) is
   begin
      Finalize (Object.Reader_Barrier);
      Finalize (Object.Writer_Barrier);
   end Finalize;

   procedure Enter_Reading (Object : in out RW_Lock) is
   begin
      loop
         declare
            Current : constant Counter := atomic_load (Object.State'Access);
         begin
            if Current >= 0 then
               declare
                  Expected : aliased Counter := Current;
               begin
                  exit when atomic_compare_exchange (
                     Object.State'Access,
                     Expected'Access,
                     Current + 1);
               end;
            else
               Wait (Object.Reader_Barrier);
            end if;
         end;
      end loop;
   end Enter_Reading;

   procedure Enter_Writing (Object : in out RW_Lock) is
   begin
      loop
         declare
            Expected : aliased Counter := 0;
         begin
            exit when atomic_compare_exchange (
               Object.State'Access,
               Expected'Access,
               -999);
         end;
         Wait (Object.Writer_Barrier);
      end loop;
      Reset (Object.Reader_Barrier);
   end Enter_Writing;

   procedure Leave (Object : in out RW_Lock) is
      Expected : aliased Counter := -999;
   begin
      if atomic_compare_exchange (Object.State'Access, Expected'Access, 0) then
         --  writer
         Set (Object.Reader_Barrier);
         Set (Object.Writer_Barrier);
      else
         --  reader
         if atomic_sub_fetch (Object.State'Access, 1) = 0 then
            Reset (Object.Reader_Barrier);
            Set (Object.Writer_Barrier);
         end if;
      end if;
   end Leave;

end System.Synchronous_Objects;
