with Ada.Exceptions;
with C.winbase;
with C.windef;
with C.winerror;
package body System.Tasking.Synchronous_Objects is
   use type C.windef.DWORD;
   use type C.windef.WINBOOL;

   --  mutex

   procedure Initialize (Object : in out Mutex) is
   begin
      Object.Handle := C.winbase.CreateMutex (null, 0, null);
   end Initialize;

   procedure Finalize (Object : in out Mutex) is
   begin
      if C.winbase.CloseHandle (Object.Handle) = 0 then
         null; -- ignore
      end if;
   end Finalize;

   procedure Enter (Object : in out Mutex) is
   begin
      if C.winbase.WaitForSingleObject (Object.Handle, C.winbase.INFINITE) /=
         C.winbase.WAIT_OBJECT_0
      then
         Ada.Exceptions.Raise_Exception_From_Here (Tasking_Error'Identity);
      end if;
   end Enter;

   procedure Leave (Object : in out Mutex) is
   begin
      if C.winbase.ReleaseMutex (Object.Handle) = 0 then
         Ada.Exceptions.Raise_Exception_From_Here (Tasking_Error'Identity);
      end if;
   end Leave;

   --  condition variable

   procedure Initialize (Object : in out Condition_Variable) is
   begin
      Object.Waiters := 0;
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
      sync_add_and_fetch (Object.Waiters'Access, 1);
      case C.winbase.SignalObjectAndWait (
         hObjectToSignal => Mutex.Handle,
         hObjectToWaitOn => Object.Event.Handle,
         dwMilliseconds => C.winbase.INFINITE,
         bAlertable => 0)
      is
         when C.winbase.WAIT_OBJECT_0 =>
            null;
         when others =>
            Ada.Exceptions.Raise_Exception_From_Here (Tasking_Error'Identity);
      end case;
      if sync_sub_and_fetch (Object.Waiters'Access, 1) = 0 then
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
      Mutex : not null access Synchronous_Objects.Mutex) is
   begin
      Object.Mutex := Mutex;
      Initialize (Object.Event, Manual => False);
      Object.Head := null;
      Object.Tail := null;
      Object.Waiting := False;
      Object.Filter := null;
      Object.Canceled := False;
   end Initialize;

   procedure Finalize (Object : in out Queue) is
   begin
      Finalize (Object.Event);
   end Finalize;

   function Count (
      Object : not null access Queue;
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
            Set (Object.Event);
         end if;
      end if;
      Leave (Object.Mutex.all);
      if Error then
         Ada.Exceptions.Raise_Exception_From_Here (Tasking_Error'Identity);
      end if;
   end Add;

   procedure Take ( -- no waiting
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

   --  event for Ada.Synchronous_Task_Control

   procedure Initialize (Object : in out Event; Manual : Boolean := True) is
   begin
      Object.Handle :=
         C.winbase.CreateEvent (null, Boolean'Pos (Manual), 0, null);
   end Initialize;

   procedure Finalize (Object : in out Event) is
   begin
      if C.winbase.CloseHandle (Object.Handle) = 0 then
         null; -- ignore
      end if;
   end Finalize;

   function Get (Object : Event) return Boolean is
   begin
      return C.winbase.WaitForSingleObject (Object.Handle, 0) =
         C.winbase.WAIT_OBJECT_0;
   end Get;

   procedure Set (Object : in out Event) is
   begin
      if C.winbase.SetEvent (Object.Handle) = 0 then
         Ada.Exceptions.Raise_Exception_From_Here (Tasking_Error'Identity);
      end if;
   end Set;

   procedure Reset (Object : in out Event) is
   begin
      if C.winbase.ResetEvent (Object.Handle) = 0 then
         Ada.Exceptions.Raise_Exception_From_Here (Tasking_Error'Identity);
      end if;
   end Reset;

   procedure Wait (
      Object : in out Event) is
   begin
      if C.winbase.WaitForSingleObject (Object.Handle, C.winbase.INFINITE) /=
         C.winbase.WAIT_OBJECT_0
      then
         Ada.Exceptions.Raise_Exception_From_Here (Tasking_Error'Identity);
      end if;
   end Wait;

   procedure Wait (
      Object : in out Event;
      Timeout : Native_Time.Native_Time;
      Value : out Boolean) is
   begin
      Wait (Object, Timeout - Native_Time.Clock, Value);
   end Wait;

   procedure Wait (
      Object : in out Event;
      Timeout : Duration;
      Value : out Boolean)
   is
      Milliseconds : constant C.windef.DWORD :=
         C.windef.DWORD (Duration'Max (0.0, Timeout) * 1000);
   begin
      case C.winbase.WaitForSingleObject (Object.Handle, Milliseconds) is
         when C.winbase.WAIT_OBJECT_0 =>
            Value := True;
         when C.winerror.WAIT_TIMEOUT =>
            Value := False;
         when others =>
            Ada.Exceptions.Raise_Exception_From_Here (Tasking_Error'Identity);
      end case;
   end Wait;

   --  group-synchronization for Ada.Synchronous_Barriers

   procedure Initialize (
      Object : in out Barrier;
      Release_Threshold : Natural) is
   begin
      Object.Release_Threshold := Counter (Release_Threshold);
      Object.Blocked := 0;
      Initialize (Object.Event, Manual => True);
   end Initialize;

   procedure Finalize (
      Object : in out Barrier) is
   begin
      Finalize (Object.Event);
   end Finalize;

   procedure Wait (
      Object : in out Barrier;
      Notified : out Boolean)
   is
      Added : Counter;
   begin
      Added := sync_add_and_fetch (Object.Blocked'Access, 1);
      Notified := Added = 1;
      if Added = Object.Release_Threshold then
         Object.Blocked := 0;
         Set (Object.Event);
      else
         Wait (Object.Event);
      end if;
   end Wait;

   --  multi-read/exclusive-write lock for protected

   procedure Initialize (Object : in out RW_Lock) is
   begin
      Object.State := 0;
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
            Current : constant Counter := Object.State;
         begin
            if Current >= 0 then
               exit when sync_bool_compare_and_swap (
                  Object.State'Access,
                  Current,
                  Current + 1);
            else
               Wait (Object.Reader_Barrier);
            end if;
         end;
      end loop;
   end Enter_Reading;

   procedure Enter_Writing (Object : in out RW_Lock) is
   begin
      loop
         exit when sync_bool_compare_and_swap (Object.State'Access, 0, -999);
         Wait (Object.Writer_Barrier);
      end loop;
      Reset (Object.Reader_Barrier);
   end Enter_Writing;

   procedure Leave (Object : in out RW_Lock) is
   begin
      if sync_bool_compare_and_swap (Object.State'Access, -999, 0) then
         --  writer
         Set (Object.Reader_Barrier);
         Set (Object.Writer_Barrier);
      else
         --  reader
         if sync_sub_and_fetch (Object.State'Access, 1) = 0 then
            Reset (Object.Reader_Barrier);
            Set (Object.Writer_Barrier);
         end if;
      end if;
   end Leave;

   --  for Abortable

   function "-" (Left, Right : Native_Time.Native_Time) return Duration is
   begin
      return Native_Time.To_Time (Left) - Native_Time.To_Time (Right);
   end "-";

end System.Tasking.Synchronous_Objects;
