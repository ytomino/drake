with Ada.Exception_Identification.From_Here;
with System.Debug;
with System.Native_Real_Time;
with System.Native_Time;
with System.Storage_Elements;
with C.errno;
with C.fcntl;
with C.poll;
with C.sys.types;
with C.unistd;
package body System.Synchronous_Objects is
   use Ada.Exception_Identification.From_Here;
   use type Native_Time.Nanosecond_Number;
   use type Storage_Elements.Storage_Offset;
   use type C.signed_int;
   use type C.signed_long;
   use type C.unsigned_short;
   use type C.pthread.pthread_cond_t;
   use type C.pthread.pthread_mutex_t;
   use type C.pthread.pthread_rwlock_t;

   function memcmp (s1, s2 : Address; n : Storage_Elements.Storage_Count)
      return Integer
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_memcmp";
      --  [gcc-5] cannot compare Unchecked_Union

   procedure Set_Close_On_Exec (Handle : C.signed_int);
   procedure Set_Close_On_Exec (Handle : C.signed_int) is
      R : C.signed_int;
   begin
      R := C.fcntl.fcntl (
         Handle,
         C.fcntl.F_SETFD,
         C.fcntl.FD_CLOEXEC);
      pragma Check (Debug,
         Check => R = 0 or else Debug.Runtime_Error ("fcntl failed"));
   end Set_Close_On_Exec;

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
         Check =>
            R = 0
            or else (
               R = C.errno.EINVAL
               and then memcmp (
                  Object.Handle'Address,
                  C.pthread.PTHREAD_MUTEX_INITIALIZER'Address,
                  C.pthread.pthread_mutex_t'Size / Standard'Storage_Unit) = 0)
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
         Check =>
            R = 0
            or else (
               R = C.errno.EINVAL
               and then memcmp (
                  Object.Handle'Address,
                  C.pthread.PTHREAD_COND_INITIALIZER'Address,
                  C.pthread.pthread_cond_t'Size / Standard'Storage_Unit) = 0)
            or else Debug.Runtime_Error ("pthread_cond_destroy failed"));
   end Finalize;

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

   --  queue

   procedure Initialize (
      Object : in out Queue;
      Mutex : not null Mutex_Access) is
   begin
      Object.Mutex := Mutex;
      Initialize (Object.Pipe);
      Object.Head := null;
      Object.Tail := null;
      Object.Filter := null;
      Object.Waiting := False;
      Object.Canceled := False;
   end Initialize;

   procedure Finalize (Object : in out Queue) is
   begin
      Finalize (Object.Pipe);
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
            Object.Filter = null
            or else Object.Filter (Item, Object.Params))
      then
         Set (Object.Pipe); -- append 1 byte
      end if;
   end Notify_All;

   --  event

   procedure Initialize (Object : in out Event) is
      Handles : aliased C.signed_int_array (0 .. 1);
      R : C.signed_int;
   begin
      R := C.unistd.pipe (Handles (0)'Access);
      pragma Check (Debug,
         Check => R = 0 or else Debug.Runtime_Error ("pipe failed"));
      Set_Close_On_Exec (Handles (0));
      Set_Close_On_Exec (Handles (1));
      Object.Reading_Pipe := Handles (0);
      Object.Writing_Pipe := Handles (1);
   end Initialize;

   procedure Finalize (Object : in out Event) is
      R1, R2 : C.signed_int;
   begin
      R1 := C.unistd.close (Object.Reading_Pipe);
      R2 := C.unistd.close (Object.Writing_Pipe);
      Object.Reading_Pipe := 0;
      Object.Writing_Pipe := 0;
      pragma Check (Debug,
         Check =>
            (R1 = 0 and then R2 = 0)
            or else Debug.Runtime_Error ("close failed"));
   end Finalize;

   function Get (Object : Event) return Boolean is
      Polling : aliased C.poll.struct_pollfd;
   begin
      Polling.fd := Object.Reading_Pipe;
      Polling.events :=
         C.signed_short (C.unsigned_short'(C.poll.POLLIN or C.poll.POLLERR));
      loop
         declare
            R : C.signed_int;
         begin
            R := C.poll.poll (Polling'Access, 1, 0);
            if R > 0 then
               pragma Check (Debug,
                  Check =>
                     (C.unsigned_short (Polling.revents)
                        and C.poll.POLLERR) = 0
                     or else Debug.Runtime_Error ("POLLERR"));
               if (C.unsigned_short (Polling.revents)
                  and C.poll.POLLIN) /= 0
               then
                  return True;
               end if;
            elsif R = 0 then -- timeout
               return False;
            end if;
            pragma Check (Debug,
               Check =>
                  R >= 0
                  or else C.errno.errno = C.errno.EINTR
                  or else Debug.Runtime_Error ("poll failed"));
         end;
      end loop;
   end Get;

   procedure Set (Object : in out Event) is
   begin
      Write_1 (Object.Writing_Pipe);
   end Set;

   procedure Reset (Object : in out Event) is
      Polling : aliased C.poll.struct_pollfd;
   begin
      Polling.fd := Object.Reading_Pipe;
      Polling.events :=
         C.signed_short (C.unsigned_short'(C.poll.POLLIN or C.poll.POLLERR));
      loop
         declare
            R : C.signed_int;
         begin
            R := C.poll.poll (Polling'Access, 1, 0);
            if R > 0 then
               pragma Check (Debug,
                  Check =>
                     (C.unsigned_short (Polling.revents)
                        and C.poll.POLLERR) = 0
                     or else Debug.Runtime_Error ("POLLERR"));
               if (C.unsigned_short (Polling.revents)
                  and C.poll.POLLIN) /= 0
               then
                  Read_1 (Object.Reading_Pipe);
               end if;
            elsif R = 0 then -- timeout, the pipe is empty
               exit;
            end if;
            pragma Check (Debug,
               Check =>
                  R >= 0
                  or else C.errno.errno = C.errno.EINTR
                  or else Debug.Runtime_Error ("poll failed"));
         end;
      end loop;
   end Reset;

   procedure Wait (Object : in out Event) is
      Polling : aliased C.poll.struct_pollfd;
   begin
      Polling.fd := Object.Reading_Pipe;
      Polling.events :=
         C.signed_short (C.unsigned_short'(C.poll.POLLIN or C.poll.POLLERR));
      loop
         declare
            R : C.signed_int;
            Value : Boolean;
         begin
            R := C.poll.poll (Polling'Access, 1, -1); -- waiting indefinitely
            if R > 0 then
               pragma Check (Debug,
                  Check =>
                     (C.unsigned_short (Polling.revents)
                        and C.poll.POLLERR) = 0
                     or else Debug.Runtime_Error ("POLLERR"));
               Value := (C.unsigned_short (Polling.revents)
                  and C.poll.POLLIN) /= 0;
               exit when Value;
            end if;
            pragma Check (Debug,
               Check =>
                  R >= 0
                  or else C.errno.errno = C.errno.EINTR
                  or else Debug.Runtime_Error ("poll failed"));
         end;
      end loop;
   end Wait;

   procedure Wait (
      Object : in out Event;
      Timeout : Duration;
      Value : out Boolean)
   is
      Deadline : constant Duration :=
         Native_Real_Time.To_Duration (Native_Real_Time.Clock) + Timeout;
      Span : Duration := Timeout;
      Polling : aliased C.poll.struct_pollfd;
   begin
      Polling.fd := Object.Reading_Pipe;
      Polling.events :=
         C.signed_short (C.unsigned_short'(C.poll.POLLIN or C.poll.POLLERR));
      loop
         declare
            Nanoseconds : constant Native_Time.Nanosecond_Number :=
               Native_Time.Nanosecond_Number'Integer_Value (Span);
            Milliseconds : constant C.signed_int :=
               C.signed_int'Max (C.signed_int (Nanoseconds / 1_000_000), 0);
            R : C.signed_int;
         begin
            R := C.poll.poll (Polling'Access, 1, Milliseconds);
            if R > 0 then
               pragma Check (Debug,
                  Check =>
                     (C.unsigned_short (Polling.revents)
                        and C.poll.POLLERR) = 0
                     or else Debug.Runtime_Error ("POLLERR"));
               Value := (C.unsigned_short (Polling.revents)
                  and C.poll.POLLIN) /= 0;
               exit when Value;
            end if;
            pragma Check (Debug,
               Check =>
                  R >= 0
                  or else C.errno.errno = C.errno.EINTR
                  or else Debug.Runtime_Error ("poll failed"));
         end;
         Span :=
            Deadline - Native_Real_Time.To_Duration (Native_Real_Time.Clock);
         if Span <= 0.0 then -- timeout
            Value := False;
            exit;
         end if;
      end loop;
   end Wait;

   procedure Read_1 (Reading_Pipe : C.signed_int) is
   begin
      loop
         declare
            Item : aliased C.char;
            Read_Size : C.sys.types.ssize_t;
         begin
            Read_Size :=
               C.unistd.read (Reading_Pipe, C.void_ptr (Item'Address), 1);
            exit when Read_Size > 0;
            pragma Check (Debug,
               Check =>
                  Read_Size >= 0
                  or else C.errno.errno = C.errno.EINTR
                  or else Debug.Runtime_Error ("read failed"));
         end;
      end loop;
   end Read_1;

   procedure Write_1 (Writing_Pipe : C.signed_int) is
      Item : aliased constant C.char := C.char'Val (16#1A#);
   begin
      loop
         declare
            Written_Size : C.sys.types.ssize_t;
         begin
            Written_Size :=
               C.unistd.write (
                  Writing_Pipe,
                  C.void_const_ptr (Item'Address),
                  1);
            exit when Written_Size > 0;
            pragma Check (Debug,
               Check =>
                  Written_Size >= 0
                  or else C.errno.errno = C.errno.EINTR
                  or else Debug.Runtime_Error ("write failed"));
         end;
      end loop;
   end Write_1;

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
         Check =>
            R = 0
            or else (
               R = C.errno.EINVAL
               and then memcmp (
                  Object.Handle'Address,
                  C.pthread.PTHREAD_RWLOCK_INITIALIZER'Address,
                  C.pthread.pthread_rwlock_t'Size / Standard'Storage_Unit) = 0)
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
