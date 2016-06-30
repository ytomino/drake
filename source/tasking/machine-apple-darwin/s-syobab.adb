with System.Native_Real_Time;
with System.Native_Time;
with System.Debug;
with System.Tasks;
with C.errno;
with C.poll;
package body System.Synchronous_Objects.Abortable is
   use type Native_Time.Nanosecond_Number;
   use type C.signed_int;
   use type C.unsigned_short;

   type struct_pollfd_array is
      array (C.size_t range <>) of aliased C.poll.struct_pollfd
      with Convention => C;
   pragma Suppress_Initialization (struct_pollfd_array);

   --  queue

   procedure Take (
      Object : in out Queue;
      Item : out Queue_Node_Access;
      Params : Address;
      Filter : Queue_Filter;
      Aborted : out Boolean) is
   begin
      Aborted := Tasks.Is_Aborted;
      Enter (Object.Mutex.all);
      declare
         Previous : Queue_Node_Access := null;
         I : Queue_Node_Access := Object.Head;
      begin
         Taking : loop
            Take_No_Sync (Object, Item, Params, Filter, Previous, I);
            exit Taking when Item /= null;
            Not_Found : declare
               Tail_On_Waiting : constant Queue_Node_Access := Object.Tail;
            begin
               Object.Params := Params;
               Object.Filter := Filter;
               loop
                  Object.Waiting := True;
                  Leave (Object.Mutex.all);
                  Wait (Object.Pipe, Aborted => Aborted);
                  if not Aborted then
                     Read_1 (Object.Pipe.Reading_Pipe);
                  end if;
                  Enter (Object.Mutex.all);
                  Object.Waiting := False;
                  exit Taking when Aborted;
                  exit when Object.Tail /= Tail_On_Waiting;
               end loop;
               if Tail_On_Waiting /= null then
                  Previous := Tail_On_Waiting;
                  I := Tail_On_Waiting.Next;
               else
                  Previous := null;
                  I := Object.Head;
               end if;
            end Not_Found;
         end loop Taking;
      end;
      Leave (Object.Mutex.all);
   end Take;

   --  event

   procedure Wait (
      Object : in out Event;
      Aborted : out Boolean)
   is
      Abort_Event : constant access Event := Tasks.Abort_Event;
   begin
      if Abort_Event /= null then
         declare
            Polling : aliased struct_pollfd_array (0 .. 1);
         begin
            Polling (0).fd := Object.Reading_Pipe;
            Polling (0).events :=
               C.signed_short (
                  C.unsigned_short'(C.poll.POLLIN or C.poll.POLLERR));
            Polling (1).fd := Abort_Event.Reading_Pipe;
            Polling (1).events :=
               C.signed_short (
                  C.unsigned_short'(C.poll.POLLIN or C.poll.POLLERR));
            loop
               declare
                  R : C.signed_int;
                  Value : Boolean;
               begin
                  R := C.poll.poll (
                     Polling (0)'Access,
                     2,
                     -1); -- waiting indefinitely
                  if R > 0 then
                     pragma Check (Debug,
                        Check =>
                           ((C.unsigned_short (Polling (0).revents)
                              and C.poll.POLLERR) = 0
                           and then (C.unsigned_short (Polling (0).revents)
                              and C.poll.POLLERR) = 0)
                           or else Debug.Runtime_Error ("POLLERR"));
                     Value :=
                        (C.unsigned_short (Polling (0).revents)
                           and C.poll.POLLIN) /= 0;
                     Aborted :=
                        (C.unsigned_short (Polling (1).revents)
                           and C.poll.POLLIN) /= 0
                        or else Tasks.Is_Aborted;
                     exit when Value or else Aborted;
                  else -- timeout
                     Aborted := Tasks.Is_Aborted;
                     exit;
                  end if;
                  pragma Check (Debug,
                     Check =>
                        not (R < 0)
                        or else C.errno.errno = C.errno.EINTR
                        or else Debug.Runtime_Error ("poll failed"));
               end;
            end loop;
         end;
      else
         Wait (Object);
         Aborted := Tasks.Is_Aborted;
      end if;
   end Wait;

   procedure Wait (
      Object : in out Event;
      Timeout : Duration;
      Value : out Boolean;
      Aborted : out Boolean)
   is
      Abort_Event : constant access Event := Tasks.Abort_Event;
   begin
      if Abort_Event /= null then
         declare
            Deadline : constant Duration :=
               Native_Real_Time.To_Duration (Native_Real_Time.Clock) + Timeout;
            Span : Duration := Timeout;
            Polling : aliased struct_pollfd_array (0 .. 1);
         begin
            Polling (0).fd := Object.Reading_Pipe;
            Polling (0).events :=
               C.signed_short (
                  C.unsigned_short'(C.poll.POLLIN or C.poll.POLLERR));
            Polling (1).fd := Abort_Event.Reading_Pipe;
            Polling (1).events :=
               C.signed_short (
                  C.unsigned_short'(C.poll.POLLIN or C.poll.POLLERR));
            loop
               declare
                  Nanoseconds : constant Native_Time.Nanosecond_Number :=
                     Native_Time.Nanosecond_Number'Integer_Value (Span);
                  Milliseconds : constant C.signed_int :=
                     C.signed_int'Max (
                        C.signed_int (Nanoseconds / 1_000_000),
                        0);
                  R : C.signed_int;
               begin
                  R := C.poll.poll (Polling (0)'Access, 2, Milliseconds);
                  if R > 0 then
                     pragma Check (Debug,
                        Check =>
                           ((C.unsigned_short (Polling (0).revents)
                                 and C.poll.POLLERR) = 0
                              and then (C.unsigned_short (Polling (1).revents)
                                 and C.poll.POLLERR) = 0)
                           or else Debug.Runtime_Error ("POLLERR"));
                     Value :=
                        (C.unsigned_short (Polling (0).revents)
                           and C.poll.POLLIN) /= 0;
                     Aborted :=
                        (C.unsigned_short (Polling (1).revents)
                           and C.poll.POLLIN) /= 0
                        or else Tasks.Is_Aborted;
                     exit when Value or else Aborted;
                  end if;
                  pragma Check (Debug,
                     Check =>
                        not (R < 0)
                        or else C.errno.errno = C.errno.EINTR
                        or else Debug.Runtime_Error ("poll failed"));
               end;
               Span :=
                  Deadline
                  - Native_Real_Time.To_Duration (Native_Real_Time.Clock);
               if Span <= 0.0 then -- timeout
                  Value := False;
                  Aborted := Tasks.Is_Aborted;
                  exit;
               end if;
            end loop;
         end;
      else
         Wait (Object, Timeout, Value);
         Aborted := Tasks.Is_Aborted;
      end if;
   end Wait;

end System.Synchronous_Objects.Abortable;
