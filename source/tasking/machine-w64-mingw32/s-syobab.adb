with Ada.Exception_Identification.From_Here;
with System.Tasks;
with C.winbase;
with C.windef;
with C.winerror;
package body System.Synchronous_Objects.Abortable is
   use Ada.Exception_Identification.From_Here;
   use type C.windef.DWORD;

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
                  Wait (
                     Object.Event,
                     Aborted => Aborted);
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
            Handles : aliased array (0 .. 1) of aliased C.winnt.HANDLE :=
               (Object.Handle, Abort_Event.Handle);
         begin
            case C.winbase.WaitForMultipleObjects (
               2,
               Handles (0)'Access,
               0,
               C.winbase.INFINITE)
            is
               when C.winbase.WAIT_OBJECT_0 | C.winbase.WAIT_OBJECT_0 + 1 =>
                  null;
               when others =>
                  Raise_Exception (Tasking_Error'Identity);
            end case;
         end;
      else
         Wait (Object);
      end if;
      Aborted := Tasks.Is_Aborted;
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
            Handles : aliased array (0 .. 1) of aliased C.winnt.HANDLE :=
               (Object.Handle, Abort_Event.Handle);
            Milliseconds : constant C.windef.DWORD :=
               C.windef.DWORD (Timeout * 1_000);
         begin
            case C.winbase.WaitForMultipleObjects (
               2,
               Handles (0)'Access,
               0,
               Milliseconds)
            is
               when C.winbase.WAIT_OBJECT_0 =>
                  Value := True;
                  Aborted := Tasks.Is_Aborted;
               when C.winbase.WAIT_OBJECT_0 + 1 | C.winerror.WAIT_TIMEOUT =>
                  Value := Get (Object);
                  Aborted := True;
               when others =>
                  Raise_Exception (Tasking_Error'Identity);
            end case;
         end;
      else
         Wait (Object, Timeout, Value);
         Aborted := Tasks.Is_Aborted;
      end if;
   end Wait;

end System.Synchronous_Objects.Abortable;
