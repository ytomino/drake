pragma License (Unrestricted);
--  implementation unit required by compiler
with Ada.Exceptions;
limited with System.Tasking.Protected_Objects.Entries;
package System.Tasking.Rendezvous is

   --  required for accept statement by compiler (s-tasren.ads)
   procedure Accept_Call (
      E : Task_Entry_Index;
      Uninterpreted_Data : out Address);

   --  required optional for accept statement by compiler (s-tasren.ads)
   procedure Complete_Rendezvous;

   --  required for accept statement by compiler (s-tasren.ads)
   procedure Exceptional_Complete_Rendezvous (
      Ex : Ada.Exceptions.Exception_Id);

   --  required for simple accept statement by compiler (s-tasren.ads)
   procedure Accept_Trivial (E : Task_Entry_Index);

   --  by -fdump-tree-all, accept statement expanded below:
   --
   --  A19b = system.tasking.rendezvous.accept_call (1);
   --  const arg2_t arg2 = *((struct xTK__P3b *) A19b)->arg2;
   --  const arg1_t arg1 = *((struct xTK__P3b *) A19b)->arg1;
   --  try
   --    {
   --      ... user code ...
   --      system.tasking.rendezvous.complete_rendezvous ();
   --    }
   --  catch
   --    {
   --      catch (&ALL_OTHERS)
   --        {
   --          void * EXPTR = .builtin_eh_pointer (0);
   --          try
   --            {
   --              void * EXPTR = .builtin_eh_pointer (0);
   --              .gnat_begin_handler (EXPTR);
   --              system.tasking.rendezvous.exceptional_complete_rendezvous (
   --                (struct system__standard_library__exception_data * const)
   --                  system.soft_links.get_gnat_exception ());
   --            }
   --          finally
   --            {
   --              .gnat_end_handler (EXPTR);
   --            }
   --        }
   --    }

   --  required for synchronized interface by compiler (s-tasren.ads)
   procedure Task_Entry_Call (
      Acceptor : Task_Id;
      E : Task_Entry_Index;
      Uninterpreted_Data : Address;
      Mode : Call_Modes;
      Rendezvous_Successful : out Boolean);

   --  required for synchronized interface by compiler (s-tasren.ads)
   procedure Timed_Task_Entry_Call (
      Acceptor : Task_Id;
      E : Task_Entry_Index;
      Uninterpreted_Data : System.Address;
      Timeout : Duration;
      Mode : Integer; -- Tasking.Delay_Modes;
      Rendezvous_Successful : out Boolean);

   --  required for calling entry of task by compiler (s-tasren.ads)
   procedure Call_Simple (
      Acceptor : Task_Id;
      E : Task_Entry_Index;
      Uninterpreted_Data : Address);

   --  by -fdump-tree-all, calling entry of task expanded below:
   --
   --  struct xTK__P3b P = {.arg1=&arg1, .arg2=&arg2};
   --  system.tasking.rendezvous.call_simple (
   --    (struct system__tasking__ada_task_control_block * const {ref-all})
   --      _init._task_id,
   --    entry_index,
   --    (const system__address) &P);

   --  required for synchronized interface by compiler (s-tasren.ads)
   procedure Requeue_Task_Entry (
      Acceptor : Task_Id;
      E : Task_Entry_Index;
      With_Abort : Boolean);

   --  required for synchronized interface by compiler (s-tasren.ads)
   procedure Requeue_Protected_To_Task_Entry (
      Object : not null access Entries.Protection_Entries'Class;
      Acceptor : Task_Id;
      E : Task_Entry_Index;
      With_Abort : Boolean);

   --  required for 'Callable by compiler (s-tasren.ads)
   function Callable (T : Task_Id) return Boolean;

   --  required for 'Count by compiler (s-tasren.ads)
   function Task_Count (E : Task_Entry_Index) return Natural;

   --  unimplemented subprograms required by compiler
   --  Cancel_Task_Entry_Call
   --  Selective_Wait
   --  Task_Entry_Caller
   --  Timed_Selective_Wait

end System.Tasking.Rendezvous;
