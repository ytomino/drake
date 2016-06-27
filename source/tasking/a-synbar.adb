with System.Synchronous_Objects.Abortable;
with System.Tasks;
package body Ada.Synchronous_Barriers is

   procedure Do_Wait (
      Object : in out Synchronous_Barrier;
      Notified : out Boolean;
      Aborted : out Boolean);
   procedure Do_Wait (
      Object : in out Synchronous_Barrier;
      Notified : out Boolean;
      Aborted : out Boolean)
   is
      Order : Natural;
   begin
      System.Synchronous_Objects.Enter (Object.Mutex);
      Object.Blocked := Object.Blocked + 1;
      Order := Object.Blocked rem Object.Release_Threshold;
      Notified := Order = 1;
      if Order = 0 then
         System.Synchronous_Objects.Set (Object.Event);
         Aborted := System.Tasks.Is_Aborted;
         Object.Unblocked := Object.Unblocked + 1;
      else
         loop
            System.Synchronous_Objects.Leave (Object.Mutex);
            System.Synchronous_Objects.Abortable.Wait (Object.Event, Aborted);
            System.Synchronous_Objects.Enter (Object.Mutex);
            exit when Object.Blocked >= Object.Release_Threshold
               or else Aborted;
         end loop;
         Object.Unblocked := Object.Unblocked + 1;
      end if;
      if Object.Unblocked = Object.Release_Threshold then
         Object.Blocked := Object.Blocked - Object.Release_Threshold;
         Object.Unblocked := 0;
         if Object.Blocked < Object.Release_Threshold then
            System.Synchronous_Objects.Reset (Object.Event);
         end if;
      end if;
      System.Synchronous_Objects.Leave (Object.Mutex);
   end Do_Wait;

   --  implementation

   procedure Wait_For_Release (
      The_Barrier : in out Synchronous_Barrier;
      Notified : out Boolean)
   is
      Aborted : Boolean;
   begin
      System.Tasks.Enable_Abort;
      Do_Wait (The_Barrier, Notified, Aborted => Aborted);
      System.Tasks.Disable_Abort (Aborted);
   end Wait_For_Release;

   overriding procedure Initialize (Object : in out Synchronous_Barrier) is
   begin
      Object.Blocked := 0;
      Object.Unblocked := 0;
      System.Synchronous_Objects.Initialize (Object.Mutex);
      System.Synchronous_Objects.Initialize (Object.Event);
   end Initialize;

   overriding procedure Finalize (Object : in out Synchronous_Barrier) is
   begin
      System.Synchronous_Objects.Finalize (Object.Mutex);
      System.Synchronous_Objects.Finalize (Object.Event);
   end Finalize;

end Ada.Synchronous_Barriers;
