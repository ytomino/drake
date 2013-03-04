with System.Tasking.Inside;
with System.Tasking.Synchronous_Objects.Abortable;
package body Ada.Synchronous_Barriers is
   pragma Suppress (All_Checks);

   procedure Wait_For_Release (
      The_Barrier : in out Synchronous_Barrier;
      Notified : out Boolean)
   is
      Aborted : Boolean;
   begin
      System.Tasking.Inside.Enable_Abort;
      System.Tasking.Synchronous_Objects.Abortable.Wait (
         The_Barrier.Object,
         Notified,
         Aborted => Aborted);
      System.Tasking.Inside.Disable_Abort (Aborted);
   end Wait_For_Release;

   overriding procedure Initialize (Object : in out Synchronous_Barrier) is
   begin
      System.Tasking.Synchronous_Objects.Initialize (
         Object.Object,
         Object.Release_Threshold);
   end Initialize;

   overriding procedure Finalize (Object : in out Synchronous_Barrier) is
   begin
      System.Tasking.Synchronous_Objects.Finalize (Object.Object);
   end Finalize;

end Ada.Synchronous_Barriers;
