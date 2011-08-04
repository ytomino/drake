package body Ada.Synchronous_Barriers is

   procedure Wait_For_Release (
      The_Barrier : in out Synchronous_Barrier;
      Notified : out Boolean) is
   begin
      System.Tasking.Inside.Wait (The_Barrier.Object, Notified);
   end Wait_For_Release;

   overriding procedure Initialize (Object : in out Synchronous_Barrier) is
   begin
      System.Tasking.Inside.Initialize (
         Object.Object,
         Object.Release_Threshold);
   end Initialize;

   overriding procedure Finalize (Object : in out Synchronous_Barrier) is
   begin
      System.Tasking.Inside.Finalize (Object.Object);
   end Finalize;

end Ada.Synchronous_Barriers;
