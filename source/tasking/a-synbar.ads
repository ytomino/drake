pragma License (Unrestricted);
--  Ada 2012
private with Ada.Finalization;
private with System.Synchronous_Objects;
package Ada.Synchronous_Barriers is
   pragma Preelaborate;

   subtype Barrier_Limit is
      Positive range 1 .. Natural'Last; -- implementation-defined

   type Synchronous_Barrier (Release_Threshold : Barrier_Limit) is
      limited private;

   procedure Wait_For_Release (
      The_Barrier : in out Synchronous_Barrier;
      Notified : out Boolean);

private

   type Synchronous_Barrier (Release_Threshold : Barrier_Limit) is
      limited new Finalization.Limited_Controlled with
   record
      Object : System.Synchronous_Objects.Barrier;
   end record;

   overriding procedure Initialize (Object : in out Synchronous_Barrier);
   overriding procedure Finalize (Object : in out Synchronous_Barrier);

end Ada.Synchronous_Barriers;
