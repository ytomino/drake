pragma License (Unrestricted);
--  implementation unit specialized for Windows
package System.Synchronous_Objects.Abortable is
   pragma Preelaborate;

   --  condition variable

--  procedure Wait (
--    Object : in out Condition_Variable;
--    Mutex : in out Synchronous_Objects.Mutex;
--    Timeout : Duration;
--    Notified : out Boolean;
--    Aborted : out Boolean);

   --  queue

   procedure Take (
      Object : in out Queue;
      Item : out Queue_Node_Access;
      Params : Address;
      Filter : Queue_Filter;
      Aborted : out Boolean);
      --  waiting

   --  event

   procedure Wait (
      Object : in out Event;
      Aborted : out Boolean);
   procedure Wait (
      Object : in out Event;
      Timeout : Duration;
      Value : out Boolean;
      Aborted : out Boolean);

end System.Synchronous_Objects.Abortable;
