pragma License (Unrestricted);
--  implementation unit specialized for Windows
package System.Synchronous_Objects.Abortable is
   pragma Preelaborate;

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
