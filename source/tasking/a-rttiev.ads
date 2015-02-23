pragma License (Unrestricted);
package Ada.Real_Time.Timing_Events is

   type Timing_Event is tagged limited private;
   type Timing_Event_Handler is
      access protected procedure (Event : in out Timing_Event);

--  procedure Set_Handler (
--    Event   : in out Timing_Event;
--    At_Time : Time;
--    Handler : Timing_Event_Handler);
--  procedure Set_Handler (
--    Event   : in out Timing_Event;
--    In_Time : Time_Span;
--    Handler : Timing_Event_Handler);
--  function Current_Handler (Event : Timing_Event)
--    return Timing_Event_Handler;
--  procedure Cancel_Handler (
--    Event : in out Timing_Event;
--    Cancelled : out Boolean);

--  function Time_Of_Event (Event : Timing_Event) return Time;

private

   type Timing_Event is tagged limited null record;

end Ada.Real_Time.Timing_Events;
