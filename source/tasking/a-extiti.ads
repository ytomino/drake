pragma License (Unrestricted);
with Ada.Task_Identification;
--  with System;
package Ada.Execution_Time.Timers is

   type Timer (T : not null access constant Task_Identification.Task_Id) is
      tagged limited private;

   type Timer_Handler is access protected procedure (TM : in out Timer);

--  Min_Handler_Ceiling : constant System.Any_Priority :=
--    implementation-defined;

--  procedure Set_Handler (
--    TM : in out Timer;
--    In_Time : Real_Time.Time_Span;
--    Handler : Timer_Handler);
--  procedure Set_Handler (
--    TM : in out Timer;
--    At_Time : CPU_Time;
--    Handler : Timer_Handler);
--  function Current_Handler (TM : Timer) return Timer_Handler;
--  procedure Cancel_Handler (TM : in out Timer; Cancelled : out Boolean);

--  function Time_Remaining (TM : Timer) return Real_Time.Time_Span;

   Timer_Resource_Error : exception;

private

   type Timer (T : not null access constant Task_Identification.Task_Id) is
      tagged limited null record;

end Ada.Execution_Time.Timers;
