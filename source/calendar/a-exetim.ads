pragma License (Unrestricted);
--  with Ada.Task_Identification;
with Ada.Real_Time; -- use Ada.Real_Time;
package Ada.Execution_Time is

   type CPU_Time is private;
   CPU_Time_First : constant CPU_Time;
   CPU_Time_Last : constant CPU_Time;
   CPU_Time_Unit : constant :=
      Duration'Delta; -- implementation-defined-real-number
--  CPU_Tick : constant Time_Span;

--  function Clock (
--    T : Task_Identification.Task_Id := Task_Identification.Current_Task)
--    return CPU_Time;
   function Clock return CPU_Time; -- substitution
   pragma Inline (Clock); -- renamed

--  function "+" (Left : CPU_Time; Right : Time_Span) return CPU_Time;
--  function "+" (Left : Time_Span; Right : CPU_Time) return CPU_Time;
--  function "-" (Left : CPU_Time; Right : Time_Span) return CPU_Time;
   function "-" (Left : CPU_Time; Right : CPU_Time) return Real_Time.Time_Span
      with Pure_Function, Import, Convention => Intrinsic;

--  function "<" (Left, Right : CPU_Time) return Boolean;
--  function "<=" (Left, Right : CPU_Time) return Boolean;
--  function ">" (Left, Right : CPU_Time) return Boolean;
--  function ">=" (Left, Right : CPU_Time) return Boolean;

--  procedure Split (T : CPU_Time; SC : out Seconds_Count; TS : out Time_Span);

--  function Time_Of (SC : Seconds_Count; TS : Time_Span := Time_Span_Zero)
--    return CPU_Time;

   Interrupt_Clocks_Supported : constant Boolean :=
      False; -- implementation-defined

   Separate_Interrupt_Clocks_Supported : constant Boolean :=
      False; -- implementation-defined

--  function Clock_For_Interrupts return CPU_Time;

private

   type CPU_Time is new Real_Time.Time;
   CPU_Time_First : constant CPU_Time := CPU_Time (Real_Time.Time_First);
   CPU_Time_Last : constant CPU_Time := CPU_Time (Real_Time.Time_Last);

end Ada.Execution_Time;
