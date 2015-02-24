pragma License (Unrestricted);
with Ada.Real_Time;
--  with Ada.Task_Identification;
package Ada.Dispatching.EDF is

   subtype Deadline is Real_Time.Time;

   Default_Deadline : constant Deadline := Real_Time.Time_Last;

--  procedure Set_Deadline (
--    D : Deadline;
--    T : Task_Identification.Task_Id := Task_Identification.Current_Task);
--  procedure Delay_Until_And_Set_Deadline (
--    Delay_Until_Time : Real_Time.Time;
--    Deadline_Offset : Real_Time.Time_Span);
--  function Get_Deadline (
--    T : Task_Identification.Task_Id := Task_Identification.Current_Task)
--    return Deadline;

end Ada.Dispatching.EDF;
