pragma License (Unrestricted);
with Ada.Task_Identification;
with System.Multiprocessors;
package Ada.Execution_Time.Group_Budgets is

   type Group_Budget (
      CPU : System.Multiprocessors.CPU := System.Multiprocessors.CPU'First) is
      tagged limited private;

   type Group_Budget_Handler is
      access protected procedure (GB : in out Group_Budget);

   type Task_Array is array (Positive range <>) of Task_Identification.Task_Id;

--  Min_Handler_Ceiling : constant System.Any_Priority :=
--    implementation-defined;

--  procedure Add_Task (
--    GB : in out Group_Budget;
--    T  : Task_Identification.Task_Id);
--  procedure Remove_Task (
--    GB : in out Group_Budget;
--    T : Task_Identification.Task_Id);
--  function Is_Member (
--    GB : Group_Budget;
--    T : Task_Identification.Task_Id)
--    return Boolean;
--  function Is_A_Group_Member (T : Task_Identification.Task_Id)
--    return Boolean;
--  function Members (GB : Group_Budget) return Task_Array;

--  procedure Replenish (GB : in out Group_Budget; To : Real_Time.Time_Span);
--  procedure Add (GB : in out Group_Budget; Interval : Real_Time.Time_Span);
--  function Budget_Has_Expired (GB : Group_Budget) return Boolean;
--  function Budget_Remaining (GB : Group_Budget) return Real_Time.Time_Span;

--  procedure Set_Handler (
--    GB : in out Group_Budget;
--    Handler : Group_Budget_Handler);
--  function Current_Handler (GB : Group_Budget) return Group_Budget_Handler;
--  procedure Cancel_Handler (
--    GB : in out Group_Budget;
--    Cancelled : out Boolean);

   Group_Budget_Error : exception;

private

   type Group_Budget (
      CPU : System.Multiprocessors.CPU := System.Multiprocessors.CPU'First) is
      tagged limited null record;

end Ada.Execution_Time.Group_Budgets;
