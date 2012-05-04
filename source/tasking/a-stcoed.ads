pragma License (Unrestricted);
--  Ada 2012
with Ada.Real_Time;
package Ada.Synchronous_Task_Control.EDF is

   procedure Suspend_Until_True_And_Set_Deadline (
      S : in out Suspension_Object;
      TS : Real_Time.Time_Span);
   --  extended
   --  If State is True, Set_True is called, otherwise it's timeout.
   procedure Suspend_Until_True_And_Set_Deadline (
      S : in out Suspension_Object;
      TS : Real_Time.Time_Span;
      State : out Boolean);

end Ada.Synchronous_Task_Control.EDF;
