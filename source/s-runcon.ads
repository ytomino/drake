pragma License (Unrestricted);
--  runtime unit
with System.Unwind.Representation;
package System.Runtime_Context is
   pragma Preelaborate;

   --  equivalent to TSD (s-soflin.ads)
   type Task_Local_Storage is record
      Secondary_Stack : aliased Address;
      Overlaid_Allocation : Address; -- for System.Storage_Pools.Overlaps
      SEH : Address; -- Win32 only
      Machine_Occurrence : Unwind.Representation.Machine_Occurrence_Access;
      Secondary_Occurrence : aliased Unwind.Representation.Machine_Occurrence;
      Triggered_By_Abort : Boolean;
      No_Discrete_Value_Failure_Propagation : Boolean;
      Discrete_Value_Failure : Boolean;
   end record;
   pragma Suppress_Initialization (Task_Local_Storage);
   type Task_Local_Storage_Access is access all Task_Local_Storage;
   for Task_Local_Storage_Access'Storage_Size use 0;

   function Get_Environment_Task_Local_Storage
      return not null Task_Local_Storage_Access;

   type Get_Task_Local_Storage_Handler is
      access function return not null Task_Local_Storage_Access;
   pragma Favor_Top_Level (Get_Task_Local_Storage_Handler);

   Get_Task_Local_Storage_Hook : not null Get_Task_Local_Storage_Handler :=
      Get_Environment_Task_Local_Storage'Access;

   function Get_Task_Local_Storage
      return not null Task_Local_Storage_Access;

end System.Runtime_Context;
