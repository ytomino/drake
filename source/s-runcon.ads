pragma License (Unrestricted);
--  runtime unit
with System.Unwind;
package System.Runtime_Context is
   pragma Preelaborate;

   --  equivalent to TSD (s-soflin.ads)
   type Task_Local_Storage is record
      Secondary_Stack : aliased Address;
      Overlaid_Allocation : Address; -- for System.Storage_Pools.Overlaps
      Current_Exception : aliased Unwind.Exception_Occurrence;
   end record;
   pragma Suppress_Initialization (Task_Local_Storage);
   type Task_Local_Storage_Access is access all Task_Local_Storage;
   for Task_Local_Storage_Access'Storage_Size use 0;

   function Get_Main_Task_Local_Storage
      return not null Task_Local_Storage_Access;

   type Get_Task_Local_Storage_Handler is
      access function return not null Task_Local_Storage_Access;
   pragma Suppress (Access_Check, Get_Task_Local_Storage_Handler);

   Get_Task_Local_Storage_Hook : Get_Task_Local_Storage_Handler :=
      Get_Main_Task_Local_Storage'Access;
   pragma Suppress (Access_Check, Get_Task_Local_Storage_Hook); -- not null

   function Get_Task_Local_Storage
      return not null Task_Local_Storage_Access;

end System.Runtime_Context;
