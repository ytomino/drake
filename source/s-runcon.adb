package body System.Runtime_Context is

   --  I hope it will be zero-initialized...
   Main_Task_Local_Storage : aliased Task_Local_Storage;

   --  implementation

   function Get_Main_Task_Local_Storage
      return not null Task_Local_Storage_Access is
   begin
      return Main_Task_Local_Storage'Access;
   end Get_Main_Task_Local_Storage;

   function Get_Task_Local_Storage
      return not null Task_Local_Storage_Access is
   begin
      return Get_Task_Local_Storage_Hook.all;
   end Get_Task_Local_Storage;

end System.Runtime_Context;
