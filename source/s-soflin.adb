with Ada.Unchecked_Conversion;
package body System.Soft_Links is
   pragma Suppress (All_Checks);

   --  I hope it will be zero-initialized...
   Main_Task_Local_Storage : aliased Task_Local_Storage;

   function Get_Main_Current_Excep
      return Ada.Exceptions.Exception_Occurrence_Access
   is
      function Cast is new Ada.Unchecked_Conversion (
         Unwind.Exception_Occurrence_Access,
         Ada.Exceptions.Exception_Occurrence_Access);
   begin
      return Cast (Main_Task_Local_Storage.Current_Exception'Access);
   end Get_Main_Current_Excep;

   function Get_Main_Task_Local_Storage
      return not null Task_Local_Storage_Access is
   begin
      return Main_Task_Local_Storage'Access;
   end Get_Main_Task_Local_Storage;

   function Zero return Integer is
   begin
      return 0;
   end Zero;

   procedure Abort_Undefer_Direct is
   begin
      Abort_Undefer.all;
   end Abort_Undefer_Direct;

end System.Soft_Links;
