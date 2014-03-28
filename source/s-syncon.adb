package body System.Synchronous_Control is
   pragma Suppress (All_Checks);

   procedure Unlock_Abort is
   begin
      Unlock_Abort_Hook.all;
   end Unlock_Abort;

   procedure Lock_Abort is
   begin
      Lock_Abort_Hook.all;
   end Lock_Abort;

end System.Synchronous_Control;
