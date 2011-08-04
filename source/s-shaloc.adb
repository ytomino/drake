package body System.Shared_Locking is

   procedure Enter is
   begin
      Enter_Hook.all;
   end Enter;

   procedure Leave is
   begin
      Leave_Hook.all;
   end Leave;

end System.Shared_Locking;
