pragma License (Unrestricted);
--  implementation unit
package System.Shared_Locking is
   pragma Preelaborate;

   --  no-operation
   procedure Nop is null;

   type Enter_Handler is access procedure;
   pragma Favor_Top_Level (Enter_Handler);

   Enter_Hook : not null Enter_Handler := Nop'Access;

   procedure Enter;

   type Leave_Handler is access procedure;
   pragma Favor_Top_Level (Leave_Handler);

   Leave_Hook : not null Leave_Handler := Nop'Access;

   procedure Leave;

end System.Shared_Locking;
