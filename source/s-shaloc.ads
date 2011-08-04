pragma License (Unrestricted);
--  implementation package
package System.Shared_Locking is
   pragma Preelaborate;

   --  no-operation
   procedure Nop is null;

   type Enter_Handler is access procedure;

   Enter_Hook : not null Enter_Handler := Nop'Access;
   pragma Suppress (Access_Check, Enter_Hook);

   procedure Enter;
   pragma Inline (Enter);

   type Leave_Handler is access procedure;

   Leave_Hook : not null Leave_Handler := Nop'Access;
   pragma Suppress (Access_Check, Leave_Hook);

   procedure Leave;
   pragma Inline (Leave);

end System.Shared_Locking;
