pragma License (Unrestricted);
--  runtime unit
package System.Shared_Locking is
   pragma Preelaborate;

   --  no-operation
   procedure Nop is null;

   type Enter_Handler is access procedure;
   pragma Suppress (Access_Check, Enter_Handler);

   Enter_Hook : Enter_Handler := Nop'Access;
   pragma Suppress (Access_Check, Enter_Hook); -- not null

   procedure Enter;
   pragma Inline (Enter);

   type Leave_Handler is access procedure;
   pragma Suppress (Access_Check, Leave_Handler);

   Leave_Hook : Leave_Handler := Nop'Access;
   pragma Suppress (Access_Check, Leave_Hook); -- not null

   procedure Leave;
   pragma Inline (Leave);

end System.Shared_Locking;
