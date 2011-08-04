pragma License (Unrestricted);
--  implementation package
package System.Once is
   pragma Preelaborate;

   --  no-operation
   procedure Nop is null;

   type Yield_Handler is access procedure;

   Yield_Hook : not null Yield_Handler := Nop'Access;
   pragma Suppress (Access_Check, Yield_Hook);

   type Flag is mod 2 ** 8; -- it should be initialized to zero
   for Flag'Size use 8;
   pragma Atomic (Flag);

   procedure Initialize (
      Flag : not null access Once.Flag;
      Process : not null access procedure);

end System.Once;
