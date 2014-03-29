pragma License (Unrestricted);
--  implementation unit
package System.Once is
   pragma Preelaborate;

   type Flag is mod 2 ** 8; -- it should be initialized to zero
   for Flag'Size use 8;
   pragma Atomic (Flag);

   procedure Initialize (
      Flag : not null access Once.Flag;
      Process : not null access procedure);

end System.Once;
