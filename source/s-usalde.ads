pragma License (Unrestricted);
--  implementation unit
package System.Unbounded_Stack_Allocators.Debug is
   pragma Preelaborate;

   --  output address without secondary stack
   procedure Error_Put (Item : Address);

   --  dump the secondary stack of current task
   procedure Dump (Allocator : aliased in out Allocator_Type);

end System.Unbounded_Stack_Allocators.Debug;
