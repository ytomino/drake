pragma License (Unrestricted);
--  implementation unit
package System.Secondary_Stack.Debug is
   pragma Preelaborate;

   --  output address without secondary stack
   procedure Error_Put (Item : Address);

   --  dump the secondary stack of current task
   procedure Dump;

end System.Secondary_Stack.Debug;
