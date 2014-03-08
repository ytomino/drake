package body System.Interrupts is

   procedure Install_Handlers (
      Object : not null access Static_Interrupt_Protection;
      New_Handlers : New_Handler_Array)
   is
      pragma Unreferenced (Object);
   begin
      for I in New_Handlers'Range loop
         if Ada.Interrupts.Is_Reserved (New_Handlers (I).Interrupt) then
            raise Program_Error; -- CXC3002
         end if;
         Ada.Interrupts.Unchecked_Attach_Handler (
            New_Handlers (I).Handler,
            New_Handlers (I).Interrupt);
      end loop;
   end Install_Handlers;

end System.Interrupts;
