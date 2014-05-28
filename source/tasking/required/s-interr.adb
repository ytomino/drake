package body System.Interrupts is

   procedure Install_Handlers (
      Object : not null access Static_Interrupt_Protection;
      New_Handlers : New_Handler_Array) is
   begin
      for I in New_Handlers'Range loop
         if Ada.Interrupts.Is_Reserved (New_Handlers (I).Interrupt) then
            raise Program_Error; -- CXC3002
         end if;
         declare
            Previous_Item : Previous_Handler_Item
               renames Object.Previous_Handlers (
                  I + Object.Previous_Handlers'First - New_Handlers'First);
            New_Item : New_Handler_Item
               renames New_Handlers (I);
         begin
            Previous_Item.Interrupt := New_Item.Interrupt;
            Interrupt_Handlers.Set_Static_Handler (
               New_Item.Handler);
            Ada.Interrupts.Unchecked_Exchange_Handler (
               Previous_Item.Handler,
               New_Item.Handler,
               New_Item.Interrupt);
         end;
      end loop;
   end Install_Handlers;

   overriding procedure Finalize (
      Object : in out Static_Interrupt_Protection) is
   begin
      for I in Object.Previous_Handlers'Range loop
         declare
            Previous_Item : Previous_Handler_Item
               renames Object.Previous_Handlers (I);
         begin
            Ada.Interrupts.Unchecked_Attach_Handler (
               Previous_Item.Handler,
               Previous_Item.Interrupt);
         end;
      end loop;
   end Finalize;

end System.Interrupts;
