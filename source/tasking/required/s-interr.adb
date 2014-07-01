package body System.Interrupts is

   procedure Install_Handlers (
      Object : not null access Static_Interrupt_Protection;
      New_Handlers : New_Handler_Array)
   is
      Length : constant Natural := New_Handlers'Length;
   begin
      for I in 0 .. Length - 1 loop
         declare
            New_Item : New_Handler_Item
               renames New_Handlers (New_Handlers'First + I);
         begin
            if Ada.Interrupts.Is_Reserved (New_Item.Interrupt) then
               raise Program_Error; -- CXC3002
            end if;
            declare
               Previous_Item : Previous_Handler_Item
                  renames Object.Previous_Handlers (
                     Object.Previous_Handlers'First + I);
            begin
               Previous_Item.Interrupt := New_Item.Interrupt;
               Interrupt_Handlers.Set_Static_Handler (
                  New_Item.Handler);
               Ada.Interrupts.Unchecked_Exchange_Handler (
                  Previous_Item.Handler,
                  New_Item.Handler,
                  New_Item.Interrupt);
            end;
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
