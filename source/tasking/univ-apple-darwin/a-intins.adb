with Ada.Interrupts.Names;
with C.signal;
package body Ada.Interrupts.Inside is
   use type C.signed_int;

   type Signal_Rec is record
      Installed_Handler : Parameterless_Handler;
      Saved : aliased C.signal.struct_sigaction;
   end record;
   pragma Suppress_Initialization (Signal_Rec);

   type Signal_Vec is array (
      Ada.Interrupts.Names.First_Interrupt_Id ..
      Ada.Interrupts.Names.Last_Interrupt_Id) of Signal_Rec;
   pragma Suppress_Initialization (Signal_Vec);

   Table : Signal_Vec;

   procedure Handler (
      Signal_Number : C.signed_int;
      Info : access C.signal.struct_siginfo;
      Context : C.void_ptr);
   pragma Convention (C, Handler);
   procedure Handler (
      Signal_Number : C.signed_int;
      Info : access C.signal.struct_siginfo;
      Context : C.void_ptr)
   is
      pragma Unreferenced (Info);
      pragma Unreferenced (Context);
   begin
      Table (Interrupt_Id (Signal_Number)).Installed_Handler.all;
   end Handler;

   --  implementation

   function Is_Reserved (Interrupt : Interrupt_Id) return Boolean is
   begin
      return Interrupt not in
         Names.First_Interrupt_Id ..
         Names.Last_Interrupt_Id;
   end Is_Reserved;

   function Current_Handler (Interrupt : Interrupt_Id)
      return Parameterless_Handler is
   begin
      if Is_Reserved (Interrupt) then
         raise Program_Error;
      end if;
      return Table (Interrupt).Installed_Handler;
   end Current_Handler;

   procedure Exchange_Handler (
      Old_Handler : out Parameterless_Handler;
      New_Handler : Parameterless_Handler;
      Interrupt : Interrupt_Id) is
   begin
      if Is_Reserved (Interrupt) then
         raise Program_Error;
      end if;
      declare
         Item : Signal_Rec
            renames Table (Interrupt);
      begin
         Old_Handler := Item.Installed_Handler;
         if Old_Handler = null and then New_Handler /= null then
            declare
               Dummy : C.signed_int;
               pragma Unreferenced (Dummy);
               Action : aliased C.signal.struct_sigaction :=
                  (others => <>); -- uninitialized
            begin
               Action.sigaction_u.sa_sigaction := Handler'Access;
               Action.sa_flags := C.signal.SA_SIGINFO
                  + C.signal.SA_RESTART;
               Dummy := C.signal.sigemptyset (Action.sa_mask'Access);
               Dummy := C.signal.sigaction (
                  C.signed_int (Interrupt),
                  Action'Access,
                  Item.Saved'Access);
            end;
         elsif Old_Handler /= null and then New_Handler = null then
            declare
               Dummy : C.signed_int;
               pragma Unreferenced (Dummy);
               Old_Action : aliased C.signal.struct_sigaction :=
                  (others => <>); -- uninitialized
            begin
               Dummy := C.signal.sigaction (
                  C.signed_int (Interrupt),
                  Item.Saved'Access,
                  Old_Action'Access);
            end;
         end if;
         Item.Installed_Handler := New_Handler;
      end;
   end Exchange_Handler;

end Ada.Interrupts.Inside;
