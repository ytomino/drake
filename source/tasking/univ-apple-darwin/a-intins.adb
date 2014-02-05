with Ada.Unchecked_Conversion;
with Ada.Exceptions;
with Ada.Interrupts.Names;
with System.Formatting;
with System.Termination;
with System.Unwind;
with C.signal;
package body Ada.Interrupts.Inside is
   use type System.Address;
   use type C.signed_int;
   use type C.unsigned_int;

   Report_Traceback : access procedure (
      Current : Exceptions.Exception_Occurrence);
   pragma Import (Ada, Report_Traceback, "__drake_ref_report_traceback");
   pragma Weak_External (Report_Traceback);

   procedure Report (
      Interrupt : Interrupt_Id;
      Current : Exceptions.Exception_Occurrence);
   procedure Report (
      Interrupt : Interrupt_Id;
      Current : Exceptions.Exception_Occurrence)
   is
      function Cast is new Unchecked_Conversion (
         Exceptions.Exception_Occurrence,
         System.Unwind.Exception_Occurrence);
      subtype Fixed_String is String (Positive);
      Full_Name : Fixed_String;
      for Full_Name'Address use Cast (Current).Id.Full_Name;
      Name : String (1 .. Interrupt_Id'Width);
      Name_Last : Natural;
      Error : Boolean;
   begin
      System.Formatting.Image (
         System.Formatting.Unsigned (Interrupt),
         Name,
         Name_Last,
         Error => Error);
      System.Termination.Error_New_Line;
      if Cast (Current).Num_Tracebacks > 0
         and then Report_Traceback'Address /= System.Null_Address
      then
         System.Termination.Error_Put ("Interrupt ");
         System.Termination.Error_Put (Name (Name'First .. Name_Last));
         System.Termination.Error_Put (" terminated by unhandled exception");
         System.Termination.Error_New_Line;
         Report_Traceback (Current);
      else
         System.Termination.Error_Put ("in interrupt ");
         System.Termination.Error_Put (Name (Name'First .. Name_Last));
         System.Termination.Error_Put (", raised ");
         System.Termination.Error_Put (
            Full_Name (1 .. Cast (Current).Id.Name_Length - 1));
         if Cast (Current).Msg_Length > 0 then
            System.Termination.Error_Put (" : ");
            System.Termination.Error_Put (
               Cast (Current).Msg (1 .. Cast (Current).Msg_Length));
         end if;
         System.Termination.Error_New_Line;
      end if;
   end Report;

   type Signal_Rec is record
      Installed_Handler : Parameterless_Handler;
      Saved : aliased C.signal.struct_sigaction;
   end record;
   pragma Suppress_Initialization (Signal_Rec);

   type Signal_Vec is array (
      Interrupts.Names.First_Interrupt_Id ..
      Interrupts.Names.Last_Interrupt_Id) of Signal_Rec;
   pragma Suppress_Initialization (Signal_Vec);

   Table : Signal_Vec;

   procedure Handler (
      Signal_Number : C.signed_int;
      Info : access C.signal.siginfo_t;
      Context : C.void_ptr);
   pragma Convention (C, Handler);
   procedure Handler (
      Signal_Number : C.signed_int;
      Info : access C.signal.siginfo_t;
      Context : C.void_ptr)
   is
      pragma Unreferenced (Info);
      pragma Unreferenced (Context);
   begin
      Table (Interrupt_Id (Signal_Number)).Installed_Handler.all;
   exception -- CXC3004, an exception propagated from a handler has no effect
      when E : others =>
         Report (Interrupt_Id (Signal_Number), E);
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
               Action : aliased C.signal.struct_sigaction := (
                  (Unchecked_Tag => 1, sa_sigaction => Handler'Access),
                  others => <>); -- uninitialized
            begin
               Action.sa_flags := C.signed_int (C.unsigned_int'(
                  C.signal.SA_SIGINFO
                  or C.signal.SA_RESTART));
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

   procedure Raise_Interrupt (Interrupt : Interrupt_Id) is
      Dummy : C.signed_int;
      pragma Unreferenced (Dummy);
   begin
      Dummy := C.signal.C_raise (C.signed_int (Interrupt));
   end Raise_Interrupt;

end Ada.Interrupts.Inside;
