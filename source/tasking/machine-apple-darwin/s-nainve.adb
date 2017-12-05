with Ada.Exceptions;
with Ada.Interrupts.Names;
with Ada.Unchecked_Conversion;
with System.Formatting;
with System.Long_Long_Integer_Types;
with System.Unwind.Occurrences;
with C.signal;
package body System.Native_Interrupts.Vector is
   use type Ada.Interrupts.Parameterless_Handler;
   use type C.signed_int;
   use type C.unsigned_int;

   subtype Word_Unsigned is Long_Long_Integer_Types.Word_Unsigned;

   procedure Report (
      Interrupt : Interrupt_Id;
      X : Ada.Exceptions.Exception_Occurrence);
   procedure Report (
      Interrupt : Interrupt_Id;
      X : Ada.Exceptions.Exception_Occurrence)
   is
      function Cast is
         new Ada.Unchecked_Conversion (
            Ada.Exceptions.Exception_Occurrence,
            Unwind.Exception_Occurrence);
      Name_Prefix : constant String := "Interrupt ";
      Name : String (1 .. Name_Prefix'Length + Interrupt_Id'Width);
      Name_Last : Natural;
      Error : Boolean;
   begin
      Name (1 .. Name_Prefix'Length) := Name_Prefix;
      Formatting.Image (
         Word_Unsigned (Interrupt),
         Name (Name_Prefix'Length + 1 .. Name'Last),
         Name_Last,
         Error => Error);
      Unwind.Occurrences.Report (Cast (X), Name (1 .. Name_Last));
   end Report;

   type Signal_Rec is record
      Installed_Handler : Parameterless_Handler;
      Saved : aliased C.signal.struct_sigaction;
   end record;
   pragma Suppress_Initialization (Signal_Rec);

   type Signal_Vec is
      array (
            C.signed_int range
               C.signed_int (Ada.Interrupts.Names.First_Interrupt_Id) ..
               C.signed_int (Ada.Interrupts.Names.Last_Interrupt_Id)) of
         Signal_Rec;
   pragma Suppress_Initialization (Signal_Vec);

   Table : Signal_Vec;

   procedure Handler (
      Signal_Number : C.signed_int;
      Info : access C.signal.siginfo_t;
      Context : C.void_ptr)
      with Convention => C;

   procedure Handler (
      Signal_Number : C.signed_int;
      Info : access C.signal.siginfo_t;
      Context : C.void_ptr)
   is
      pragma Unreferenced (Info);
      pragma Unreferenced (Context);
   begin
      Table (Signal_Number).Installed_Handler.all;
   exception -- CXC3004, an exception propagated from a handler has no effect
      when E : others =>
         Report (Interrupt_Id (Signal_Number), E);
   end Handler;

   --  implementation

   function Current_Handler (Interrupt : Interrupt_Id)
      return Parameterless_Handler is
   begin
      return Table (Interrupt).Installed_Handler;
   end Current_Handler;

   procedure Exchange_Handler (
      Old_Handler : out Parameterless_Handler;
      New_Handler : Parameterless_Handler;
      Interrupt : Interrupt_Id)
   is
      Item : Signal_Rec
         renames Table (Interrupt);
   begin
      Old_Handler := Item.Installed_Handler;
      if Old_Handler = null and then New_Handler /= null then
         declare
            Action : aliased C.signal.struct_sigaction := (
               (Unchecked_Tag => 1, sa_sigaction => Handler'Access),
               others => <>); -- uninitialized
            Dummy : C.signed_int;
         begin
            Action.sa_flags := C.signed_int (
               C.unsigned_int'(C.signal.SA_SIGINFO or C.signal.SA_RESTART));
            Dummy := C.signal.sigemptyset (Action.sa_mask'Access);
            if C.signal.sigaction (
               Interrupt,
               Action'Access,
               Item.Saved'Access) < 0
            then
               raise Program_Error;
            end if;
         end;
      elsif Old_Handler /= null and then New_Handler = null then
         if C.signal.sigaction (Interrupt, Item.Saved'Access, null) < 0 then
            raise Program_Error;
         end if;
      end if;
      Item.Installed_Handler := New_Handler;
   end Exchange_Handler;

end System.Native_Interrupts.Vector;
