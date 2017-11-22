pragma License (Unrestricted);
--  specialized for Windows
private with C.signal;
package Ada.Interrupts.Names is
   --  This package is system-specific.

   SIGINT : constant Interrupt_Id;
   SIGILL : constant Interrupt_Id;
   SIGABRT_COMPAT : constant Interrupt_Id;
   SIGFPE : constant Interrupt_Id;
   SIGSEGV : constant Interrupt_Id;
   SIGTERM : constant Interrupt_Id;
   SIGBREAK : constant Interrupt_Id;
   SIGABRT : constant Interrupt_Id;
--  SIGABRT2 : constant Interrupt_Id renames SIGABRT;

   First_Interrupt_Id : constant Interrupt_Id;
   Last_Interrupt_Id : constant Interrupt_Id;

private

   SIGINT : constant Interrupt_Id := 2;
   SIGILL : constant Interrupt_Id := 4;
   SIGABRT_COMPAT : constant Interrupt_Id := 6;
   SIGFPE : constant Interrupt_Id := 8;
   SIGSEGV : constant Interrupt_Id := 11;
   SIGTERM : constant Interrupt_Id := 15;
   SIGBREAK : constant Interrupt_Id := 21;
   SIGABRT : constant Interrupt_Id := 22;

   First_Interrupt_Id : constant Interrupt_Id := 1;
   Last_Interrupt_Id : constant Interrupt_Id := C.signal.NSIG - 1;

end Ada.Interrupts.Names;
