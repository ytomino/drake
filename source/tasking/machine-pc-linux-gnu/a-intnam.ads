pragma License (Unrestricted);
--  specialized for Linux
private with C.signal;
package Ada.Interrupts.Names is
   --  This package is system-specific.

   SIGHUP : constant Interrupt_Id;
   SIGINT : constant Interrupt_Id;
   SIGQUIT : constant Interrupt_Id;
   SIGILL : constant Interrupt_Id;
   SIGTRAP : constant Interrupt_Id;
   SIGABRT : constant Interrupt_Id;
--  SIGIOT : Interrupt_Id renames SIGABRT;
   SIGBUS : constant Interrupt_Id;
   SIGFPE : constant Interrupt_Id;
   SIGKILL : constant Interrupt_Id;
   SIGUSR1 : constant Interrupt_Id;
   SIGSEGV : constant Interrupt_Id;
   SIGUSR2 : constant Interrupt_Id;
   SIGPIPE : constant Interrupt_Id;
   SIGALRM : constant Interrupt_Id;
   SIGTERM : constant Interrupt_Id;
   SIGSTKFLT : constant Interrupt_Id;
   SIGCHLD : constant Interrupt_Id;
--  SIGCLD : constant Interrupt_Id renames SIGCHLD;
   SIGCONT : constant Interrupt_Id;
   SIGSTOP : constant Interrupt_Id;
   SIGTSTP : constant Interrupt_Id;
   SIGTTIN : constant Interrupt_Id;
   SIGTTOU : constant Interrupt_Id;
   SIGURG : constant Interrupt_Id;
   SIGXCPU : constant Interrupt_Id;
   SIGXFSZ : constant Interrupt_Id;
   SIGVTALRM : constant Interrupt_Id;
   SIGPROF : constant Interrupt_Id;
   SIGWINCH : constant Interrupt_Id;
   SIGIO : constant Interrupt_Id;
--  SIGPOLL : constant Interrupt_Id renames SIGIO;
   SIGPWR : constant Interrupt_Id;
   SIGSYS : constant Interrupt_Id;
--  SIGUNUSED : constant Interrupt_Id renames SIGSYS;

   SIGRTMIN : constant Interrupt_Id;
   SIGRTMAX : constant Interrupt_Id;

   First_Interrupt_Id : constant Interrupt_Id;
   Last_Interrupt_Id : constant Interrupt_Id;

private

   SIGHUP : constant Interrupt_Id := C.signal.SIGHUP;
   SIGINT : constant Interrupt_Id := C.signal.SIGINT;
   SIGQUIT : constant Interrupt_Id := C.signal.SIGQUIT;
   SIGILL : constant Interrupt_Id := C.signal.SIGILL;
   SIGTRAP : constant Interrupt_Id := C.signal.SIGTRAP;
   SIGABRT : constant Interrupt_Id := C.signal.SIGABRT;
   SIGBUS : constant Interrupt_Id := C.signal.SIGBUS;
   SIGFPE : constant Interrupt_Id := C.signal.SIGFPE;
   SIGKILL : constant Interrupt_Id := C.signal.SIGKILL;
   SIGUSR1 : constant Interrupt_Id := C.signal.SIGUSR1;
   SIGSEGV : constant Interrupt_Id := C.signal.SIGSEGV;
   SIGUSR2 : constant Interrupt_Id := C.signal.SIGUSR2;
   SIGPIPE : constant Interrupt_Id := C.signal.SIGPIPE;
   SIGALRM : constant Interrupt_Id := C.signal.SIGALRM;
   SIGTERM : constant Interrupt_Id := C.signal.SIGTERM;
   SIGSTKFLT : constant Interrupt_Id := C.signal.SIGSTKFLT;
   SIGCHLD : constant Interrupt_Id := C.signal.SIGCHLD;
   SIGCONT : constant Interrupt_Id := C.signal.SIGCONT;
   SIGSTOP : constant Interrupt_Id := C.signal.SIGSTOP;
   SIGTSTP : constant Interrupt_Id := C.signal.SIGTSTP;
   SIGTTIN : constant Interrupt_Id := C.signal.SIGTTIN;
   SIGTTOU : constant Interrupt_Id := C.signal.SIGTTOU;
   SIGURG : constant Interrupt_Id := C.signal.SIGURG;
   SIGXCPU : constant Interrupt_Id := C.signal.SIGXCPU;
   SIGXFSZ : constant Interrupt_Id := C.signal.SIGXFSZ;
   SIGVTALRM : constant Interrupt_Id := C.signal.SIGVTALRM;
   SIGPROF : constant Interrupt_Id := C.signal.SIGPROF;
   SIGWINCH : constant Interrupt_Id := C.signal.SIGWINCH;
   SIGIO : constant Interrupt_Id := C.signal.SIGIO;
   SIGSYS : constant Interrupt_Id := C.signal.SIGSYS;
   SIGPWR : constant Interrupt_Id := C.signal.SIGPWR;

   SIGRTMIN : constant Interrupt_Id := Interrupt_Id (C.signal.SIGRTMIN);
   SIGRTMAX : constant Interrupt_Id := Interrupt_Id (C.signal.SIGRTMAX);

   First_Interrupt_Id : constant Interrupt_Id := SIGHUP;
   Last_Interrupt_Id : constant Interrupt_Id := SIGRTMAX;

end Ada.Interrupts.Names;
