with C.winbase;
package body System.Multiprocessors is
   pragma Suppress (All_Checks);

   function Number_Of_CPUs return CPU is
      Info : aliased C.winbase.SYSTEM_INFO;
   begin
      C.winbase.GetSystemInfo (Info'Access);
      return CPU (Info.dwNumberOfProcessors);
   end Number_Of_CPUs;

end System.Multiprocessors;
