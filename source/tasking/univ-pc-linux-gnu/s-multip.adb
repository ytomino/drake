with C.bits.confname;
with C.unistd;
package body System.Multiprocessors is
   pragma Suppress (All_Checks);
   use type C.signed_long;

   function Number_Of_CPUs return CPU is
      Result : constant C.signed_long :=
         C.unistd.sysconf (
            C.bits.confname.Cast (C.unistd.SC_NPROCESSORS_ONLN));
   begin
      if Result < 0 then
         raise Program_Error;
      end if;
      return CPU (Result);
   end Number_Of_CPUs;

end System.Multiprocessors;
