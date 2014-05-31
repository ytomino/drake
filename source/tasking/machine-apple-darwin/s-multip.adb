with C.unistd;
package body System.Multiprocessors is
   use type C.signed_long;

   function Number_Of_CPUs return CPU is
      Result : constant C.signed_long :=
         C.unistd.sysconf (C.unistd.SC_NPROCESSORS_ONLN);
   begin
      if Result < 0 then
         raise Program_Error;
      end if;
      return CPU (Result);
   end Number_Of_CPUs;

end System.Multiprocessors;
