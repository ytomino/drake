pragma License (Unrestricted);
--  runtime unit
package System.Unwind.Foreign is
   pragma Preelaborate;

   --  the wide exception raised from any other runtimes (s-except.ads)
   Foreign_Exception : exception
      with Export,
         Convention => Ada,
         External_Name => "system__exceptions__foreign_exception";

end System.Unwind.Foreign;
