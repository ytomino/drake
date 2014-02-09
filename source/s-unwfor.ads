pragma License (Unrestricted);
--  runtime unit
package System.Unwind.Foreign is
   pragma Preelaborate;

   --  the wide exception raised from any other runtimes (s-except.ads)
   Foreign_Exception : exception;
   pragma Export (Ada, Foreign_Exception,
      "system__exceptions__foreign_exception");

end System.Unwind.Foreign;
