pragma License (Unrestricted);
--  runtime unit
package System.Unwind.Foreign is
   pragma Preelaborate;

   --  This is the substitute name of any exceptions propagated from any other
   --    runtimes. (s-except.ads)
   Foreign_Exception : exception
      with Export,
         Convention => Ada,
         External_Name => "system__exceptions__foreign_exception";

end System.Unwind.Foreign;
