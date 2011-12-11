pragma License (Unrestricted);
pragma Compiler_Unit;
--  runtime unit
with System.Standard_Library;
package System.Exception_Table is
   pragma Preelaborate;

   --  required for user-defined exception by compiler (s-exctab.ads)
   procedure Register_Exception (X : Standard_Library.Exception_Data_Ptr) is
      null; -- unimplemented

end System.Exception_Table;
