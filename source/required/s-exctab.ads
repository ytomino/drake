pragma License (Unrestricted);
--  runtime unit required by compiler
with System.Unwind;
package System.Exception_Table is
   pragma Preelaborate;

   --  required for user-defined exception by compiler (s-exctab.ads)
   procedure Register_Exception (X : Unwind.Exception_Data_Access) is
      null; -- unimplemented
   pragma Inline (Register_Exception);
      --  [gcc-7] can not skip calling null procedure

end System.Exception_Table;
