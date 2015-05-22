pragma License (Unrestricted);
--  runtime unit required by compiler
with System.Unwind;
package System.Standard_Library is
   pragma Preelaborate;

   --  required for controlled type by compiler (s-stalib.ads)
   procedure Abort_Undefer_Direct
      with Import,
         Convention => Ada,
         External_Name => "system__standard_library__abort_undefer_direct";

   --  required by compiler (s-stalib.ads)
   subtype Exception_Data_Ptr is Unwind.Exception_Data_Access;

end System.Standard_Library;
