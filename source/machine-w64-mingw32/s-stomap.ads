pragma License (Unrestricted);
--  runtime unit specialized for Windows
with C.windef;
package System.Storage_Map is
   pragma Preelaborate;

   function Load_Address return Address;

   --  module handle of "NTDLL.DLL"
   function NTDLL return C.windef.HMODULE;

   Growing_Down_Is_Preferred : constant Boolean := False;

end System.Storage_Map;
