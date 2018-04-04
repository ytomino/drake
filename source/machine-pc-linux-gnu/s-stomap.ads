pragma License (Unrestricted);
--  runtime unit specialized for Linux
package System.Storage_Map is
   pragma Preelaborate;

   function Load_Address return Address;

   Growing_Down_Is_Preferred : constant Boolean := True; -- bias of mmap

end System.Storage_Map;
