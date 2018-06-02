pragma License (Unrestricted);
--  runtime unit specialized for Darwin
package System.Storage_Map is
   pragma Preelaborate;

   function Load_Address return Address;
   pragma Inline (Load_Address);

   Growing_Down_Is_Preferred : constant Boolean := False;

end System.Storage_Map;
