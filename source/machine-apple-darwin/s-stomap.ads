pragma License (Unrestricted);
--  runtime unit specialized for Darwin
package System.Storage_Map is
   pragma Preelaborate;

   function Load_Address return Address;
   pragma Inline (Load_Address);

end System.Storage_Map;
