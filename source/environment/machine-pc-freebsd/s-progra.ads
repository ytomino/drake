pragma License (Unrestricted);
--  extended unit specialized for FreeBSD
private with System.Storage_Map;
package System.Program is
   --  Probing information of the program itself.
   pragma Preelaborate;

   --  the executable file

   function Full_Name return String;

   function Load_Address return Address;
   pragma Inline (Load_Address); -- renamed

private

   function Load_Address return Address
      renames Storage_Map.Load_Address;

end System.Program;
