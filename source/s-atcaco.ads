pragma License (Unrestricted);
--  extended unit, please see AI05-0002-1
generic
   type Object (<>) is limited private;
   type Object_Pointer is access constant Object;
package System.Address_To_Constant_Access_Conversions is
   --  This is an implementation of Robert I. Eachus's plan in AI05-0002-1.
   pragma Preelaborate;

   function To_Pointer (Value : Address) return Object_Pointer
      with Import, Convention => Intrinsic;
   function To_Address (Value : Object_Pointer) return Address
      with Import, Convention => Intrinsic;

end System.Address_To_Constant_Access_Conversions;
