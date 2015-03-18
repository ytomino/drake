pragma License (Unrestricted);
--  extended unit, please see AI05-0002-1
generic
   type Object (<>) is limited private;
   type Object_Pointer is access all Object;
package System.Address_To_Named_Access_Conversions is
   --  This is an implementation of Robert I. Eachus's plan in AI05-0002-1.
   pragma Preelaborate;

   function To_Pointer (Value : Address) return Object_Pointer;
   pragma Import (Intrinsic, To_Pointer);
   function To_Address (Value : Object_Pointer) return Address;
   pragma Import (Intrinsic, To_Address);

--  pragma Convention (Intrinsic, To_Pointer);
--  pragma Convention (Intrinsic, To_Address);

end System.Address_To_Named_Access_Conversions;
