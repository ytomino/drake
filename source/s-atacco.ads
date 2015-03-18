pragma License (Unrestricted);
generic
   type Object (<>) is limited private;
package System.Address_To_Access_Conversions is
   pragma Preelaborate;

   type Object_Pointer is access all Object;
   pragma No_Strict_Aliasing (Object_Pointer);
   function To_Pointer (Value : Address) return Object_Pointer;
   pragma Import (Intrinsic, To_Pointer);
   function To_Address (Value : Object_Pointer) return Address;
   pragma Import (Intrinsic, To_Address);

--  pragma Convention (Intrinsic, To_Pointer);
--  pragma Convention (Intrinsic, To_Address);

end System.Address_To_Access_Conversions;
