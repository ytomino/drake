pragma License (Unrestricted);
package System.Storage_Elements is
   pragma Pure;

   type Storage_Offset is range
      -(2 ** (Standard'Address_Size - 1)) ..
      +(2 ** (Standard'Address_Size - 1)) - 1; -- implementation-defined

   subtype Storage_Count is Storage_Offset range 0 .. Storage_Offset'Last;

   type Storage_Element is mod 2 ** Storage_Unit; -- implementation-defined
   for Storage_Element'Size use Storage_Unit;
   type Storage_Array is
      array (Storage_Offset range <>) of aliased Storage_Element;
   for Storage_Array'Component_Size use Storage_Unit;

   --  Address Arithmetic:

   function "+" (Left : Address; Right : Storage_Offset) return Address;
   function "+" (Left : Storage_Offset; Right : Address) return Address;
   pragma Convention (Intrinsic, "+");
   pragma Pure_Function ("+");
   pragma Inline_Always ("+");
   function "-" (Left : Address; Right : Storage_Offset) return Address;
   function "-" (Left, Right : Address) return Storage_Offset;
   pragma Convention (Intrinsic, "-");
   pragma Pure_Function ("-");
   pragma Inline_Always ("-");

   function "mod" (Left : Address; Right : Storage_Offset)
      return Storage_Offset;
   pragma Convention (Intrinsic, "mod");
   pragma Pure_Function ("mod");
   pragma Inline_Always ("mod");

   --  Conversion to/from integers:

   type Integer_Address is mod Memory_Size; -- implementation-defined
   function To_Address (Value : Integer_Address) return Address;
   pragma Convention (Intrinsic, To_Address);
   pragma Pure_Function (To_Address);
   pragma Inline_Always (To_Address);
   function To_Integer (Value : Address) return Integer_Address;
   pragma Convention (Intrinsic, To_Integer);
   pragma Pure_Function (To_Integer);
   pragma Inline_Always (To_Integer);

--  pragma Convention (Intrinsic, "+");
   --  ...and so on for all language-defined subprograms declared in this
   --  package.

   --  extended
   function Shift_Left (Left : Storage_Element; Right : Natural)
      return Storage_Element;
   pragma Import (Intrinsic, Shift_Left);

   --  extended
   subtype Address_Image is String (1 .. Standard'Address_Size / 4);

end System.Storage_Elements;
