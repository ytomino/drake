pragma License (Unrestricted);
--  runtime unit required by compiler
package System.Unsigned_Types is
   pragma Pure;

   --  required for modular types by compiler (s-unstyp.ads)
   type Short_Short_Unsigned is mod 2 ** Short_Short_Integer'Size;
   type Short_Unsigned is mod 2 ** Short_Integer'Size;
   type Unsigned is mod 2 ** Integer'Size;
   type Long_Unsigned is mod 2 ** Long_Integer'Size;
   type Long_Long_Unsigned is mod 2 ** Long_Long_Integer'Size;

   function Shift_Left (Left : Unsigned; Right : Natural) return Unsigned;
   function Shift_Left (Left : Long_Long_Unsigned; Right : Natural)
      return Long_Long_Unsigned;
   pragma Import (Intrinsic, Shift_Left);

   --  required for packed boolean arrays by compiler (s-unstyp.ads)

   type Packed_Byte is mod 2 ** Standard'Storage_Unit;
   for Packed_Byte'Size use Standard'Storage_Unit;

   type Packed_Bytes1 is array (Natural range <>) of Packed_Byte;
   for Packed_Bytes1'Alignment use 1;
   for Packed_Bytes1'Component_Size use Packed_Byte'Size;
   pragma Suppress_Initialization (Packed_Bytes1);

   type Packed_Bytes2 is new Packed_Bytes1;
   for Packed_Bytes2'Alignment use Integer'Min (2, Standard'Maximum_Alignment);
   pragma Suppress_Initialization (Packed_Bytes2);

   type Packed_Bytes4 is new Packed_Bytes1;
   for Packed_Bytes4'Alignment use Integer'Min (4, Standard'Maximum_Alignment);
   pragma Suppress_Initialization (Packed_Bytes4);

   --  required for Is_Negative by compiler (s-unstyp.ads)
   type Float_Unsigned is mod 2 ** Float'Size;

   --  required by compiler ??? (s-unstyp.ads)
--  type Bits_1 is mod 2 ** 1;
--  type Bits_2 is mod 2 ** 2;
--  type Bits_4 is mod 2 ** 4;

end System.Unsigned_Types;
