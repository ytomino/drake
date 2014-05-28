pragma License (Unrestricted);
--  implementation unit
generic
   type Bits_Type is mod <>;
package System.Packed_Arrays is
   pragma Pure;

   function Get (Arr : Address; N : Natural) return Bits_Type;
   pragma Pure_Function (Get);
   pragma Inline (Get);
   procedure Set (Arr : Address; N : Natural; E : Bits_Type);
   pragma Inline (Set);

end System.Packed_Arrays;
