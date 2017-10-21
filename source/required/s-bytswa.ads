pragma License (Unrestricted);
--  implementation unit required by compiler
package System.Byte_Swapping is
   pragma Pure;

   type U16 is mod 2 ** 16;
   type U32 is mod 2 ** 32;
   type U64 is mod 2 ** 64;

   function Bswap_16 (X : U16) return U16
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_bswap16";

   function Bswap_32 (X : U32) return U32
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_bswap32";

   function Bswap_64 (X : U64) return U64
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_bswap64";

end System.Byte_Swapping;
