pragma License (Unrestricted);
--  implementation unit required by compiler
with Interfaces;
package System.Atomic_Primitives is
   pragma Preelaborate;

   --  required for pragma Lock_Free by compiler (s-atopri.ads)

   subtype uint8 is Interfaces.Unsigned_8;
   subtype uint16 is Interfaces.Unsigned_16;
   subtype uint32 is Interfaces.Unsigned_32;
   subtype uint64 is Interfaces.Unsigned_64;

   function Lock_Free_Read_8 (Ptr : Address) return Interfaces.Unsigned_8;
   pragma Inline (Lock_Free_Read_8);
   function Lock_Free_Read_16 (Ptr : Address) return Interfaces.Unsigned_16;
   pragma Inline (Lock_Free_Read_16);
   function Lock_Free_Read_32 (Ptr : Address) return Interfaces.Unsigned_32;
   pragma Inline (Lock_Free_Read_32);
   function Lock_Free_Read_64 (Ptr : Address) return Interfaces.Unsigned_64;
   pragma Inline (Lock_Free_Read_64);

   function Lock_Free_Try_Write_8 (
      Ptr : Address;
      Expected : in out Interfaces.Unsigned_8;
      Desired : Interfaces.Unsigned_8)
      return Boolean;
   pragma Inline (Lock_Free_Try_Write_8);
   function Lock_Free_Try_Write_16 (
      Ptr : Address;
      Expected : in out Interfaces.Unsigned_16;
      Desired : Interfaces.Unsigned_16)
      return Boolean;
   pragma Inline (Lock_Free_Try_Write_16);
   function Lock_Free_Try_Write_32 (
      Ptr : Address;
      Expected : in out Interfaces.Unsigned_32;
      Desired : Interfaces.Unsigned_32)
      return Boolean;
   pragma Inline (Lock_Free_Try_Write_32);
   function Lock_Free_Try_Write_64 (
      Ptr : Address;
      Expected : in out Interfaces.Unsigned_64;
      Desired : Interfaces.Unsigned_64)
      return Boolean;
   pragma Inline (Lock_Free_Try_Write_64);

end System.Atomic_Primitives;
