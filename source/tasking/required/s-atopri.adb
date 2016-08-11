with System.Address_To_Named_Access_Conversions;
with System.Storage_Barriers;
package body System.Atomic_Primitives is
   use type Interfaces.Unsigned_8;
   use type Interfaces.Unsigned_16;
   use type Interfaces.Unsigned_32;
   use type Interfaces.Unsigned_64;

   pragma Compile_Time_Warning (not uint8'Atomic_Always_Lock_Free,
      "uint8 is not atomic");
   pragma Compile_Time_Warning (not uint16'Atomic_Always_Lock_Free,
      "uint16 is not atomic");
   pragma Compile_Time_Warning (not uint32'Atomic_Always_Lock_Free,
      "uint32 is not atomic");
--  pragma Compile_Time_Warning (not uint64'Atomic_Always_Lock_Free,
--    "uint64 is not atomic");

   type uint8_Access is access all uint8;
   for uint8_Access'Storage_Size use 0;
   type uint16_Access is access all uint16;
   for uint16_Access'Storage_Size use 0;
   type uint32_Access is access all uint32;
   for uint32_Access'Storage_Size use 0;
   type uint64_Access is access all uint64;
   for uint64_Access'Storage_Size use 0;

   package uint8_Access_Conv is
      new Address_To_Named_Access_Conversions (uint8, uint8_Access);
   package uint16_Access_Conv is
      new Address_To_Named_Access_Conversions (uint16, uint16_Access);
   package uint32_Access_Conv is
      new Address_To_Named_Access_Conversions (uint32, uint32_Access);
   package uint64_Access_Conv is
      new Address_To_Named_Access_Conversions (uint64, uint64_Access);

   --  Use sequentially consistent model for general purpose.
   Order : constant := Storage_Barriers.ATOMIC_SEQ_CST;

   function atomic_load (
      ptr : not null access constant uint8;
      memorder : Integer := Order)
      return uint8
      with Import, Convention => Intrinsic, External_Name => "__atomic_load_1";
   function atomic_load (
      ptr : not null access constant uint16;
      memorder : Integer := Order)
      return uint16
      with Import, Convention => Intrinsic, External_Name => "__atomic_load_2";
   function atomic_load (
      ptr : not null access constant uint32;
      memorder : Integer := Order)
      return uint32
      with Import, Convention => Intrinsic, External_Name => "__atomic_load_4";
   function atomic_load (
      ptr : not null access constant uint64;
      memorder : Integer := Order)
      return uint64
      with Import, Convention => Intrinsic, External_Name => "__atomic_load_8";

   function atomic_compare_exchange (
      ptr : not null access uint8;
      expected : not null access uint8;
      desired : uint8;
      weak : Boolean := False;
      success_memorder : Integer := Order;
      failure_memorder : Integer := Order)
      return Boolean
      with Import,
         Convention => Intrinsic,
         External_Name => "__atomic_compare_exchange_1";
   function atomic_compare_exchange (
      ptr : not null access uint16;
      expected : not null access uint16;
      desired : uint16;
      weak : Boolean := False;
      success_memorder : Integer := Order;
      failure_memorder : Integer := Order)
      return Boolean
      with Import,
         Convention => Intrinsic,
         External_Name => "__atomic_compare_exchange_2";
   function atomic_compare_exchange (
      ptr : not null access uint32;
      expected : not null access uint32;
      desired : uint32;
      weak : Boolean := False;
      success_memorder : Integer := Order;
      failure_memorder : Integer := Order)
      return Boolean
      with Import,
         Convention => Intrinsic,
         External_Name => "__atomic_compare_exchange_4";
   function atomic_compare_exchange (
      ptr : not null access uint64;
      expected : not null access uint64;
      desired : uint64;
      weak : Boolean := False;
      success_memorder : Integer := Order;
      failure_memorder : Integer := Order)
      return Boolean
      with Import,
         Convention => Intrinsic,
         External_Name => "__atomic_compare_exchange_8";

   --  implementation

   function Lock_Free_Read_8 (Ptr : Address) return Interfaces.Unsigned_8 is
   begin
      return atomic_load (uint8_Access_Conv.To_Pointer (Ptr));
   end Lock_Free_Read_8;

   function Lock_Free_Read_16 (Ptr : Address) return Interfaces.Unsigned_16 is
   begin
      return atomic_load (uint16_Access_Conv.To_Pointer (Ptr));
   end Lock_Free_Read_16;

   function Lock_Free_Read_32 (Ptr : Address) return Interfaces.Unsigned_32 is
   begin
      return atomic_load (uint32_Access_Conv.To_Pointer (Ptr));
   end Lock_Free_Read_32;

   function Lock_Free_Read_64 (Ptr : Address) return Interfaces.Unsigned_64 is
   begin
      return atomic_load (uint64_Access_Conv.To_Pointer (Ptr));
   end Lock_Free_Read_64;

   function Lock_Free_Try_Write_8 (
      Ptr : Address;
      Expected : in out Interfaces.Unsigned_8;
      Desired : Interfaces.Unsigned_8)
      return Boolean is
   begin
      return atomic_compare_exchange (
         uint8_Access_Conv.To_Pointer (Ptr),
         Expected'Unrestricted_Access,
         Desired);
   end Lock_Free_Try_Write_8;

   function Lock_Free_Try_Write_16 (
      Ptr : Address;
      Expected : in out Interfaces.Unsigned_16;
      Desired : Interfaces.Unsigned_16)
      return Boolean is
   begin
      return atomic_compare_exchange (
         uint16_Access_Conv.To_Pointer (Ptr),
         Expected'Unrestricted_Access,
         Desired);
   end Lock_Free_Try_Write_16;

   function Lock_Free_Try_Write_32 (
      Ptr : Address;
      Expected : in out Interfaces.Unsigned_32;
      Desired : Interfaces.Unsigned_32)
      return Boolean is
   begin
      return atomic_compare_exchange (
         uint32_Access_Conv.To_Pointer (Ptr),
         Expected'Unrestricted_Access,
         Desired);
   end Lock_Free_Try_Write_32;

   function Lock_Free_Try_Write_64 (
      Ptr : Address;
      Expected : in out Interfaces.Unsigned_64;
      Desired : Interfaces.Unsigned_64)
      return Boolean is
   begin
      return atomic_compare_exchange (
         uint64_Access_Conv.To_Pointer (Ptr),
         Expected'Unrestricted_Access,
         Desired);
   end Lock_Free_Try_Write_64;

end System.Atomic_Primitives;
