with System.Address_To_Access_Conversions;
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
   pragma Compile_Time_Warning (not uint64'Atomic_Always_Lock_Free,
      "uint64 is not atomic");

   package uint8_Conv is new Address_To_Access_Conversions (uint8);
   package uint16_Conv is new Address_To_Access_Conversions (uint16);
   package uint32_Conv is new Address_To_Access_Conversions (uint32);
   package uint64_Conv is new Address_To_Access_Conversions (uint64);

   Acquire : constant := 2; -- gcc's intrinsic

   --  implementation

   function Lock_Free_Read_8 (Ptr : Address) return Interfaces.Unsigned_8 is
   begin
      return Interfaces.atomic_load (uint8_Conv.To_Pointer (Ptr), Acquire);
   end Lock_Free_Read_8;

   function Lock_Free_Read_16 (Ptr : Address) return Interfaces.Unsigned_16 is
   begin
      return Interfaces.atomic_load (uint16_Conv.To_Pointer (Ptr), Acquire);
   end Lock_Free_Read_16;

   function Lock_Free_Read_32 (Ptr : Address) return Interfaces.Unsigned_32 is
   begin
      return Interfaces.atomic_load (uint32_Conv.To_Pointer (Ptr), Acquire);
   end Lock_Free_Read_32;

   function Lock_Free_Read_64 (Ptr : Address) return Interfaces.Unsigned_64 is
   begin
      return Interfaces.atomic_load (uint64_Conv.To_Pointer (Ptr), Acquire);
   end Lock_Free_Read_64;

   function Lock_Free_Try_Write_8 (
      Ptr : Address;
      Expected : in out Interfaces.Unsigned_8;
      Desired : Interfaces.Unsigned_8)
      return Boolean
   is
      Actual : Interfaces.Unsigned_8;
   begin
      Actual := Interfaces.sync_val_compare_and_swap (
         uint8_Conv.To_Pointer (Ptr),
         Expected,
         Desired);
      if Actual = Expected then
         return True;
      else
         Expected := Actual;
         return False;
      end if;
   end Lock_Free_Try_Write_8;

   function Lock_Free_Try_Write_16 (
      Ptr : Address;
      Expected : in out Interfaces.Unsigned_16;
      Desired : Interfaces.Unsigned_16)
      return Boolean
   is
      Actual : Interfaces.Unsigned_16;
   begin
      Actual := Interfaces.sync_val_compare_and_swap (
         uint16_Conv.To_Pointer (Ptr),
         Expected,
         Desired);
      if Actual = Expected then
         return True;
      else
         Expected := Actual;
         return False;
      end if;
   end Lock_Free_Try_Write_16;

   function Lock_Free_Try_Write_32 (
      Ptr : Address;
      Expected : in out Interfaces.Unsigned_32;
      Desired : Interfaces.Unsigned_32)
      return Boolean
   is
      Actual : Interfaces.Unsigned_32;
   begin
      Actual := Interfaces.sync_val_compare_and_swap (
         uint32_Conv.To_Pointer (Ptr),
         Expected,
         Desired);
      if Actual = Expected then
         return True;
      else
         Expected := Actual;
         return False;
      end if;
   end Lock_Free_Try_Write_32;

   function Lock_Free_Try_Write_64 (
      Ptr : Address;
      Expected : in out Interfaces.Unsigned_64;
      Desired : Interfaces.Unsigned_64)
      return Boolean
   is
      Actual : Interfaces.Unsigned_64;
   begin
      Actual := Interfaces.sync_val_compare_and_swap (
         uint64_Conv.To_Pointer (Ptr),
         Expected,
         Desired);
      if Actual = Expected then
         return True;
      else
         Expected := Actual;
         return False;
      end if;
   end Lock_Free_Try_Write_64;

end System.Atomic_Primitives;
