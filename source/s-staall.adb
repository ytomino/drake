with System.System_Allocators;
with System.Unwind.Raising;
with System.Unwind.Standard;
package body System.Standard_Allocators is
   pragma Suppress (All_Checks);

   function Allocate (
      Size : Storage_Elements.Storage_Count)
      return Address
   is
      Result : constant Address := System_Allocators.Allocate (Size);
   begin
      if Result = Null_Address then
         Raise_Heap_Exhausted;
      end if;
      return Result;
   end Allocate;

   procedure Free (Storage_Address : Address) is
   begin
      System_Allocators.Free (Storage_Address);
   end Free;

   function Reallocate (
      Storage_Address : Address;
      Size : Storage_Elements.Storage_Count)
      return Address
   is
      Result : constant Address :=
         System_Allocators.Reallocate (Storage_Address, Size);
   begin
      if Result = Null_Address then
         Raise_Heap_Exhausted;
      end if;
      return Result;
   end Reallocate;

   procedure Raise_Heap_Exhausted is
      Heap_Exhausted : constant String := "heap exhausted"; -- (s-memory.adb)
   begin
      Unwind.Raising.Raise_Exception_From_Here_With (
         Unwind.Standard.Storage_Error'Access,
         Message => Heap_Exhausted);
   end Raise_Heap_Exhausted;

end System.Standard_Allocators;
