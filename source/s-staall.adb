with System.Native_Allocators;
with System.Unwind.Raising;
with System.Unwind.Standard;
package body System.Standard_Allocators is
   pragma Suppress (All_Checks);

   Heap_Exhausted : constant String := "heap exhausted";

   --  implementation

   function Allocate (
      Size : Storage_Elements.Storage_Count)
      return Address is
   begin
      return Result : constant Address := Native_Allocators.Allocate (Size) do
         if Result = Null_Address then
            Unwind.Raising.Raise_Exception_From_Here_With (
               Unwind.Standard.Storage_Error'Access,
               Message => Heap_Exhausted);
         end if;
      end return;
   end Allocate;

   procedure Free (Storage_Address : Address) is
   begin
      Native_Allocators.Free (Storage_Address);
   end Free;

   function Reallocate (
      Storage_Address : Address;
      Size : Storage_Elements.Storage_Count)
      return Address is
   begin
      return Result : constant Address :=
         Native_Allocators.Reallocate (Storage_Address, Size)
      do
         if Result = Null_Address then
            Unwind.Raising.Raise_Exception_From_Here_With (
               Unwind.Standard.Storage_Error'Access,
               Message => Heap_Exhausted);
         end if;
      end return;
   end Reallocate;

end System.Standard_Allocators;
