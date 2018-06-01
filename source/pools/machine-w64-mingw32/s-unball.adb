with System.Debug;
with C.basetsd;
with C.winbase;
with C.windef;
package body System.Unbounded_Allocators is
   use type Storage_Elements.Storage_Offset;
   use type C.windef.WINBOOL;
   use type C.winnt.HANDLE; -- C.void_ptr

   --  implementation

   procedure Initialize (Object : in out Unbounded_Allocator) is
   begin
      Object := Unbounded_Allocator (C.winbase.HeapCreate (0, 0, 0));
   end Initialize;

   procedure Finalize (Object : in out Unbounded_Allocator) is
      Success : C.windef.WINBOOL;
   begin
      Success := C.winbase.HeapDestroy (C.winnt.HANDLE (Object));
      pragma Check (Debug,
         Check =>
            Success /= C.windef.FALSE
            or else Debug.Runtime_Error ("HeapDestroy failed"));
   end Finalize;

   procedure Allocate (
      Allocator : in out Unbounded_Allocator;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count) is
   begin
      Storage_Address := Address (
         C.winbase.HeapAlloc (
            C.winnt.HANDLE (Allocator),
            0,
            C.basetsd.SIZE_T (Size_In_Storage_Elements)));
      if Storage_Address = Null_Address then
         raise Storage_Error;
      elsif Storage_Address mod Alignment /= 0 then
         Deallocate (
            Allocator,
            Storage_Address,
            Size_In_Storage_Elements,
            Alignment);
         raise Storage_Error;
      end if;
   end Allocate;

   procedure Deallocate (
      Allocator : in out Unbounded_Allocator;
      Storage_Address : Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count)
   is
      pragma Unreferenced (Size_In_Storage_Elements);
      pragma Unreferenced (Alignment);
      Success : C.windef.WINBOOL;
   begin
      Success := C.winbase.HeapFree (
         C.winnt.HANDLE (Allocator),
         0,
         C.windef.LPVOID (Storage_Address));
      pragma Check (Debug,
         Check =>
            Success /= C.windef.FALSE
            or else Debug.Runtime_Error ("HeapFree failed"));
   end Deallocate;

end System.Unbounded_Allocators;
