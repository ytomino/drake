with System.Debug;
with C.basetsd;
with C.winbase;
with C.windef;
with C.winnt;
package body System.System_Allocators is
   pragma Suppress (All_Checks);
   use type C.windef.WINBOOL;
--  use type C.basetsd.SIZE_T;
--  use type C.windef.DWORD;

   function Round_Up (Size : C.basetsd.SIZE_T) return C.basetsd.SIZE_T;
   function Round_Up (Size : C.basetsd.SIZE_T) return C.basetsd.SIZE_T is
      use type C.basetsd.SIZE_T;
      Result : C.basetsd.SIZE_T;
   begin
      --  do round up here since HeapSize always returns the same size
      --  that is passed to HeapAlloc
      --  heap memory is separated to 16, 24, 32... by Windows heap manager
      if Size < 16 then
         Result := 16;
      else
         Result := (Size + 7) and not 7;
      end if;
      return Result;
   end Round_Up;

   --  implementation

   function Allocate (Size : Storage_Elements.Storage_Count)
      return Address is
   begin
      return Address (
         C.winbase.HeapAlloc (
            C.winbase.GetProcessHeap,
            0,
            Round_Up (C.basetsd.SIZE_T (Size))));
   end Allocate;

   procedure Free (Storage_Address : Address) is
      R : C.windef.WINBOOL;
   begin
      R := C.winbase.HeapFree (
         C.winbase.GetProcessHeap,
         0,
         C.windef.LPVOID (Storage_Address));
      pragma Check (Debug,
         Check => R /= 0 or else Debug.Runtime_Error ("HeapFree failed"));
   end Free;

   function Reallocate (
      Storage_Address : Address;
      Size : Storage_Elements.Storage_Count)
      return Address
   is
      Actual_Size : constant C.basetsd.SIZE_T :=
         Round_Up (C.basetsd.SIZE_T (Size));
      Heap : constant C.winnt.HANDLE := C.winbase.GetProcessHeap;
      Result : Address;
   begin
      Result := Address (
         C.winbase.HeapReAlloc (
            Heap,
            0,
            C.windef.LPVOID (Storage_Address),
            Actual_Size));
      if Result = Null_Address then
         if Storage_Address = Null_Address then
            --  Reallocate (null, ...)
            Result := Address (C.winbase.HeapAlloc (Heap, 0, Actual_Size));
         end if;
      end if;
      return Result;
   end Reallocate;

   function Page_Size return Storage_Elements.Storage_Count is
      Info : aliased C.winbase.SYSTEM_INFO;
   begin
      C.winbase.GetSystemInfo (Info'Access);
      return Storage_Elements.Storage_Offset (Info.dwPageSize);
   end Page_Size;

   function Map (
      Size : Storage_Elements.Storage_Count)
      return Address
   is
      use type C.windef.DWORD;
      Mapped_Address : C.windef.LPVOID;
   begin
      Mapped_Address := C.winbase.VirtualAlloc (
         C.windef.LPVOID (Null_Address),
         C.basetsd.SIZE_T (Size),
         C.winnt.MEM_RESERVE or C.winnt.MEM_COMMIT,
         C.winnt.PAGE_READWRITE);
      return Address (Mapped_Address);
   end Map;

   function Map (
      Storage_Address : Address;
      Size : Storage_Elements.Storage_Count)
      return Address
   is
      pragma Unreferenced (Storage_Address);
      pragma Unreferenced (Size);
   begin
      --  VirtualAlloc and VirtualFree should be one-to-one correspondence
      return Null_Address;
   end Map;

   procedure Unmap (
      Storage_Address : Address;
      Size : Storage_Elements.Storage_Count) is
   begin
      declare
         R : C.windef.WINBOOL;
      begin
         R := C.winbase.VirtualFree (
            C.windef.LPVOID (Storage_Address),
            C.basetsd.SIZE_T (Size),
            C.winnt.MEM_DECOMMIT);
         pragma Check (Debug,
            Check =>
               R /= 0
               or else Debug.Runtime_Error (
                  "VirtualFree (..., MEM_DECOMMIT) failed"));
      end;
      declare
         R : C.windef.WINBOOL;
      begin
         R := C.winbase.VirtualFree (
            C.windef.LPVOID (Storage_Address),
            0,
            C.winnt.MEM_RELEASE);
         pragma Check (Debug,
            Check =>
               R /= 0
               or else Debug.Runtime_Error (
                  "VirtualFree (..., MEM_RELEASE) failed"));
      end;
   end Unmap;

end System.System_Allocators;
