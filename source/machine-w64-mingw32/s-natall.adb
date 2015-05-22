with Ada;
with C.basetsd;
with C.winbase;
with C.windef;
with C.winnt;
package body System.Native_Allocators is
   pragma Suppress (All_Checks);
   use type C.windef.WINBOOL;
--  use type C.basetsd.SIZE_T;
--  use type C.windef.DWORD;

   function Runtime_Error (
      S : String;
      Source_Location : String := Ada.Debug.Source_Location;
      Enclosing_Entity : String := Ada.Debug.Enclosing_Entity)
      return Boolean
      with Import, Convention => Ada, External_Name => "__drake_runtime_error";
   pragma Machine_Attribute (Runtime_Error, "noreturn");

   --  implementation

   function Allocate (Size : Storage_Elements.Storage_Count)
      return Address
   is
      use type C.basetsd.SIZE_T;
      Actual_Size : C.basetsd.SIZE_T := C.basetsd.SIZE_T (Size);
   begin
      --  do round up here since HeapSize always returns the same size
      --  that is passed to HeapAlloc
      --  heap memory is separated to 16, 24, 32... by Windows heap manager
      if Actual_Size < 16 then
         Actual_Size := 16;
      else
         Actual_Size := (Actual_Size + 7) and not 7;
      end if;
      return Address (
         C.winbase.HeapAlloc (
            C.winbase.GetProcessHeap,
            0,
            Actual_Size));
   end Allocate;

   procedure Free (Storage_Address : Address) is
      R : C.windef.WINBOOL;
   begin
      R := C.winbase.HeapFree (
         C.winbase.GetProcessHeap,
         0,
         C.windef.LPVOID (Storage_Address));
      pragma Check (Debug,
         Check => R /= 0 or else Runtime_Error ("failed to HeapFree"));
   end Free;

   function Reallocate (
      Storage_Address : Address;
      Size : Storage_Elements.Storage_Count)
      return Address is
   begin
      return Result : Address := Address (C.winbase.HeapReAlloc (
         C.winbase.GetProcessHeap,
         0,
         C.windef.LPVOID (Storage_Address),
         C.basetsd.SIZE_T (Storage_Elements.Storage_Count'Max (1, Size))))
      do
         if Result = Null_Address then
            if Storage_Address = Null_Address then
               Result := Allocate (Size); -- Reallocate (null, ...)
            end if;
         end if;
      end return;
   end Reallocate;

   function Page_Size return Storage_Elements.Storage_Count is
      Info : aliased C.winbase.SYSTEM_INFO;
   begin
      C.winbase.GetSystemInfo (Info'Access);
      return Storage_Elements.Storage_Count (Info.dwPageSize);
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
      Size : Storage_Elements.Storage_Count)
   is
      R : C.windef.WINBOOL;
   begin
      R := C.winbase.VirtualFree (
         C.windef.LPVOID (Storage_Address),
         C.basetsd.SIZE_T (Size),
         C.winnt.MEM_DECOMMIT);
      pragma Check (Debug,
         Check => R /= 0
            or else Runtime_Error (
               "failed to VirtualFree (..., MEM_DECOMMIT)"));
      R := C.winbase.VirtualFree (
         C.windef.LPVOID (Storage_Address),
         0,
         C.winnt.MEM_RELEASE);
      pragma Check (Debug,
         Check => R /= 0
            or else Runtime_Error (
               "failed to VirtualFree (..., MEM_RELEASE)"));
   end Unmap;

end System.Native_Allocators;
