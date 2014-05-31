with Ada.Unchecked_Conversion;
with System.Unwind.Raising; -- raising exception in compiler unit
with System.Unwind.Standard;
with C.basetsd;
with C.winbase;
with C.windef;
with C.winnt;
package body System.Standard_Allocators is
   pragma Suppress (All_Checks);
   use type C.void_ptr;
   use type C.windef.WINBOOL;
--  use type C.basetsd.SIZE_T;
--  use type C.windef.DWORD;

   procedure Runtime_Error (
      Condition : Boolean;
      S : String;
      Source_Location : String := Ada.Debug.Source_Location;
      Enclosing_Entity : String := Ada.Debug.Enclosing_Entity);
   pragma Import (Ada, Runtime_Error, "__drake_runtime_error");

   Heap_Exhausted : constant String := "heap exhausted";

   --  implementation

   function Allocate (Size : Storage_Elements.Storage_Count)
      return Address
   is
      use type C.basetsd.SIZE_T;
      function Cast is new Ada.Unchecked_Conversion (C.windef.LPVOID, Address);
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
      return Result : constant Address := Cast (C.winbase.HeapAlloc (
         C.winbase.GetProcessHeap,
         0,
         Actual_Size))
      do
         if Result = Null_Address then
            Unwind.Raising.Raise_Exception_From_Here_With (
               Unwind.Standard.Storage_Error'Access,
               Message => Heap_Exhausted);
         end if;
      end return;
   end Allocate;

   procedure Free (Storage_Address : Address) is
      R : C.windef.WINBOOL;
   begin
      R := C.winbase.HeapFree (
         C.winbase.GetProcessHeap,
         0,
         C.windef.LPVOID (Storage_Address));
      pragma Debug (Runtime_Error (R = 0, "failed to HeapFree"));
   end Free;

   function Reallocate (
      Storage_Address : Address;
      Size : Storage_Elements.Storage_Count)
      return Address
   is
      function Cast is new Ada.Unchecked_Conversion (C.void_ptr, Address);
   begin
      return Result : Address := Cast (C.winbase.HeapReAlloc (
         C.winbase.GetProcessHeap,
         0,
         C.windef.LPVOID (Storage_Address),
         C.basetsd.SIZE_T (Storage_Elements.Storage_Count'Max (1, Size))))
      do
         if Result = Null_Address then
            if Storage_Address = Null_Address then
               Result := Allocate (Size); -- Reallocate (null, ...)
            else
               Unwind.Raising.Raise_Exception_From_Here_With (
                  Unwind.Standard.Storage_Error'Access,
                  Message => Heap_Exhausted);
            end if;
         end if;
      end return;
   end Reallocate;

   Page_Exhausted : constant String := "page exhausted";

   function Page_Size return Storage_Elements.Storage_Count is
      Info : aliased C.winbase.SYSTEM_INFO;
   begin
      C.winbase.GetSystemInfo (Info'Access);
      return Storage_Elements.Storage_Count (Info.dwPageSize);
   end Page_Size;

   function Map (
      Size : Storage_Elements.Storage_Count;
      Raise_On_Error : Boolean := True)
      return Address
   is
      use type C.windef.DWORD;
      function Cast is new Ada.Unchecked_Conversion (C.windef.LPVOID, Address);
      Mapped_Address : C.windef.LPVOID;
   begin
      Mapped_Address := C.winbase.VirtualAlloc (
         C.windef.LPVOID (Null_Address),
         C.basetsd.SIZE_T (Size),
         C.winnt.MEM_RESERVE or C.winnt.MEM_COMMIT,
         C.winnt.PAGE_READWRITE);
      if Mapped_Address = C.windef.LPVOID (Null_Address)
         and then Raise_On_Error
      then
         Unwind.Raising.Raise_Exception_From_Here_With (
            Unwind.Standard.Storage_Error'Access,
            Message => Page_Exhausted);
      end if;
      return Cast (Mapped_Address);
   end Map;

   function Map (
      Storage_Address : Address;
      Size : Storage_Elements.Storage_Count;
      Raise_On_Error : Boolean := True)
      return Address
   is
      pragma Unreferenced (Storage_Address);
      pragma Unreferenced (Size);
   begin
      --  VirtualAlloc and VirtualFree should be one-to-one correspondence
      if Raise_On_Error then
         Unwind.Raising.Raise_Exception_From_Here_With (
            Unwind.Standard.Storage_Error'Access,
            Message => Page_Exhausted);
      end if;
      return Null_Address;
   end Map;

   procedure Unmap (
      Storage_Address : Address;
      Size : Storage_Elements.Storage_Count) is
      R : C.windef.WINBOOL;
   begin
      R := C.winbase.VirtualFree (
         C.windef.LPVOID (Storage_Address),
         C.basetsd.SIZE_T (Size),
         C.winnt.MEM_DECOMMIT);
      pragma Debug (Runtime_Error (R = 0,
         "failed to VirtualFree (..., MEM_DECOMMIT)"));
      R := C.winbase.VirtualFree (
         C.windef.LPVOID (Storage_Address),
         0,
         C.winnt.MEM_RELEASE);
      pragma Debug (Runtime_Error (R = 0,
         "failed to VirtualFree (..., MEM_RELEASE)"));
   end Unmap;

end System.Standard_Allocators;
