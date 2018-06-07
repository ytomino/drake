with Ada.Exceptions.Finally;
with System.Address_To_Named_Access_Conversions;
with System.Debug;
with System.Standard_Allocators;
with C.basetsd;
with C.winbase;
with C.windef;
package body System.Unbounded_Allocators is
   use type Storage_Elements.Storage_Offset;
   use type C.size_t;
--   use type C.windef.DWORD;
   use type C.windef.WORD;
   use type C.windef.WINBOOL;
   use type C.winnt.HANDLE; -- C.void_ptr

   package HANDLE_ptr_Conv is
      new Address_To_Named_Access_Conversions (
         C.winnt.HANDLE,
         C.winnt.HANDLE_ptr);

   type HANDLE_array is array (C.size_t range <>) of C.winnt.HANDLE
      with Convention => C;

   function Is_In (Storage_Address : Address; Heap : C.winnt.HANDLE)
      return Boolean;
   function Is_In (Storage_Address : Address; Heap : C.winnt.HANDLE)
      return Boolean
   is
      Result : Boolean := False;
   begin
      Result :=
         C.winbase.HeapValidate (
            Heap,
            0,
            C.void_const_ptr (Storage_Address)) /=
         C.windef.FALSE;
--    Result := False;
--    Dummy := C.winbase.HeapLock (Heap);
--    declare
--       Heap_Entry : aliased C.winbase.PROCESS_HEAP_ENTRY :=
--          (lpData => C.void_ptr (Null_Address), others => <>);
--    begin
--       while C.winbase.HeapWalk (Heap, Heap_Entry'Access) /=
--          C.windef.FALSE
--       loop
--          if (Heap_Entry.wFlags and C.winbase.PROCESS_HEAP_REGION) /= 0
--             and then Storage_Address >=
--                Address (Heap_Entry.anonymous_1.Region.lpFirstBlock)
--             and then Storage_Address <
--                Address (Heap_Entry.anonymous_1.Region.lpLastBlock)
--          then
--             Result := True;
--             exit;
--          end if;
--       end loop;
--    end;
--    Dummy := C.winbase.HeapUnlock (Heap);
      return Result;
   end Is_In;

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
      Allocator : Unbounded_Allocator;
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
      Allocator : Unbounded_Allocator;
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

   function Allocator_Of (Storage_Address : Address)
      return Unbounded_Allocator
   is
      procedure Finally (X : in out C.winnt.HANDLE_ptr);
      procedure Finally (X : in out C.winnt.HANDLE_ptr) is
         pragma Unmodified (X);
      begin
         Standard_Allocators.Free (HANDLE_ptr_Conv.To_Address (X));
      end Finally;
      package Holder is
         new Ada.Exceptions.Finally.Scoped_Holder (
            C.winnt.HANDLE_ptr,
            Finally);
      Buffer_Capacity : C.size_t := 64;
      Buffer_Length : C.size_t;
      Buffer : aliased C.winnt.HANDLE_ptr :=
         HANDLE_ptr_Conv.To_Pointer (
            Standard_Allocators.Allocate (
               Storage_Elements.Storage_Offset (
                  C.winnt.HANDLE'Size / Standard'Storage_Unit
                     * Buffer_Capacity)));
   begin
      Holder.Assign (Buffer);
      loop
         Buffer_Length :=
            C.size_t (
               C.winbase.GetProcessHeaps (
                  C.windef.DWORD (Buffer_Capacity),
                  Buffer));
         if Buffer_Length = 0 then
            raise Program_Error; -- GetProcessHeaps failed
         end if;
         exit when Buffer_Length <= Buffer_Capacity;
         Buffer_Capacity := Buffer_Capacity * 2;
         Buffer :=
            HANDLE_ptr_Conv.To_Pointer (
               Standard_Allocators.Reallocate (
                  HANDLE_ptr_Conv.To_Address (Buffer),
                  Storage_Elements.Storage_Offset (
                     C.winnt.HANDLE'Size / Standard'Storage_Unit
                        * Buffer_Capacity)));
      end loop;
      declare
         Heaps : HANDLE_array (0 .. Buffer_Length - 1);
         for Heaps'Address use HANDLE_ptr_Conv.To_Address (Buffer);
      begin
         for I in Heaps'Range loop
            if Is_In (Storage_Address, Heaps (I)) then
               return Unbounded_Allocator (Heaps (I)); -- found
            end if;
         end loop;
      end;
      raise Program_Error; -- not found
   end Allocator_Of;

end System.Unbounded_Allocators;
