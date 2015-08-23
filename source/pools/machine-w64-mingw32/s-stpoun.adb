with C.basetsd;
with C.winbase;
with C.windef;
package body System.Storage_Pools.Unbounded is
   use type Storage_Elements.Storage_Offset;
   use type C.windef.WINBOOL;
   use type C.winnt.HANDLE; -- C.void_ptr

   function Runtime_Error (
      S : String;
      Source_Location : String := Ada.Debug.Source_Location;
      Enclosing_Entity : String := Ada.Debug.Enclosing_Entity)
      return Boolean
      with Import, Convention => Ada, External_Name => "__drake_runtime_error";
   pragma Machine_Attribute (Runtime_Error, "noreturn");

   --  implementation

   overriding procedure Initialize (Object : in out Unbounded_Pool) is
   begin
      Object.Heap := C.winbase.HeapCreate (0, 0, 0);
   end Initialize;

   overriding procedure Finalize (Object : in out Unbounded_Pool) is
      R : C.windef.WINBOOL;
   begin
      R := C.winbase.HeapDestroy (Object.Heap);
      pragma Check (Debug,
         Check => R /= 0 or else Runtime_Error ("failed to HeapDestroy"));
   end Finalize;

   overriding procedure Allocate (
      Pool : in out Unbounded_Pool;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count) is
   begin
      Storage_Address := Address (
         C.winbase.HeapAlloc (
            Pool.Heap,
            0,
            C.basetsd.SIZE_T (Size_In_Storage_Elements)));
      if Storage_Address = Null_Address then
         raise Storage_Error;
      elsif Storage_Address mod Alignment /= 0 then
         Deallocate (
            Pool,
            Storage_Address,
            Size_In_Storage_Elements,
            Alignment);
         raise Storage_Error;
      end if;
   end Allocate;

   overriding procedure Deallocate (
      Pool : in out Unbounded_Pool;
      Storage_Address : Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count)
   is
      pragma Unreferenced (Size_In_Storage_Elements);
      pragma Unreferenced (Alignment);
      R : C.windef.WINBOOL;
   begin
      R := C.winbase.HeapFree (
         Pool.Heap,
         0,
         C.windef.LPVOID (Storage_Address));
      pragma Check (Debug,
         Check => R /= 0 or else Runtime_Error ("failed to HeapFree"));
   end Deallocate;

   overriding function Storage_Size (Pool : Unbounded_Pool)
      return Storage_Elements.Storage_Count
   is
      pragma Unreferenced (Pool);
   begin
      return Storage_Elements.Storage_Count'Last;
   end Storage_Size;

end System.Storage_Pools.Unbounded;
