pragma License (Unrestricted);
--  implementation unit specialized for Darwin
function System.Native_Allocators.Allocated_Size (
   Storage_Address : Address)
   return Storage_Elements.Storage_Count;
pragma Preelaborate (System.Native_Allocators.Allocated_Size);
