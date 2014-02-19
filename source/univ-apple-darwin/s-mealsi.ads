pragma License (Unrestricted);
--  implementation unit specialized for Darwin
function System.Memory.Allocated_Size (
   Storage_Address : Address)
   return Storage_Elements.Storage_Count;
pragma Preelaborate (System.Memory.Allocated_Size);
