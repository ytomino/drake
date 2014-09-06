pragma License (Unrestricted);
--  Ada 2012
with System.Storage_Pools.Subpools;
procedure Ada.Unchecked_Deallocate_Subpool (
   Subpool : in out System.Storage_Pools.Subpools.Subpool_Handle);
pragma Preelaborate (Ada.Unchecked_Deallocate_Subpool);
pragma Inline (Ada.Unchecked_Deallocate_Subpool);
