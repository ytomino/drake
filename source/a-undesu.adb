procedure Ada.Unchecked_Deallocate_Subpool (
   Subpool : in out System.Storage_Pools.Subpools.Subpool_Handle)
is
   pragma Suppress (All_Checks);
begin
   System.Storage_Pools.Subpools.Unchecked_Deallocate_Subpool (Subpool);
end Ada.Unchecked_Deallocate_Subpool;
