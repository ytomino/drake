pragma License (Unrestricted);
--  implementation unit required by compiler
with System.Storage_Pools.Standard_Pools;
package System.Pool_Global is
   pragma Preelaborate;

   subtype Unbounded_No_Reclaim_Pool is
      Storage_Pools.Standard_Pools.Standard_Pool;

   pragma Suppress (All_Checks); -- for renaming

   --  required for default of 'Storage_Pool by compiler (s-pooglo.ads)
   Global_Pool_Object : Unbounded_No_Reclaim_Pool
      renames Storage_Pools.Standard_Pools.Standard_Storage_Pool.all;

end System.Pool_Global;
