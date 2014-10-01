pragma License (Unrestricted);
--  Ada 2012
private with System.Finalization_Masters;
package System.Storage_Pools.Subpools is
   pragma Preelaborate;

   type Root_Storage_Pool_With_Subpools is
      abstract limited new Root_Storage_Pool with private;

   type Root_Subpool is abstract tagged limited private;

   type Subpool_Handle is access all Root_Subpool'Class;
   for Subpool_Handle'Storage_Size use 0;

   function Create_Subpool (Pool : in out Root_Storage_Pool_With_Subpools)
      return not null Subpool_Handle is abstract;

   --  The following operations are intended for pool implementers:

--  function Pool_of_Subpool (
   --  RM defined Pool_*of*_Subpool,
   --  but GNAT runtime defined Pool_*Of*_Subpool...
   function Pool_Of_Subpool (
      Subpool : not null Subpool_Handle)
      return access Root_Storage_Pool_With_Subpools'Class;

--  procedure Set_Pool_of_Subpool (
   --  RM defined Set_Pool_*of*_Subpool,
   --  but GNAT runtime defined Set_Pool_*Of*_Subpool...
   procedure Set_Pool_Of_Subpool (
      Subpool : not null Subpool_Handle;
      To : in out Root_Storage_Pool_With_Subpools'Class);

   procedure Allocate_From_Subpool (
      Pool : in out Root_Storage_Pool_With_Subpools;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count;
      Subpool : not null Subpool_Handle) is abstract;
--       with Pre'Class => Pool_of_Subpool(Subpool) = Pool'Access;

   procedure Deallocate_Subpool (
      Pool : in out Root_Storage_Pool_With_Subpools;
      Subpool : in out Subpool_Handle) is abstract;
--       with Pre'Class => Pool_of_Subpool(Subpool) = Pool'Access;

--  function Default_Subpool_for_Pool (
   --  RM defined Default_Subpool_*for*_Pool,
   --  but GNAT runtime defined Default_Subpool_*For*_Pool...
   function Default_Subpool_For_Pool (
      Pool : Root_Storage_Pool_With_Subpools)
      return not null Subpool_Handle;

   overriding procedure Allocate (
      Pool : in out Root_Storage_Pool_With_Subpools;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count);

   overriding procedure Deallocate (
      Pool : in out Root_Storage_Pool_With_Subpools;
      Storage_Address : Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count) is null;

   overriding function Storage_Size (Pool : Root_Storage_Pool_With_Subpools)
      return Storage_Elements.Storage_Count is
      (Storage_Elements.Storage_Count'Last);

   --  extended
   --  This is same as/called from Ada.Unchecked_Deallocate_Subpool.
   procedure Unchecked_Deallocate_Subpool (Subpool : in out Subpool_Handle);

private
   use type Storage_Elements.Storage_Offset;

   type Finalization_State is range 0 .. 1;
   for Finalization_State'Size use 8;
   pragma Atomic (Finalization_State);
   FS_Active : constant Finalization_State := 0;
   FS_Finalization_Started : constant Finalization_State := 1;

   type Root_Storage_Pool_With_Subpools is
      abstract limited new Root_Storage_Pool with
   record
      Last : Subpool_Handle := null;
      --  state
      Finalization_State : aliased Subpools.Finalization_State := FS_Active;
      pragma Atomic (Finalization_State);
   end record;

   overriding procedure Initialize (
      Object : in out Root_Storage_Pool_With_Subpools);
   overriding procedure Finalize (
      Object : in out Root_Storage_Pool_With_Subpools);

   type Root_Storage_Pool_With_Subpools_Access is
      access all Root_Storage_Pool_With_Subpools'Class;
   for Root_Storage_Pool_With_Subpools_Access'Storage_Size use 0;

   --  Root_Subpool

   type Root_Subpool is abstract tagged limited record
      Owner : Root_Storage_Pool_With_Subpools_Access := null;
      Previous : Subpool_Handle := null;
      Next : Subpool_Handle := null;
      --  for owned objects
      Master : aliased Finalization_Masters.Finalization_Master;
   end record;

   --  required by compiler (s-stposu.ads)
   procedure Allocate_Any_Controlled (
      Pool : in out Root_Storage_Pool'Class;
      Context_Subpool : Subpool_Handle;
      Context_Master : Finalization_Masters.Finalization_Master_Ptr;
      Fin_Address : Finalization_Masters.Finalize_Address_Ptr;
      Addr : out Address;
      Storage_Size : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count;
      Is_Controlled : Boolean;
      On_Subpool : Boolean);

   --  required by compiler (s-stposu.ads)
   procedure Deallocate_Any_Controlled (
      Pool : in out Root_Storage_Pool'Class;
      Addr : Address;
      Storage_Size : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count;
      Is_Controlled : Boolean);

   --  required by compiler (s-stposu.ads)
   function Header_Size_With_Padding (
      Alignment : Storage_Elements.Storage_Count)
      return Storage_Elements.Storage_Count;
   pragma Inline (Header_Size_With_Padding);

   --  required for checked pool by compiler (s-stposu.ads)
--  procedure Adjust_Controlled_Dereference (
--    Addr : in out Address;
--    Storage_Size : in out Storage_Elements.Storage_Count;
--    Alignment : Storage_Elements.Storage_Count);

end System.Storage_Pools.Subpools;
