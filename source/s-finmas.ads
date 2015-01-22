pragma License (Unrestricted);
--  implementation unit required by compiler for controlled types
with Ada.Finalization;
with System.Storage_Elements;
with System.Storage_Pools;
package System.Finalization_Masters is
   pragma Preelaborate;

   type Finalize_Address_Ptr is access procedure (Obj : Address);

   type FM_Node is private;
   type FM_Node_Ptr is access all FM_Node;

   Header_Size : constant Storage_Elements.Storage_Offset;
   Header_Offset : constant Storage_Elements.Storage_Offset;

   subtype Any_Storage_Pool_Ptr is Storage_Pools.Storage_Pool_Access;

   type Finalization_Master is
      limited new Ada.Finalization.Limited_Controlled with private;

   procedure Attach_Unprotected (N, L : not null FM_Node_Ptr);
   procedure Attach (N, L : not null FM_Node_Ptr);

   procedure Detach_Unprotected (N : not null FM_Node_Ptr);
   procedure Detach (N : not null FM_Node_Ptr);

   function Objects_Unprotected (
      Master : aliased in out Finalization_Master'Class;
      Fin_Addr_Ptr : Finalize_Address_Ptr)
      return FM_Node_Ptr;

   function Finalization_Started (Master : Finalization_Master'Class)
      return Boolean;
   pragma Inline (Finalization_Started);

   procedure Set_Finalize_Address_Unprotected (
      Master : in out Finalization_Master'Class;
      Fin_Addr_Ptr : Finalize_Address_Ptr);

   --  required by compiler (s-finmas.ads)
   procedure Set_Finalize_Address (
      Master : in out Finalization_Master'Class;
      Fin_Addr_Ptr : Finalize_Address_Ptr);

   --  required by compiler (s-finmas.ads)
   procedure Set_Is_Heterogeneous (
      Master : in out Finalization_Master'Class) is null;

   --  required by compiler (s-finmas.ads)
   type Finalization_Master_Ptr is access all Finalization_Master;
   for Finalization_Master_Ptr'Storage_Size use 0;

private
   use type Storage_Elements.Storage_Offset;

   type FM_Node is record
      Prev : FM_Node_Ptr;
      Next : FM_Node_Ptr;
   end record;
   pragma Suppress_Initialization (FM_Node);

   Header_Size : constant Storage_Elements.Storage_Offset :=
      FM_Node'Size / Storage_Unit;
   Header_Offset : constant Storage_Elements.Storage_Offset :=
      FM_Node'Size / Storage_Unit;

   type FM_List;
   type FM_List_Access is access all FM_List;
   type FM_List is limited record
      Objects : aliased FM_Node;
      Finalize_Address : Finalize_Address_Ptr;
      Next : FM_List_Access;
   end record;
   pragma Suppress_Initialization (FM_List);

   type Finalization_State is range 0 .. 1;
   for Finalization_State'Size use 8;
   pragma Atomic (Finalization_State);
   FS_Active : constant Finalization_State := 0;
   FS_Finalization_Started : constant Finalization_State := 1;

   type Finalization_Master is
      limited new Ada.Finalization.Limited_Controlled with
   record
      List : aliased FM_List;
      Base_Pool : Any_Storage_Pool_Ptr := null;
      Finalization_State : aliased Finalization_Masters.Finalization_State :=
         FS_Active;
   end record;

   overriding procedure Initialize (Object : in out Finalization_Master);
   overriding procedure Finalize (Object : in out Finalization_Master);

   --  required by compiler (s-finmas.ads)
   function Base_Pool (Master : Finalization_Master'Class)
      return Any_Storage_Pool_Ptr;
   pragma Inline (Base_Pool);
   procedure Set_Base_Pool (
      Master : in out Finalization_Master'Class;
      Pool_Ptr : Any_Storage_Pool_Ptr);
   pragma Inline (Set_Base_Pool);

   --  required by compiler (s-finmas.ads)
   function Add_Offset_To_Address (
      Addr : Address;
      Offset : Storage_Elements.Storage_Offset)
      return Address
      renames Storage_Elements."+";

end System.Finalization_Masters;
