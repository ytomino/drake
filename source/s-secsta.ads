pragma License (Unrestricted);
--  implementation unit required by compiler
with System.Storage_Elements;
package System.Secondary_Stack is
   pragma Preelaborate;

   --  required for secondary stack by compiler (s-secsta.ads)
   SS_Pool : Integer; -- unused, but compiler requires

   type Mark_Id is record
      Sstk : Address;
      Sptr : Address;
   end record;
   pragma Suppress_Initialization (Mark_Id);

   function SS_Mark return Mark_Id;

   procedure SS_Allocate (
      Addr : out Address;
      Storage_Size : Storage_Elements.Storage_Count);

   procedure SS_Release (M : Mark_Id);

   --  for Tasking.Inside
   procedure Clear;

   --  required by compiler ??? (s-secsta.ads)
--  Default_Secondary_Stack_Size : Natural;

private

   type Block is record
      Previous : Address;
      Limit : Address; -- Last + 1
      Used : Address;
   end record;
   pragma Suppress_Initialization (Block);

   type Block_Access is access all Block;

end System.Secondary_Stack;
