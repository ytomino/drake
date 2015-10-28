pragma License (Unrestricted);
--  implementation unit required by compiler
with System.Storage_Elements;
with System.Unbounded_Stack_Allocators;
package System.Secondary_Stack is
   pragma Preelaborate;

   --  required for secondary stack by compiler (s-secsta.ads)
   SS_Pool : Integer; -- unused, but compiler requires

   procedure SS_Allocate (
      Addr : out Address;
      Storage_Size : Storage_Elements.Storage_Count);

   subtype Mark_Id is Unbounded_Stack_Allocators.Marker;

   function SS_Mark return Mark_Id;

   procedure SS_Release (M : Mark_Id);

   --  required by compiler ??? (s-secsta.ads)
--  Default_Secondary_Stack_Size : Natural;

end System.Secondary_Stack;
