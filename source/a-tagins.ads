pragma License (Unrestricted);
--  implementation unit
with System.Storage_Elements;
package Ada.Tags.Inside is
   pragma Preelaborate;

   --  for System.Finalization_Implementation (a-tags.ads)

   function Get_RC_Offset (T : Tag)
      return System.Storage_Elements.Storage_Offset;

   function Parent_Size (Obj : System.Address; T : Tag)
      return System.Storage_Elements.Storage_Count;

   --  more support for System.Finalization_Implementation (a-tags.adb)

   function CW_Membership (This : System.Address; T : Tag) return Boolean;

end Ada.Tags.Inside;
