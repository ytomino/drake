with Ada.Unchecked_Conversion;
package body Ada.Tags.Inside is

   function CW_Membership (This : System.Address; T : Tag) return Boolean is
      function Cast is new Unchecked_Conversion (System.Address, Tag_Ptr);
      Base_Object : constant System.Address := Base_Address (This);
      Base_Tag : constant Tag := Cast (Base_Object).all;
   begin
      return Is_Descendant (
         Base_Tag,
         T,
         Primary_Only => True,
         Same_Level => False);
   end CW_Membership;

   function Get_RC_Offset (T : Tag)
      return System.Storage_Elements.Storage_Offset
   is
      function Cast is new Unchecked_Conversion (
         System.Address,
         Type_Specific_Data_Ptr);
      TSD : constant Type_Specific_Data_Ptr := Cast (DT (T).TSD);
   begin
      return TSD.RC_Offset;
   end Get_RC_Offset;

   function Parent_Size (Obj : System.Address; T : Tag)
      return System.Storage_Elements.Storage_Count
   is
      function Cast is new Unchecked_Conversion (
         System.Address,
         Type_Specific_Data_Ptr);
      TSD : constant Type_Specific_Data_Ptr := Cast (DT (T).TSD);
      Parent_Tag : constant Tag := TSD.Tags_Table (1);
      Parent_TSD : constant Type_Specific_Data_Ptr :=
         Cast (DT (Parent_Tag).TSD);
   begin
      return System.Storage_Elements.Storage_Count (
         Parent_TSD.Size_Func.all (Obj));
   end Parent_Size;

end Ada.Tags.Inside;
