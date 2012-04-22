with System.Address_To_Named_Access_Conversions;
package body Ada.Tags.Inside is
   pragma Suppress (All_Checks);

   package Tag_Ptr_Conv is new System.Address_To_Named_Access_Conversions (
      Tag,
      Tag_Ptr);

   package TSD_Ptr_Conv is new System.Address_To_Named_Access_Conversions (
      Type_Specific_Data,
      Type_Specific_Data_Ptr);

   function CW_Membership (This : System.Address; T : Tag) return Boolean is
      Base_Object : constant System.Address := Base_Address (This);
      Base_Tag : constant Tag := Tag_Ptr_Conv.To_Pointer (Base_Object).all;
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
      TSD : constant Type_Specific_Data_Ptr :=
         TSD_Ptr_Conv.To_Pointer (DT (T).TSD);
   begin
      return TSD.RC_Offset;
   end Get_RC_Offset;

   function Parent_Size (Obj : System.Address; T : Tag)
      return System.Storage_Elements.Storage_Count
   is
      TSD : constant Type_Specific_Data_Ptr :=
         TSD_Ptr_Conv.To_Pointer (DT (T).TSD);
      Parent_Tag : constant Tag := TSD.Tags_Table (1);
      Parent_TSD : constant Type_Specific_Data_Ptr :=
         TSD_Ptr_Conv.To_Pointer (DT (Parent_Tag).TSD);
   begin
      return System.Storage_Elements.Storage_Count (
         Parent_TSD.Size_Func.all (Obj));
   end Parent_Size;

end Ada.Tags.Inside;
