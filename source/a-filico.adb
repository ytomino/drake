with System.Address_To_Named_Access_Conversions;
with System.Finalization_Implementation;
package body Ada.Finalization.List_Controller is
   pragma Suppress (All_Checks);
   use type System.Finalization_Root.Finalizable_Ptr;

   overriding procedure Finalize (Object : in out Simple_List_Controller) is
   begin
      System.Finalization_Implementation.Finalize_List (Object.F);
      Object.F := null;
   end Finalize;

   overriding procedure Finalize (Object : in out List_Controller) is
      package Conv is new System.Address_To_Named_Access_Conversions (
         System.Finalization_Root.Root_Controlled'Class,
         System.Finalization_Root.Finalizable_Ptr);
   begin
      Object.F := Conv.To_Pointer (
         System.Finalization_Implementation.Collection_Finalization_Started);
      while Object.Item.Next /= Object.Item'Unchecked_Access loop
         System.Finalization_Implementation.Finalize_One (
            Object.Item.Next.all);
      end loop;
   end Finalize;

   overriding procedure Initialize (Object : in out List_Controller) is
   begin
      Object.F := Object.Item'Unchecked_Access;
      Object.Item.Next := Object.Item'Unchecked_Access;
      Object.Item.Prev := Object.Item'Unchecked_Access;
   end Initialize;

end Ada.Finalization.List_Controller;
