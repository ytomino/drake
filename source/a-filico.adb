with Ada.Unchecked_Conversion;
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
      function Cast is new Unchecked_Conversion (
         System.Address,
         System.Finalization_Root.Finalizable_Ptr);
   begin
      Object.F := Cast (
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
