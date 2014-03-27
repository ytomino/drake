with Ada.Containers.Binary_Trees.Arne_Andersson.Debug;
with Ada.Unchecked_Conversion;
package body Ada.Containers.Ordered_Sets.Debug is

   function Downcast is
      new Unchecked_Conversion (Copy_On_Write.Data_Access, Data_Access);

   procedure Dump (Source : Set) is
      Dummy : Boolean;
      pragma Unreferenced (Dummy);
   begin
      Dummy := Base.Debug.Dump (
         Downcast (Source.Super.Data).Root,
         Marker => null);
   end Dump;

   function Validate (Source : Set) return Boolean is
   begin
      return Base.Debug.Validate (
         Downcast (Source.Super.Data).Root,
         Downcast (Source.Super.Data).Length);
   end Validate;

end Ada.Containers.Ordered_Sets.Debug;
