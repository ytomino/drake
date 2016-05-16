with Ada.Containers.Binary_Trees.Arne_Andersson.Debug;
with Ada.Unchecked_Conversion;
package body Ada.Containers.Ordered_Sets.Debug is
   use type Copy_On_Write.Data_Access;

   function Downcast is
      new Unchecked_Conversion (Copy_On_Write.Data_Access, Data_Access);

   procedure Dump (Source : Set; Message : String := "") is
      Container : Binary_Trees.Node_Access;
      Dummy : Boolean;
   begin
      if Source.Super.Data = null then
         Container := null;
      else
         Container := Downcast (Source.Super.Data).Root;
      end if;
      Dummy := Base.Debug.Dump (
         Container => Container,
         Marker => null,
         Message => Message);
   end Dump;

   function Valid (Source : Set) return Boolean is
      Container : Binary_Trees.Node_Access;
      Length : Count_Type;
   begin
      if Source.Super.Data = null then
         Container := null;
         Length := 0;
      else
         Container := Downcast (Source.Super.Data).Root;
         Length := Downcast (Source.Super.Data).Length;
      end if;
      return Base.Debug.Valid (Container => Container, Length => Length);
   end Valid;

end Ada.Containers.Ordered_Sets.Debug;
