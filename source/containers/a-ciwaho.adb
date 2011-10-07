package body Ada.Containers.Inside.Weak_Access_Holders is

   procedure Add_Weak (Item : Weak_Holder_Access) is
   begin
      Item.Previous := null;
      Item.Next := Item.Data.Weak_List;
      Item.Next.Previous := Item;
      Item.Data.Weak_List := Item;
   end Add_Weak;

   procedure Remove_Weak (Item : Weak_Holder_Access) is
   begin
      if Item.Previous /= null then
         pragma Assert (Item.Previous.Next = Item);
         Item.Previous.Next := Item.Next;
      else
         pragma Assert (Item.Data.Weak_List = Item);
         Item.Data.Weak_List := Item.Next;
      end if;
      if Item.Next /= null then
         pragma Assert (Item.Next.Previous = Item);
         Item.Next.Previous := Item.Previous;
      end if;
   end Remove_Weak;

   procedure Clear_Weaks (
      List : in out Data;
      Null_Data : not null Data_Access)
   is
      I : Weak_Holder_Access := List.Weak_List;
   begin
      while I /= null loop
         declare
            Next : constant Weak_Holder_Access := I.Next;
         begin
            I.Data := Null_Data;
            I.Previous := null;
            I.Next := null;
            I := Next;
         end;
      end loop;
   end Clear_Weaks;

end Ada.Containers.Inside.Weak_Access_Holders;
