with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System.Address_To_Named_Access_Conversions;
package body Ada.Containers.Counted_Access_Holders is
   use type System.Reference_Counting.Counter;

   package Data_Cast is
      new System.Address_To_Named_Access_Conversions (Data, Data_Access);

   subtype Not_Null_Data_Access is not null Data_Access;
   type Data_Access_Access is access all Not_Null_Data_Access;
   type System_Address_Access is access all System.Address;
   function Upcast is new Unchecked_Conversion (
      Data_Access_Access,
      System_Address_Access);

   procedure Free_Data (X : System.Address);
   procedure Free_Data (X : System.Address) is
      procedure Unchecked_Free is new Unchecked_Deallocation (
         Data,
         Data_Access);
      Y : Data_Access := Data_Cast.To_Pointer (X);
   begin
      Weak.Clear (Y.all);
      Free (Y.Item);
      Unchecked_Free (Y);
   end Free_Data;

   --  implementation

   function "=" (Left, Right : Holder) return Boolean is
   begin
      return Left.Data = Right.Data;
   end "=";

   function To_Holder (Source : Name) return Holder is
   begin
      return Result : Holder do
         Result.Data := new Data'(Source, 1, null);
      end return;
   end To_Holder;

   function Null_Holder return Holder is
   begin
      return (Finalization.Controlled with Data => Default_Data'Access);
   end Null_Holder;

   function Is_Null (Container : Holder) return Boolean is
   begin
      return Container.Data.Item = Default_Data.Item;
   end Is_Null;

   procedure Clear (Container : in out Holder) is
   begin
      Finalize (Container);
      Container.Data := Default_Data'Access;
   end Clear;

   function Constant_Reference (Container : Holder) return Name is
   begin
      return Container.Data.Item;
   end Constant_Reference;

   function Element (Container : Holder'Class) return Name is
   begin
      return Constant_Reference (Container);
   end Element;

   procedure Replace_Element (Target : in out Holder; Source : Name) is
   begin
      Clear (Target);
      Target.Data := new Data'(Source, 1, null);
   end Replace_Element;

   procedure Assign (Target : in out Holder; Source : Holder) is
   begin
      System.Reference_Counting.Assign (
         Target => Upcast (Target.Data'Unchecked_Access),
         Target_Reference_Count => Target.Data.Reference_Count'Access,
         Source => Upcast (Source.Data'Unrestricted_Access),
         Source_Reference_Count => Source.Data.Reference_Count'Access,
         Free => Free_Data'Access);
   end Assign;

   procedure Move (
      Target : in out Holder;
      Source : in out Holder) is
   begin
      System.Reference_Counting.Move (
         Target => Upcast (Target.Data'Unchecked_Access),
         Target_Reference_Count => Target.Data.Reference_Count'Access,
         Source => Upcast (Source.Data'Unchecked_Access),
         Sentinel => Default_Data'Address,
         Free => Free_Data'Access);
   end Move;

   procedure Swap (I, J : in out Holder) is
      Temp : constant Data_Access := I.Data;
   begin
      I.Data := J.Data;
      J.Data := Temp;
   end Swap;

   overriding procedure Adjust (Object : in out Holder) is
   begin
      System.Reference_Counting.Adjust (Object.Data.Reference_Count'Access);
   end Adjust;

   overriding procedure Finalize (Object : in out Holder) is
   begin
      System.Reference_Counting.Clear (
         Target => Upcast (Object.Data'Unchecked_Access),
         Reference_Count => Object.Data.Reference_Count'Access,
         Free => Free_Data'Access);
   end Finalize;

   package body Weak is

      function "=" (Left, Right : Weak_Holder) return Boolean is
      begin
         if Is_Null (Left) then
            return Is_Null (Right);
         elsif Is_Null (Right) then
            return False;
         else
            return Left.Data = Right.Data;
         end if;
      end "=";

      function To_Weak_Holder (Source : Holder)
         return Weak_Holder is
      begin
         return Result : Weak_Holder := (Finalization.Controlled with
            Data => Data_Access (Source.Data),
            others => <>)
         do
            Adjust (Result);
         end return;
      end To_Weak_Holder;

      function Null_Weak_Holder return Weak_Holder is
      begin
         return (Finalization.Controlled with others => <>);
      end Null_Weak_Holder;

      function To_Holder (Source : Weak_Holder)
         return Holder is
      begin
         return Result : Holder do
            if not Is_Null (Source) then
               Result.Data := Counted_Access_Holders.Data_Access (Source.Data);
               Adjust (Result);
            end if;
         end return;
      end To_Holder;

      function Is_Null (Container : Weak_Holder) return Boolean is
      begin
         return Container.Data = null
            or else Container.Data.Item = Default_Data.Item;
      end Is_Null;

      procedure Clear (Container : in out Weak_Holder) is
      begin
         Finalize (Container);
         Container.Data := null;
         Container.Previous := null;
         Container.Next := null;
      end Clear;

      procedure Assign (
         Target : in out Weak_Holder;
         Source : Holder) is
      begin
         Clear (Target);
         Target.Data := Data_Access (Source.Data);
         Adjust (Target);
      end Assign;

      procedure Assign (
         Target : in out Holder;
         Source : Weak_Holder) is
      begin
         Clear (Target);
         if Source.Data /= null then
            Target.Data := Counted_Access_Holders.Data_Access (Source.Data);
            Adjust (Target);
         end if;
      end Assign;

      overriding procedure Adjust (Object : in out Weak_Holder) is
         procedure Add_To_Weak_List (A : not null Weak_Holder_Access);
         procedure Add_To_Weak_List (A : not null Weak_Holder_Access) is
         begin
            A.Previous := null;
            A.Data.Weak_List := Counted_Access_Holders.Weak_Holder_Access (A);
            A.Next := Weak_Holder_Access (A.Data.Weak_List);
            A.Next.Previous := A;
         end Add_To_Weak_List;
      begin
         if Object.Data /= null
            and then Object.Data.Reference_Count
               /= System.Reference_Counting.Static
         then
            Add_To_Weak_List (Object'Unrestricted_Access);
         end if;
      end Adjust;

      overriding procedure Finalize (Object : in out Weak_Holder) is
         procedure Remove_From_Weak_List (A : not null Weak_Holder_Access);
         procedure Remove_From_Weak_List (A : not null Weak_Holder_Access) is
         begin
            if A.Previous /= null then
               pragma Assert (A.Previous.Next = A);
               A.Previous.Next := A.Next;
            else
               pragma Assert (Weak_Holder_Access (A.Data.Weak_List) = A);
               A.Data.Weak_List :=
                  Counted_Access_Holders.Weak_Holder_Access (A.Next);
            end if;
            if A.Next /= null then
               pragma Assert (A.Next.Previous = A);
               A.Next.Previous := A.Previous;
            end if;
         end Remove_From_Weak_List;
      begin
         if Object.Data /= null
            and then Object.Data.Reference_Count
               /= System.Reference_Counting.Static
         then
            Remove_From_Weak_List (Object'Unrestricted_Access);
         end if;
      end Finalize;

      procedure Clear (Item : Data) is
         type T is access all Weak.Weak_Holder;
         I : T := T (Item.Weak_List);
      begin
         while I /= null loop
            declare
               Next : constant T := T (I.Next);
            begin
               I.Data := null;
               I.Previous := null;
               I.Next := null;
               I := Next;
            end;
         end loop;
      end Clear;

   end Weak;

end Ada.Containers.Counted_Access_Holders;
