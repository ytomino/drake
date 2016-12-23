with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
package body Ada.Containers.Access_Holders is
   use type Weak_Access_Holders.Data_Access;

   subtype Nonnull_Data_Access is not null Data_Access;

   function Upcast is
      new Unchecked_Conversion (
         Nonnull_Data_Access,
         System.Reference_Counting.Container);
   function Downcast is
      new Unchecked_Conversion (
         System.Reference_Counting.Container,
         Nonnull_Data_Access);

   type Data_Access_Access is access all Nonnull_Data_Access;
   type Container_Access is access all System.Reference_Counting.Container;

   function Upcast is
      new Unchecked_Conversion (Data_Access_Access, Container_Access);

   procedure Free_Data (X : in out System.Reference_Counting.Data_Access);
   procedure Free_Data (X : in out System.Reference_Counting.Data_Access) is
      procedure Unchecked_Free is
         new Unchecked_Deallocation (Data, Data_Access);
      Y : Data_Access := Downcast (X);
   begin
      Weak_Access_Holders.Clear_Weaks (
         Y.Super,
         Null_Data.Super'Unrestricted_Access);
      Free (Y.Item);
      Unchecked_Free (Y);
      X := null;
   end Free_Data;

   --  implementation

   function Null_Holder return Holder is
   begin
      return (Finalization.Controlled
         with Data => Null_Data'Unrestricted_Access);
   end Null_Holder;

   overriding function "=" (Left, Right : Holder) return Boolean is
   begin
      return Left.Data = Right.Data;
   end "=";

   function To_Holder (Source : Name) return Holder is
   begin
      return Result : Holder do
         if Source /= Null_Data.Item then
            Result.Data := new Data'((1, null), Source);
         end if;
      end return;
   end To_Holder;

   function Is_Null (Container : Holder) return Boolean is
   begin
      return Container.Data = Null_Data'Unrestricted_Access;
   end Is_Null;

   procedure Clear (Container : in out Holder) is
   begin
      Finalize (Container);
      Container.Data := Null_Data'Unrestricted_Access;
   end Clear;

   function Element (Container : Holder'Class) return Name is
   begin
      return Constant_Reference (Holder (Container));
   end Element;

   procedure Replace_Element (Target : in out Holder; Source : Name) is
   begin
      Clear (Target);
      if Source /= Null_Data.Item then
         Target.Data := new Data'((1, null), Source);
      end if;
   end Replace_Element;

   function Constant_Reference (Container : Holder) return Name is
   begin
      return Container.Data.Item;
   end Constant_Reference;

   procedure Assign (Target : in out Holder; Source : Holder) is
   begin
      System.Reference_Counting.Assign (
         Target => Upcast (Target.Data'Unchecked_Access),
         Source => Upcast (Source.Data'Unrestricted_Access),
         Free => Free_Data'Access);
   end Assign;

   procedure Move (Target : in out Holder; Source : in out Holder) is
   begin
      System.Reference_Counting.Move (
         Target => Upcast (Target.Data'Unchecked_Access),
         Source => Upcast (Source.Data'Unchecked_Access),
         Sentinel => Upcast (Null_Data'Unrestricted_Access),
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
      System.Reference_Counting.Adjust (Upcast (Object.Data'Unchecked_Access));
   end Adjust;

   overriding procedure Finalize (Object : in out Holder) is
   begin
      System.Reference_Counting.Clear (
         Target => Upcast (Object.Data'Unchecked_Access),
         Free => Free_Data'Access);
   end Finalize;

   package body Weak is

      function Downcast is
         new Unchecked_Conversion (
            Weak_Access_Holders.Data_Access,
            Data_Access);

      overriding function "=" (Left, Right : Weak_Holder) return Boolean is
      begin
         return Left.Super.Data = Right.Super.Data;
      end "=";

      function To_Weak_Holder (Source : Holder) return Weak_Holder is
      begin
         return Result : Weak_Holder :=
            (Finalization.Controlled
               with Super => (
                  Data => Source.Data.Super'Unchecked_Access,
                  Previous => <>,
                  Next => <>))
         do
            Adjust (Result);
         end return;
      end To_Weak_Holder;

      function Null_Weak_Holder return Weak_Holder is
      begin
         return (Finalization.Controlled with others => <>);
      end Null_Weak_Holder;

      function To_Holder (Source : Weak_Holder) return Holder is
      begin
         return Result : Holder do
            if not Is_Null (Source) then
               Result.Data := Downcast (Source.Super.Data);
               Adjust (Result);
            end if;
         end return;
      end To_Holder;

      function Is_Null (Container : Weak_Holder) return Boolean is
      begin
         return Container.Super.Data = Null_Data.Super'Unrestricted_Access;
      end Is_Null;

      procedure Clear (Container : in out Weak_Holder) is
      begin
         Finalize (Container);
         Initialize (Container);
      end Clear;

      procedure Assign (Target : in out Weak_Holder; Source : Holder) is
      begin
         Clear (Target);
         Target.Super.Data := Source.Data.Super'Unchecked_Access;
         Adjust (Target);
      end Assign;

      procedure Assign (Target : in out Holder; Source : Weak_Holder) is
      begin
         Clear (Target);
         Target.Data := Downcast (Source.Super.Data);
         Adjust (Target);
      end Assign;

      overriding procedure Initialize (Object : in out Weak_Holder) is
      begin
         Object.Super.Data := Null_Data.Super'Unrestricted_Access;
         Object.Super.Previous := null;
         Object.Super.Next := null;
      end Initialize;

      overriding procedure Adjust (Object : in out Weak_Holder) is
      begin
         if not Is_Null (Object) then
            Weak_Access_Holders.Add_Weak (Object.Super'Unchecked_Access);
         end if;
      end Adjust;

      overriding procedure Finalize (Object : in out Weak_Holder) is
      begin
         if not Is_Null (Object) then
            Weak_Access_Holders.Remove_Weak (Object.Super'Unchecked_Access);
         end if;
      end Finalize;

   end Weak;

end Ada.Containers.Access_Holders;
