package body Ada.Containers.Intrusive_Counted_Access_Holders is

   function "=" (Left, Right : Holder) return Boolean is
   begin
      return Left.Item = Right.Item;
   end "=";

   function To_Holder (
      Source : Name;
      Retain : Boolean := True)
      return Holder is
   begin
      return Result : Holder := (Finalization.Controlled with
         Item => Source)
      do
         if Retain then
            Adjust (Result);
         end if;
      end return;
   end To_Holder;

   function Generic_To_Holder (Source : Name) return Holder is
   begin
      return To_Holder (Source, Retain);
   end Generic_To_Holder;

   function Null_Holder return Holder is
   begin
      return (Finalization.Controlled with Item => <>);
   end Null_Holder;

   function Is_Null (Container : Holder) return Boolean is
      Default : Name;
      pragma Unmodified (Default);
   begin
      return Container.Element = Default;
   end Is_Null;

   procedure Clear (Container : in out Holder) is
      Default : Name;
      pragma Unmodified (Default);
   begin
      Finalize (Container);
      Container.Item := Default;
   end Clear;

   function Constant_Reference (Container : Holder) return Name is
   begin
      return Container.Item;
   end Constant_Reference;

   function Element (Container : Holder'Class) return Name is
   begin
      return Constant_Reference (Container);
   end Element;

   procedure Replace_Element (
      Target : in out Holder;
      Source : Name;
      Retain : Boolean := True) is
   begin
      Clear (Target);
      Target.Item := Source;
      if Retain then
         Adjust (Target);
      end if;
   end Replace_Element;

   procedure Generic_Replace_Element (
      Target : in out Holder;
      Source : Name) is
   begin
      Replace_Element (Target, Source, Retain);
   end Generic_Replace_Element;

   procedure Assign (
      Target : in out Holder;
      Source : Holder) is
   begin
      Clear (Target);
      Target.Item := Source.Item;
      Adjust (Target);
   end Assign;

   procedure Move (
      Target : in out Holder;
      Source : in out Holder)
   is
      Default : Name;
      pragma Unmodified (Default);
   begin
      Finalize (Target);
      Target.Item := Source.Item;
      Source.Item := Default;
   end Move;

   procedure Swap (I, J : in out Holder) is
      Temp : constant Name := I.Item;
   begin
      I.Item := J.Item;
      J.Item := Temp;
   end Swap;

   overriding procedure Adjust (Object : in out Holder) is
   begin
      if not Is_Null (Object) then
         Retain (Object.Item);
      end if;
   end Adjust;

   overriding procedure Finalize (Object : in out Holder) is
   begin
      if not Is_Null (Object) then
         Release (Object.Item);
      end if;
   end Finalize;

end Ada.Containers.Intrusive_Counted_Access_Holders;
