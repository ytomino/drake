package body Ada.Containers.Scoped_Access_Holders is

   function To_Holder (Source : Name) return Holder is
   begin
      return (Finalization.Limited_Controlled with Item => Source);
   end To_Holder;

   function Null_Holder return Holder is
   begin
      return (Finalization.Limited_Controlled with Item => <>);
   end Null_Holder;

   function Is_Null (Container : Holder) return Boolean is
      Default : Name;
      pragma Unmodified (Default);
   begin
      return Container.Element = Default;
   end Is_Null;

   procedure Clear (Container : in out Holder) is
   begin
      Finalize (Container); -- Free called in Finalize sets default
   end Clear;

   function Constant_Reference (Container : Holder) return Name is
   begin
      return Container.Item;
   end Constant_Reference;

   function Element (Container : Holder'Class) return Name is
   begin
      return Constant_Reference (Container);
   end Element;

   procedure Replace_Element (Target : in out Holder; Source : Name) is
   begin
      Finalize (Target);
      Target.Item := Source;
   end Replace_Element;

   procedure Move (
      Target : in out Holder;
      Source : in out Holder) is
   begin
      Clear (Target);
      Swap (Target, Source);
   end Move;

   procedure Swap (I, J : in out Holder) is
      Temp : constant Name := I.Item;
   begin
      I.Item := J.Item;
      J.Item := Temp;
   end Swap;

   procedure Release (Container : in out Holder; Item : out Name) is
      Default : Name;
      pragma Unmodified (Default);
   begin
      Item := Container.Item;
      Container.Item := Default;
   end Release;

   function Release (Container : not null access Holder) return Name is
   begin
      return Result : Name do
         Release (Container.all, Result);
      end return;
   end Release;

   overriding procedure Finalize (Object : in out Holder) is
   begin
      Free (Object.Item);
   end Finalize;

end Ada.Containers.Scoped_Access_Holders;
