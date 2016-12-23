with Ada.Finalization;
with System;
package body Ada.Exceptions.Finally is
   pragma Suppress (All_Checks);
   use type System.Address;

   type Handler_Type is access procedure (Params : System.Address);

   type Finalizer is limited new Finalization.Limited_Controlled with record
      Params : System.Address;
      Handler : Handler_Type;
   end record;
   pragma Discard_Names (Finalizer);

   overriding procedure Finalize (Object : in out Finalizer);
   overriding procedure Finalize (Object : in out Finalizer) is
   begin
      if Object.Params /= System.Null_Address then
         Object.Handler (Object.Params);
      end if;
   end Finalize;

   --  implementation

   package body Scoped_Holder is

      procedure Finally (Params : System.Address);
      procedure Finally (Params : System.Address) is
         function To_Pointer (Value : System.Address) return access Parameters
            with Import, Convention => Intrinsic;
      begin
         Handler (To_Pointer (Params).all);
      end Finally;

      Object : Finalizer :=
         (Finalization.Limited_Controlled
            with
               Params => System.Null_Address,
               Handler => Finally'Unrestricted_Access);
      pragma Unreferenced (Object);

      procedure Assign (Params : aliased in out Parameters) is
      begin
         Object.Params := Params'Address;
      end Assign;

      procedure Clear is
      begin
         Object.Params := System.Null_Address;
      end Clear;

   end Scoped_Holder;

   procedure Try_Finally (
      Params : aliased in out Parameters;
      Process : not null access procedure (Params : in out Parameters);
      Handler : not null access procedure (Params : in out Parameters))
   is
      procedure Finally (Params : System.Address);
      procedure Finally (Params : System.Address) is
         function To_Pointer (Value : System.Address) return access Parameters
            with Import, Convention => Intrinsic;
      begin
         Handler (To_Pointer (Params).all);
      end Finally;
      Object : Finalizer :=
         (Finalization.Limited_Controlled
            with
               Params => Params'Address,
               Handler => Finally'Unrestricted_Access);
      pragma Unreferenced (Object);
   begin
      Process.all (Params);
   end Try_Finally;

   procedure Try_When_All (
      Params : aliased in out Parameters;
      Process : not null access procedure (Params : in out Parameters);
      Handler : not null access procedure (Params : in out Parameters))
   is
      procedure Finally (Params : System.Address);
      procedure Finally (Params : System.Address) is
         function To_Pointer (Value : System.Address) return access Parameters
            with Import, Convention => Intrinsic;
      begin
         Handler (To_Pointer (Params).all);
      end Finally;
      Object : Finalizer :=
         (Finalization.Limited_Controlled
            with
               Params => Params'Address,
               Handler => Finally'Unrestricted_Access);
      pragma Unreferenced (Object);
   begin
      Process.all (Params);
      Object.Params := System.Null_Address;
   end Try_When_All;

end Ada.Exceptions.Finally;
