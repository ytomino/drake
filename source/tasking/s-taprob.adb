package body System.Tasking.Protected_Objects is

   procedure Initialize_Protection (
      Object : not null access Protection;
      Ceiling_Priority : Integer)
   is
      pragma Unreferenced (Ceiling_Priority);
   begin
      Synchronous_Objects.Initialize (Object.Lock);
   end Initialize_Protection;

   procedure Finalize_Protection (Object : in out Protection) is
   begin
      Synchronous_Objects.Finalize (Object.Lock);
   end Finalize_Protection;

   procedure Lock (Object : not null access Protection) is
   begin
      --  in locking, abort is deferred
      --  so checking the aborted flag inside of r/w-lock is meaningless
      Synchronous_Objects.Enter_Writing (Object.Lock);
   end Lock;

   procedure Lock_Read_Only (Object : not null access Protection) is
   begin
      Synchronous_Objects.Enter_Reading (Object.Lock);
   end Lock_Read_Only;

   procedure Unlock (Object : not null access Protection) is
   begin
      Synchronous_Objects.Leave (Object.Lock);
   end Unlock;

   function Get_Ceiling (Object : not null access Protection)
      return Any_Priority is
   begin
      raise Program_Error; -- unimplemented
      return Get_Ceiling (Object);
   end Get_Ceiling;

end System.Tasking.Protected_Objects;
