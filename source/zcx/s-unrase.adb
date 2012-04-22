pragma Check_Policy (Trace, Off);
with System.Unwind.Handling;
with C.unwind;
separate (System.Unwind.Raising)
package body Separated is
   pragma Suppress (All_Checks);
   use type C.unwind.Unwind_Ptr;

   --  (a-exexpr-gcc.adb)
   Setup_Key : constant := 16#DEAD#;

   --  (a-exexpr-gcc.adb)
   function Is_Setup_And_Not_Propagated (
      E : not null Exception_Occurrence_Access)
      return Boolean;

   --  (a-exexpr-gcc.adb)
   procedure Set_Setup_And_Not_Propagated (
      E : not null Exception_Occurrence_Access);

   --  (a-exexpr-gcc.adb)
   procedure Clear_Setup_And_Not_Propagated (
      E : not null Exception_Occurrence_Access);

   --  (a-exexpr-gcc.adb)
   function CleanupUnwind_Handler (
      ABI_Version : C.signed_int;
      Phases : C.unwind.Unwind_Action;
      Exception_Class : C.unwind.Unwind_Exception_Class;
      Exception_Object : access C.unwind.struct_Unwind_Exception;
      Context : access C.unwind.struct_Unwind_Context;
      Argument : C.void_ptr)
      return C.unwind.Unwind_Reason_Code;
   pragma Convention (C, CleanupUnwind_Handler);

   --  implementation

   function Is_Setup_And_Not_Propagated (
      E : not null Exception_Occurrence_Access)
      return Boolean is
   begin
      if E.Private_Data = Null_Address then
         return False;
      else
         declare
            GCC_Exception : Handling.GNAT_GCC_Exception;
            for GCC_Exception'Address use E.Private_Data;
         begin
            return GCC_Exception.Header.private_1 = Setup_Key;
         end;
      end if;
   end Is_Setup_And_Not_Propagated;

   procedure Set_Setup_And_Not_Propagated (
      E : not null Exception_Occurrence_Access)
   is
      GCC_Exception : Handling.GNAT_GCC_Exception;
      for GCC_Exception'Address use E.Private_Data;
   begin
      GCC_Exception.Header.private_1 := Setup_Key;
   end Set_Setup_And_Not_Propagated;

   --  (a-exexpr-gcc.adb)
   procedure Setup_Exception (
      Excep : not null Exception_Occurrence_Access;
      Current : not null Exception_Occurrence_Access;
      Reraised : Boolean) is
   begin
      if Reraised or else not Is_Setup_And_Not_Propagated (Excep) then
         pragma Check (Trace, Debug.Put ("new obj"));
         declare
            GCC_Exception : constant
               not null Handling.GNAT_GCC_Exception_Access :=
               new Handling.GNAT_GCC_Exception'(
                  Header => (
                     exception_class => <>,
                     exception_cleanup => <>,
                     private_1 => <>,
                     private_2 => <>),
                  Id => <>,
                  N_Cleanups_To_Trigger => <>,
                  Next_Exception => null, -- initialized here
                  landing_pad => <>,
                  ttype_filter => <>);
            Next : Exception_Occurrence_Access;
         begin
            if Current.Private_Data /= Null_Address then
               pragma Check (Trace, Debug.Put ("new eo"));
               Next := new Exception_Occurrence;
               Handling.Save_Occurrence_And_Private (Next.all, Current.all);
               GCC_Exception.Next_Exception := Next;
            end if;
            Current.Private_Data := GCC_Exception.all'Address;
            Set_Setup_And_Not_Propagated (Current);
         end;
      end if;
   end Setup_Exception;

   procedure Clear_Setup_And_Not_Propagated (
      E : not null Exception_Occurrence_Access)
   is
      GCC_Exception : Handling.GNAT_GCC_Exception;
      for GCC_Exception'Address use E.Private_Data;
   begin
      GCC_Exception.Header.private_1 := 0;
   end Clear_Setup_And_Not_Propagated;

   function CleanupUnwind_Handler (
      ABI_Version : C.signed_int;
      Phases : C.unwind.Unwind_Action;
      Exception_Class : C.unwind.Unwind_Exception_Class;
      Exception_Object : access C.unwind.struct_Unwind_Exception;
      Context : access C.unwind.struct_Unwind_Context;
      Argument : C.void_ptr)
      return C.unwind.Unwind_Reason_Code
   is
      pragma Unreferenced (ABI_Version);
      pragma Unreferenced (Phases);
      pragma Unreferenced (Exception_Class);
      pragma Unreferenced (Context);
      pragma Unreferenced (Argument);
      GCC_Exception : aliased Handling.GNAT_GCC_Exception;
      for GCC_Exception'Address use Exception_Object.all'Address;
   begin
      pragma Check (Trace, Debug.Put ("enter"));
      if GCC_Exception.N_Cleanups_To_Trigger = 0 then
         Unhandled_Exception_Terminate;
      end if;
      pragma Check (Trace, Debug.Put ("leave"));
      return C.unwind.URC_NO_REASON;
   end CleanupUnwind_Handler;

   --  (a-exexpr-gcc.adb)
   procedure Propagate_Exception (
      E : not null Standard_Library.Exception_Data_Ptr;
      From_Signal_Handler : Boolean)
   is
      pragma Inspection_Point (E);
      pragma Unreferenced (From_Signal_Handler);
      Current : constant not null Exception_Occurrence_Access :=
         Soft_Links.Get_Task_Local_Storage.all.Current_Exception'Access;
      GCC_Exception : aliased Handling.GNAT_GCC_Exception;
      for GCC_Exception'Address use Current.Private_Data;
      Dummy : C.unwind.Unwind_Reason_Code;
      pragma Unreferenced (Dummy);
   begin
      Clear_Setup_And_Not_Propagated (Current);
      GCC_Exception.Header.exception_class := Handling.GNAT_Exception_Class;
      GCC_Exception.Header.exception_cleanup := null;
      GCC_Exception.Id := Current.Id;
      GCC_Exception.N_Cleanups_To_Trigger := 0;
      if Call_Chain'Address /= Null_Address then
         Call_Chain (Current);
      end if;
      Dummy := C.unwind.Unwind_RaiseException (
         GCC_Exception.Header'Unchecked_Access);
      --  it does not come here, if handler was found
      Notify_Unhandled_Exception;
      if GCC_Exception.N_Cleanups_To_Trigger /= 0 then
         pragma Check (Trace, Debug.Put ("finally"));
         --  invoke finally handlers
         Dummy := C.unwind.Unwind_ForcedUnwind (
            GCC_Exception.Header'Unchecked_Access,
            CleanupUnwind_Handler'Access,
            C.void_ptr (Null_Address));
      end if;
      Unhandled_Exception_Terminate;
   end Propagate_Exception;

end Separated;
