pragma Check_Policy (Trace, Off);
with Ada;
with System.Unwind.Mapping;
with System.Unwind.Raising;
package body System.Unwind.Handling is
   pragma Suppress (All_Checks);
   use type System.Unwind.Representation.Machine_Occurrence_Access;
   use type System.Unwind.Representation.Unwind_Exception_Class;

   --  force to link System.Unwind.Mapping
   --    to convert signals or SEH exceptions to standard exceptions.

   Force_Use : Address := Mapping.Install_Exception_Handler'Address;
   pragma Export (Ada, Force_Use, "__drake_use_install_exception_handler");

   --  implementation

   procedure Begin_Handler (
      Machine_Occurrence : Representation.Machine_Occurrence_Access)
   is
      Current : Exception_Occurrence_Access;
      pragma Unreferenced (Current);
   begin
      pragma Check (Trace, Ada.Debug.Put ("enter"));
      Raising.Save_Current_Occurrence (Machine_Occurrence, Current);
      pragma Check (Trace, Ada.Debug.Put ("leave"));
   end Begin_Handler;

   procedure End_Handler (
      Machine_Occurrence : Representation.Machine_Occurrence_Access) is
   begin
      pragma Check (Trace, Ada.Debug.Put ("enter"));
      if Machine_Occurrence = null then
         pragma Check (Trace, Ada.Debug.Put (
            "Machine_Occurrence = null, reraised"));
         null;
      else
         Raising.Free (Machine_Occurrence);
         --  in Win32 SEH, the chain may be rollback, so restore it
         Mapping.Reinstall_Exception_Handler;
      end if;
      pragma Check (Trace, Ada.Debug.Put ("leave"));
   end End_Handler;

end System.Unwind.Handling;
