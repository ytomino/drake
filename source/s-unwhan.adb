pragma Check_Policy (Trace => Ignore);
with Ada;
with System.Unwind.Mapping;
with System.Unwind.Occurrences;
package body System.Unwind.Handling is
   pragma Suppress (All_Checks);
   use type Unwind.Representation.Machine_Occurrence_Access;
   use type Unwind.Representation.Unwind_Exception_Class;

   --  force to link System.Unwind.Mapping
   --    to convert signals or SEH exceptions to standard exceptions.

   Force_Use : Address := Mapping.Install_Exception_Handler'Address
      with Export,
         Convention => Ada,
         External_Name => "__drake_use_install_exception_handler";
   pragma Unreferenced (Force_Use); -- [gcc-4.9] does not take Export as ref

   --  implementation

   procedure Begin_Handler (
      Machine_Occurrence : Representation.Machine_Occurrence_Access) is
   begin
      pragma Check (Trace, Ada.Debug.Put ("enter"));
      Occurrences.Set_Current_Machine_Occurrence (Machine_Occurrence);
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
         Occurrences.Free (Machine_Occurrence);
         Occurrences.Set_Current_Machine_Occurrence (null);
         --  in Win32 SEH, the chain may be rollback, so restore it
         Mapping.Reinstall_Exception_Handler;
      end if;
      pragma Check (Trace, Ada.Debug.Put ("leave"));
   end End_Handler;

   procedure Set_Exception_Parameter (
      X : not null Exception_Occurrence_Access;
      Machine_Occurrence :
         not null Representation.Machine_Occurrence_Access) is
   begin
      if Machine_Occurrence.Header.exception_class =
         Representation.GNAT_Exception_Class
      then
         Occurrences.Save_Occurrence (X.all, Machine_Occurrence.Occurrence);
      else
         Occurrences.Set_Foreign_Occurrence (X.all, Machine_Occurrence);
      end if;
   end Set_Exception_Parameter;

end System.Unwind.Handling;
