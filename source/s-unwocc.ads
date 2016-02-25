pragma License (Unrestricted);
--  runtime unit
with System.Unwind.Representation;
with System.Runtime_Context;
package System.Unwind.Occurrences is
   pragma Preelaborate;

   --  (s-stalib.ads)
   Local_Partition_ID : Natural := 0;

   --  implementation for catching object (a-except-2005.adb)
   procedure Save_Occurrence (
      Target : out Exception_Occurrence;
      Source : Exception_Occurrence)
      with Export,
         Convention => Ada,
         External_Name => "ada__exceptions__save_occurrence";

   --  preparing

   procedure Backtrace (X : in out Exception_Occurrence);

   --  equivalent to Set_Exception_C_Msg (a-exexda.adb)
   procedure Set_Exception_Message (
      Id : not null Exception_Data_Access;
      File : String := "";
      Line : Integer := 0;
      Column : Integer := 0;
      Message : String;
      X : in out Exception_Occurrence);

   --  representation

   function New_Machine_Occurrence (Stack_Guard : Address)
      return not null Representation.Machine_Occurrence_Access;

   procedure Free (
      Machine_Occurrence : Representation.Machine_Occurrence_Access);

   --  (a-exexpr-gcc.adb)
   procedure Set_Foreign_Occurrence (
      X : in out Exception_Occurrence;
      Machine_Occurrence : not null Representation.Machine_Occurrence_Access);

   --  equivalent to Get_Current_Excep_NT (s-soflin.adb),
   --     Get_Current_Excep (s-tarest.adb)
   --     and Setup_Current_Excep (a-exexpr-gcc.adb)
   function Get_Current_Occurrence (
      TLS : not null Runtime_Context.Task_Local_Storage_Access)
      return Exception_Occurrence_Access;

   --  task local storage

   procedure Set_Current_Machine_Occurrence (
      Machine_Occurrence : Representation.Machine_Occurrence_Access);

   --  implementation for finalizer (a-except-2005.adb)
   function Triggered_By_Abort return Boolean
      with Export,
         Convention => Ada,
         External_Name => "ada__exceptions__triggered_by_abort";

   --  handler

   --  unhandled handler (a-exexpr-gcc.adb)
   --  the symbol is required only in Win64 SEH.
   procedure Unhandled_Except_Handler (
      Machine_Occurrence : not null Representation.Machine_Occurrence_Access)
      with Export,
         Convention => C,
         External_Name => "__gnat_unhandled_except_handler";
   pragma No_Return (Unhandled_Except_Handler);

   --  reporting

   --  equivalent to Append_Info_Exception_Information (a-exexda.adb)
   procedure Exception_Information (
      X : Exception_Occurrence;
      Params : Address;
      Put : not null access procedure (S : String; Params : Address);
      New_Line : not null access procedure (Params : Address));

   --  output the information of unhandled exception

   procedure Default_Report (X : Exception_Occurrence; Where : String);

   type Report_Handler is
      access procedure (X : Exception_Occurrence; Where : String);
   pragma Suppress (Access_Check, Report_Handler);

   Report_Hook : Report_Handler := Default_Report'Access -- not null
      with Export,
         Convention => Ada,
         External_Name => "__drake_report_exception_occurrence_hook";
   pragma Suppress (Access_Check, Report_Hook);

   procedure Report (X : Exception_Occurrence; Where : String);

end System.Unwind.Occurrences;
