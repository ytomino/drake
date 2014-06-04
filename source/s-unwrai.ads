pragma License (Unrestricted);
--  runtime unit
with Ada;
package System.Unwind.Raising is
   pragma Preelaborate;

   --  (s-stalib.ads)
   Local_Partition_ID : Natural := 0;

   --  equivalent to Raise_With_Location_And_Msg (a-except-2005.adb)
   procedure Raise_Exception (
      E : not null Exception_Data_Access;
      File : String := "";
      Line : Integer := 0;
      Column : Integer := 0;
      Message : String := "";
      Stack_Guard : Address := Null_Address);
   pragma No_Return (Raise_Exception);

   --  equivalent to Raise_From_Signal_Handler (a-except-2005.adb)
   procedure Raise_From_Signal_Handler (
      E : not null Exception_Data_Access;
      File : String := "";
      Line : Integer := 0;
      Column : Integer := 0;
      Message : String;
      Stack_Guard : Address)
      renames Raise_Exception;

   --  implementation for raising (a-except-2005.adb)
   procedure Raise_E (
      E : Exception_Data_Access;
      Message : String);
   pragma No_Return (Raise_E);
   pragma Export (Ada, Raise_E, "ada__exceptions__raise_exception");

   procedure Raise_Exception_From_Here (
      E : not null Exception_Data_Access;
      File : String := Ada.Debug.File;
      Line : Integer := Ada.Debug.Line);
   pragma No_Return (Raise_Exception_From_Here);
   pragma Export (Ada, Raise_Exception_From_Here,
      "__drake_raise_exception_from_here");

   procedure Raise_Exception_From_Here_With (
      E : not null Exception_Data_Access;
      File : String := Ada.Debug.File;
      Line : Integer := Ada.Debug.Line;
      Message : String);
   pragma No_Return (Raise_Exception_From_Here_With);
   pragma Export (Ada, Raise_Exception_From_Here_With,
      "__drake_raise_exception_from_here_with");

   --  implementation for reraising (a-except-2005.adb)
   procedure Reraise (X : Exception_Occurrence);
   pragma No_Return (Reraise);
   pragma Export (Ada, Reraise, "ada__exceptions__reraise_occurrence_always");

   --  implementation for reraising from when all others (a-except-2005.adb)
   procedure Reraise_From_All_Others (X : Exception_Occurrence);
   pragma No_Return (Reraise_From_All_Others);
   pragma Export (Ada, Reraise_From_All_Others,
      "ada__exceptions__reraise_occurrence_no_defer");

   --  implementation for raising from controlled objects (a-except-2005.adb)
   procedure Reraise_From_Controlled_Operation (X : Exception_Occurrence);
   pragma No_Return (Reraise_From_Controlled_Operation);
   pragma Export (Ada, Reraise_From_Controlled_Operation,
      "__gnat_raise_from_controlled_operation");

   --  implementation for raising from finalize_library (a-except-2005.adb)
   procedure Reraise_Library_Exception_If_Any;
   pragma Export (Ada, Reraise_Library_Exception_If_Any,
      "__gnat_reraise_library_exception_if_any");

   --  utility for implementing a dummy subprogram
   procedure Raise_Program_Error;
   pragma Export (Ada, Raise_Program_Error, "__drake_program_error");

   --  shortcut required by compiler (a-except-2005.adb)

   procedure rcheck_00 (File : not null access Character; Line : Integer);
   pragma No_Return (rcheck_00);
   pragma Export (C, rcheck_00, "__gnat_rcheck_CE_Access_Check");

   procedure rcheck_02 (File : not null access Character; Line : Integer);
   pragma No_Return (rcheck_02);
   pragma Export (C, rcheck_02, "__gnat_rcheck_CE_Discriminant_Check");

   procedure rcheck_03 (File : not null access Character; Line : Integer);
   pragma No_Return (rcheck_03);
   pragma Export (C, rcheck_03, "__gnat_rcheck_CE_Divide_By_Zero");

   --  equivalent to rcheck_03
   procedure Zero_Division (
      File : String := Ada.Debug.File;
      Line : Integer := Ada.Debug.Line);
   pragma No_Return (Zero_Division);

   procedure rcheck_04 (File : not null access Character; Line : Integer);
   pragma No_Return (rcheck_04);
   pragma Export (C, rcheck_04, "__gnat_rcheck_CE_Explicit_Raise");

   procedure rcheck_05 (File : not null access Character; Line : Integer);
   pragma No_Return (rcheck_05);
   pragma Export (C, rcheck_05, "__gnat_rcheck_CE_Index_Check");

   procedure rcheck_06 (File : not null access Character; Line : Integer);
   pragma No_Return (rcheck_06);
   pragma Export (C, rcheck_06, "__gnat_rcheck_CE_Invalid_Data");

   procedure rcheck_07 (File : not null access Character; Line : Integer);
   pragma No_Return (rcheck_07);
   pragma Export (C, rcheck_07, "__gnat_rcheck_CE_Length_Check");

   procedure rcheck_09 (File : not null access Character; Line : Integer);
   pragma No_Return (rcheck_09);
   pragma Export (C, rcheck_09, "__gnat_rcheck_CE_Null_Not_Allowed");

   procedure rcheck_10 (File : not null access Character; Line : Integer);
   pragma No_Return (rcheck_10);
   pragma Export (C, rcheck_10, "__gnat_rcheck_CE_Overflow_Check");

   --  equivalent to rcheck_10
   procedure Overflow (
      File : String := Ada.Debug.File;
      Line : Integer := Ada.Debug.Line);
   pragma No_Return (Overflow);

   procedure rcheck_12 (File : not null access Character; Line : Integer);
   pragma No_Return (rcheck_12);
   pragma Export (C, rcheck_12, "__gnat_rcheck_CE_Range_Check");

   procedure rcheck_13 (File : not null access Character; Line : Integer);
   pragma No_Return (rcheck_13);
   pragma Export (C, rcheck_13, "__gnat_rcheck_CE_Tag_Check");

   procedure rcheck_14 (File : not null access Character; Line : Integer);
   pragma No_Return (rcheck_14);
   pragma Export (C, rcheck_14, "__gnat_rcheck_PE_Access_Before_Elaboration");

   procedure rcheck_15 (File : not null access Character; Line : Integer);
   pragma No_Return (rcheck_15);
   pragma Export (C, rcheck_15, "__gnat_rcheck_PE_Accessibility_Check");

   procedure rcheck_22 (File : not null access Character; Line : Integer);
   pragma No_Return (rcheck_22);
   pragma Export (C, rcheck_22, "__gnat_rcheck_PE_Explicit_Raise");

   procedure rcheck_23 (File : not null access Character; Line : Integer);
   pragma No_Return (rcheck_23);
   pragma Export (C, rcheck_23, "__gnat_rcheck_PE_Finalize_Raised_Exception");

   procedure rcheck_24 (File : not null access Character; Line : Integer);
   pragma No_Return (rcheck_24);
   pragma Export (C, rcheck_24, "__gnat_rcheck_PE_Implicit_Return");

   procedure rcheck_25 (File : not null access Character; Line : Integer);
   pragma No_Return (rcheck_25);
   pragma Export (C, rcheck_25, "__gnat_rcheck_PE_Misaligned_Address_Value");

   procedure rcheck_26 (File : not null access Character; Line : Integer);
   pragma No_Return (rcheck_26);
   pragma Export (C, rcheck_26, "__gnat_rcheck_PE_Missing_Return");

   procedure rcheck_27 (File : not null access Character; Line : Integer);
   pragma No_Return (rcheck_27);
   pragma Export (C, rcheck_27, "__gnat_rcheck_PE_Overlaid_Controlled_Object");

   procedure rcheck_30 (File : not null access Character; Line : Integer);
   pragma No_Return (rcheck_30);
   pragma Export (C, rcheck_30,
      "__gnat_rcheck_PE_Unchecked_Union_Restriction");

   procedure rcheck_32 (File : not null access Character; Line : Integer);
   pragma No_Return (rcheck_32);
   pragma Export (C, rcheck_32, "__gnat_rcheck_SE_Empty_Storage_Pool");

   procedure rcheck_33 (File : not null access Character; Line : Integer);
   pragma No_Return (rcheck_33);
   pragma Export (C, rcheck_33, "__gnat_rcheck_SE_Explicit_Raise");

   procedure rcheck_35 (File : not null access Character; Line : Integer);
   pragma No_Return (rcheck_35);
   pragma Export (C, rcheck_35, "__gnat_rcheck_SE_Object_Too_Large");

   --  excluding code range
   function AAA return Address;
   function ZZZ return Address;

   --  implementation for tasking (a-except-2005.adb)
   function Triggered_By_Abort return Boolean;
   pragma Export (Ada, Triggered_By_Abort,
      "ada__exceptions__triggered_by_abort");

   --  equivalent to Set_Exception_C_Msg (a-exexda.adb)
   procedure Set_Exception_Message (
      Id : not null Exception_Data_Access;
      File : String := "";
      Line : Integer := 0;
      Column : Integer := 0;
      Message : String;
      X : in out Exception_Occurrence);

   --  output the information of unhandled exception
   procedure Report (X : Exception_Occurrence; Where : String);
   procedure Report_Traceback (X : Exception_Occurrence);

   --  gdb knows below names for "catch exception" command
   --  but, if those symbols are existing, gdb may report another error.
   --  this is not a problem of drake,
   --  same report may be also seen with original GNAT runtime in gcc-4.7.
   --  after all, this command only works completely with debug info
   --    generated by custom version gcc in GNAT-GPL.
--  procedure __gnat_debug_raise_exception (E : Exception_Data_Ptr);
--  procedure __gnat_unhandled_exception (E : Exception_Data_Ptr);
--  procedure __gnat_debug_raise_assert_failure;
--  procedure __gnat_raise_nodefer_with_msg (E : Exception_Data_Ptr);

end System.Unwind.Raising;
