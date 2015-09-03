pragma License (Unrestricted);
--  runtime unit
with Ada;
with System.Unwind.Representation;
package System.Unwind.Raising is
   pragma Preelaborate;

   --  equivalent to Raise_With_Location_And_Msg (a-except-2005.adb)
   procedure Raise_Exception (
      E : not null Exception_Data_Access;
      File : String := "";
      Line : Integer := 0;
      Column : Integer := 0;
      Message : String := "");
   pragma No_Return (Raise_Exception);

   --  equivalent to Raise_From_Signal_Handler (a-except-2005.adb)
   procedure Raise_From_Signal_Handler (
      E : not null Exception_Data_Access;
      Message : String;
      Stack_Guard : Address);
   pragma No_Return (Raise_From_Signal_Handler);

   --  implementation for raising (a-except-2005.adb)
   procedure Raise_E (
      E : Exception_Data_Access;
      Message : String)
      with Export,
         Convention => Ada,
         External_Name => "ada__exceptions__raise_exception";
   pragma No_Return (Raise_E);

   procedure Raise_Exception_From_Here (
      E : not null Exception_Data_Access;
      File : String := Ada.Debug.File;
      Line : Integer := Ada.Debug.Line)
      with Export,
         Convention => Ada,
         External_Name => "__drake_raise_exception_from_here";
   procedure Raise_Exception_From_Here_With (
      E : not null Exception_Data_Access;
      File : String := Ada.Debug.File;
      Line : Integer := Ada.Debug.Line;
      Message : String)
      with Export,
         Convention => Ada,
         External_Name => "__drake_raise_exception_from_here_with";

   pragma No_Return (Raise_Exception_From_Here);
   pragma No_Return (Raise_Exception_From_Here_With);

   --  implementation for reraising (a-except-2005.adb)
   procedure Reraise (X : Exception_Occurrence)
      with Export,
         Convention => Ada,
         External_Name => "ada__exceptions__reraise_occurrence_always";
   pragma No_Return (Reraise);

   --  implementation for reraising from when all others (a-except-2005.adb)
   procedure Reraise_From_All_Others (X : Exception_Occurrence)
      with Export,
         Convention => Ada,
         External_Name => "ada__exceptions__reraise_occurrence_no_defer";
   pragma No_Return (Reraise_From_All_Others);

   --  implementation for raising from controlled objects (a-except-2005.adb)
   procedure Reraise_From_Controlled_Operation (X : Exception_Occurrence)
      with Export,
         Convention => Ada,
         External_Name => "__gnat_raise_from_controlled_operation";
   pragma No_Return (Reraise_From_Controlled_Operation);

   --  equivalent to Reraise_GCC_Exception (a-exexpr-gcc.adb)
   --  for nested controlled types
   procedure Reraise_Machine_Occurrence (
      Machine_Occurrence : not null Representation.Machine_Occurrence_Access)
      with Export, Convention => C, External_Name => "__gnat_reraise_zcx";

   --  implementation for raising from finalize_library (a-except-2005.adb)
   procedure Reraise_Library_Exception_If_Any
      with Export,
         Convention => Ada,
         External_Name => "__gnat_reraise_library_exception_if_any";

   --  utility for implementing a dummy subprogram
   procedure Raise_Program_Error
      with Export, Convention => Ada, External_Name => "__drake_program_error";

   --  for runtime checks (a-except-2005.adb)

   procedure rcheck_00 (File : not null access Character; Line : Integer)
      with Export,
         Convention => C, External_Name => "__gnat_rcheck_CE_Access_Check";
   pragma No_Return (rcheck_00);

   procedure rcheck_02 (File : not null access Character; Line : Integer)
      with Export,
         Convention => C,
         External_Name => "__gnat_rcheck_CE_Discriminant_Check";
   pragma No_Return (rcheck_02);

   procedure rcheck_03 (File : not null access Character; Line : Integer)
      with Export,
         Convention => C, External_Name => "__gnat_rcheck_CE_Divide_By_Zero";
   pragma No_Return (rcheck_03);

   --  equivalent to rcheck_03
   procedure Zero_Division (
      File : String := Ada.Debug.File;
      Line : Integer := Ada.Debug.Line);
   pragma No_Return (Zero_Division);

   procedure rcheck_04 (File : not null access Character; Line : Integer)
      with Export,
         Convention => C, External_Name => "__gnat_rcheck_CE_Explicit_Raise";
   pragma No_Return (rcheck_04);

   procedure rcheck_05 (File : not null access Character; Line : Integer)
      with Export,
         Convention => C, External_Name => "__gnat_rcheck_CE_Index_Check";
   pragma No_Return (rcheck_05);

   procedure rcheck_06 (File : not null access Character; Line : Integer)
      with Export,
         Convention => C, External_Name => "__gnat_rcheck_CE_Invalid_Data";
   pragma No_Return (rcheck_06);

   procedure rcheck_07 (File : not null access Character; Line : Integer)
      with Export,
         Convention => C, External_Name => "__gnat_rcheck_CE_Length_Check";
   pragma No_Return (rcheck_07);

   procedure rcheck_09 (File : not null access Character; Line : Integer)
      with Export,
         Convention => C, External_Name => "__gnat_rcheck_CE_Null_Not_Allowed";
   pragma No_Return (rcheck_09);

   procedure rcheck_10 (File : not null access Character; Line : Integer)
      with Export,
         Convention => C, External_Name => "__gnat_rcheck_CE_Overflow_Check";
   pragma No_Return (rcheck_10);

   --  equivalent to rcheck_10
   procedure Overflow (
      File : String := Ada.Debug.File;
      Line : Integer := Ada.Debug.Line);
   pragma No_Return (Overflow);

   procedure rcheck_12 (File : not null access Character; Line : Integer)
      with Export,
         Convention => C, External_Name => "__gnat_rcheck_CE_Range_Check";
   pragma No_Return (rcheck_12);

   procedure rcheck_13 (File : not null access Character; Line : Integer)
      with Export,
         Convention => C, External_Name => "__gnat_rcheck_CE_Tag_Check";
   pragma No_Return (rcheck_13);

   procedure rcheck_14 (File : not null access Character; Line : Integer)
      with Export,
         Convention => C,
         External_Name => "__gnat_rcheck_PE_Access_Before_Elaboration";
   pragma No_Return (rcheck_14);

   procedure rcheck_15 (File : not null access Character; Line : Integer)
      with Export,
         Convention => C,
         External_Name => "__gnat_rcheck_PE_Accessibility_Check";
   pragma No_Return (rcheck_15);

   procedure rcheck_22 (File : not null access Character; Line : Integer)
      with Export,
         Convention => C, External_Name => "__gnat_rcheck_PE_Explicit_Raise";
   pragma No_Return (rcheck_22);

   procedure rcheck_23 (File : not null access Character; Line : Integer)
      with Export,
         Convention => C,
         External_Name => "__gnat_rcheck_PE_Finalize_Raised_Exception";
   pragma No_Return (rcheck_23);

   procedure rcheck_24 (File : not null access Character; Line : Integer)
      with Export,
         Convention => C, External_Name => "__gnat_rcheck_PE_Implicit_Return";
   pragma No_Return (rcheck_24);

   procedure rcheck_25 (File : not null access Character; Line : Integer)
      with Export,
         Convention => C,
         External_Name => "__gnat_rcheck_PE_Misaligned_Address_Value";
   pragma No_Return (rcheck_25);

   procedure rcheck_26 (File : not null access Character; Line : Integer)
      with Export,
         Convention => C, External_Name => "__gnat_rcheck_PE_Missing_Return";
   pragma No_Return (rcheck_26);

   procedure rcheck_27 (File : not null access Character; Line : Integer)
      with Export,
         Convention => C,
         External_Name => "__gnat_rcheck_PE_Overlaid_Controlled_Object";
   pragma No_Return (rcheck_27);

   procedure rcheck_30 (File : not null access Character; Line : Integer)
      with Export,
         Convention => C,
         External_Name => "__gnat_rcheck_PE_Unchecked_Union_Restriction";
   pragma No_Return (rcheck_30);

   procedure rcheck_32 (File : not null access Character; Line : Integer)
      with Export,
         Convention => C,
         External_Name => "__gnat_rcheck_SE_Empty_Storage_Pool";
   pragma No_Return (rcheck_32);

   procedure rcheck_33 (File : not null access Character; Line : Integer)
      with Export,
         Convention => C, External_Name => "__gnat_rcheck_SE_Explicit_Raise";
   pragma No_Return (rcheck_33);

   procedure rcheck_35 (File : not null access Character; Line : Integer)
      with Export,
         Convention => C, External_Name => "__gnat_rcheck_SE_Object_Too_Large";
   pragma No_Return (rcheck_35);

   procedure Save_Exception (
      X : out Exception_Occurrence;
      E : not null Exception_Data_Access;
      File : String := "";
      Line : Integer := 0;
      Column : Integer := 0;
      Message : String := "");

   procedure Save_E (
      X : out Exception_Occurrence;
      E : not null Exception_Data_Access;
      Message : String)
      with Export,
         Convention => Ada, External_Name => "__drake_save_exception";

   procedure Save_Exception_From_Here (
      X : out Exception_Occurrence;
      E : not null Exception_Data_Access;
      File : String := Ada.Debug.File;
      Line : Integer := Ada.Debug.Line)
      with Export,
         Convention => Ada,
         External_Name => "__drake_save_exception_from_here";

   procedure Save_Exception_From_Here_With (
      X : out Exception_Occurrence;
      E : not null Exception_Data_Access;
      File : String := Ada.Debug.File;
      Line : Integer := Ada.Debug.Line;
      Message : String)
      with Export,
         Convention => Ada,
         External_Name => "__drake_save_exception_from_here_with";

   --  excluding code range
   function AAA return Address;
   function ZZZ return Address;

   --  gdb knows some names for "catch exception" command.
   --  but, it works incompletely with not GNAT-GPL but official gcc.
   --  in drake, only the simple form of "catch exception" is supported.

   --  (s-excdeb.ads)
   procedure Debug_Raise_Exception (E : not null Exception_Data_Access)
      with Export,
         Convention => Ada, External_Name => "__gnat_debug_raise_exception";

--  procedure __gnat_unhandled_exception (E : Exception_Data_Ptr);
--  procedure __gnat_debug_raise_assert_failure;
--  procedure __gnat_raise_nodefer_with_msg (E : Exception_Data_Ptr);

end System.Unwind.Raising;
