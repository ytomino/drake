pragma Check_Policy (Trace, Off);
with System.Startup;
with System.Storage_Elements;
with System.Synchronous_Control;
with System.Unwind.Occurrences;
with System.Unwind.Standard;
package body System.Unwind.Raising is
   pragma Suppress (All_Checks);

   --  package separated for depending on libgcc
   package Separated is

      --  equivalent to Propagate_GCC_Exception (a-exexpr-gcc.adb)
      procedure Propagate_Machine_Occurrence (
         Machine_Occurrence :
            not null Representation.Machine_Occurrence_Access);
      pragma No_Return (Propagate_Machine_Occurrence);
      pragma Convention (C, Propagate_Machine_Occurrence);

   end Separated;

   package body Separated is separate;

   function strlen (s : not null access Character)
      return Storage_Elements.Storage_Count;
   pragma Import (Intrinsic, strlen, "__builtin_strlen");

   --  equivalent to Complete_And_Propagate_Occurrence (a-exexpr-gcc.adb)
   procedure Propagate_Machine_Occurrence (
      Machine_Occurrence :
         not null Representation.Machine_Occurrence_Access);
   pragma No_Return (Propagate_Machine_Occurrence);

   --  not calling Abort_Defer

   --  equivalent to Raise_Exception_No_Defer (a-except-2005.adb)
   procedure Raise_Exception_No_Defer (
      E : not null Exception_Data_Access;
      File : String := "";
      Line : Integer := 0;
      Column : Integer := 0;
      Message : String := "");
   pragma No_Return (Raise_Exception_No_Defer);

   procedure Reraise_No_Defer (X : Exception_Occurrence);
   pragma No_Return (Reraise_No_Defer);

   --  for rcheck

   procedure Raise_From_rcheck (
      File : not null access Character;
      Line : Integer;
      E : not null Exception_Data_Access;
      Message : String);
   pragma No_Return (Raise_From_rcheck);

   Explicit_Raise : constant String := "explicit raise";
   Divide_By_Zero : constant String := "divide by zero";
   Overflow_Check_Failed : constant String := "overflow check failed";
   From_Finalize : constant String := "finalize/adjust raised exception";

   --  implementation

   --  at first of exclusion
   function AAA return Address is
   begin
      <<Code>>
      return Code'Address;
   end AAA;

   --  start private part in exclusion

   procedure Propagate_Machine_Occurrence (
      Machine_Occurrence :
         not null Representation.Machine_Occurrence_Access) is
   begin
      Occurrences.Backtrace (Machine_Occurrence.Occurrence);
      Debug_Raise_Exception (Machine_Occurrence.Occurrence.Id); -- for gdb
      Separated.Propagate_Machine_Occurrence (Machine_Occurrence);
   end Propagate_Machine_Occurrence;

   procedure Raise_Exception_No_Defer (
      E : not null Exception_Data_Access;
      File : String := "";
      Line : Integer := 0;
      Column : Integer := 0;
      Message : String := "")
   is
      Machine_Occurrence : Representation.Machine_Occurrence_Access;
   begin
      Machine_Occurrence := Occurrences.New_Machine_Occurrence (
         Stack_Guard => Null_Address);
      Occurrences.Set_Exception_Message (E, File, Line, Column, Message,
         X => Machine_Occurrence.Occurrence);
      Propagate_Machine_Occurrence (Machine_Occurrence);
   end Raise_Exception_No_Defer;

   procedure Raise_Exception (
      E : not null Exception_Data_Access;
      File : String := "";
      Line : Integer := 0;
      Column : Integer := 0;
      Message : String := "";
      Stack_Guard : Address := Null_Address)
   is
      Machine_Occurrence : Representation.Machine_Occurrence_Access;
   begin
      if not ZCX_By_Default then
         Synchronous_Control.Lock_Abort;
      end if;
      Machine_Occurrence := Occurrences.New_Machine_Occurrence (
         Stack_Guard => Stack_Guard);
      Occurrences.Set_Exception_Message (E, File, Line, Column, Message,
         X => Machine_Occurrence.Occurrence);
      Propagate_Machine_Occurrence (Machine_Occurrence);
   end Raise_Exception;

   procedure Raise_E (
      E : Exception_Data_Access;
      Message : String)
   is
      Actual_E : Exception_Data_Access := E;
   begin
      if Actual_E = null then
         Actual_E := Unwind.Standard.Constraint_Error'Access;
      end if;
      Raise_Exception (Actual_E, Message => Message);
   end Raise_E;

   procedure Raise_Exception_From_Here (
      E : not null Exception_Data_Access;
      File : String := Ada.Debug.File;
      Line : Integer := Ada.Debug.Line) is
   begin
      Raise_Exception (E, File, Line, Message => Explicit_Raise);
   end Raise_Exception_From_Here;

   procedure Raise_Exception_From_Here_With (
      E : not null Exception_Data_Access;
      File : String := Ada.Debug.File;
      Line : Integer := Ada.Debug.Line;
      Message : String) is
   begin
      Raise_Exception (E, File, Line, Message => Message);
   end Raise_Exception_From_Here_With;

   procedure Reraise_No_Defer (X : Exception_Occurrence) is
      Machine_Occurrence : Representation.Machine_Occurrence_Access;
   begin
      pragma Check (Trace, Ada.Debug.Put ("reraising..."));
      Machine_Occurrence := Occurrences.New_Machine_Occurrence (
         Stack_Guard => Null_Address);
      Machine_Occurrence.Occurrence := X;
      Propagate_Machine_Occurrence (Machine_Occurrence);
   end Reraise_No_Defer;

   procedure Reraise (X : Exception_Occurrence) is
   begin
      if not ZCX_By_Default then
         Synchronous_Control.Lock_Abort;
      end if;
      Reraise_No_Defer (X);
   end Reraise;

   procedure Reraise_From_All_Others (X : Exception_Occurrence)
      renames Reraise_No_Defer;

   procedure Reraise_From_Controlled_Operation (X : Exception_Occurrence) is
      Prefix : constant String := "adjust/finalize raised ";
   begin
      if X.Id = Unwind.Standard.Program_Error'Access
         and then X.Msg_Length >= Prefix'Length
         and then X.Msg (1 .. Prefix'Length) = Prefix
      then
         Reraise_No_Defer (X);
      else
         declare
            Full_Name : String (1 .. X.Id.Name_Length - 1);
            for Full_Name'Address use X.Id.Full_Name;
            New_Message : String (1 .. Default_Exception_Msg_Max_Length);
            Last : Natural := New_Message'First - 1;
         begin
            New_Message (Last + 1 .. Last + Prefix'Length) := Prefix;
            Last := Prefix'Length;
            New_Message (Last + 1 .. Last + Full_Name'Length) := Full_Name;
            Last := Last + (X.Id.Name_Length - 1);
            if X.Msg_Length > 0 then
               New_Message (Last + 1 .. Last + 2) := ": ";
               Last := Last + 2;
               New_Message (Last + 1 .. Last + X.Msg_Length) :=
                  X.Msg (1 .. X.Msg_Length);
               Last := Last + X.Msg_Length;
            end if;
            Raise_Exception_No_Defer (
               Unwind.Standard.Program_Error'Access,
               Message => New_Message (New_Message'First .. Last));
         end;
      end if;
   end Reraise_From_Controlled_Operation;

   procedure Reraise_Machine_Occurrence (
      Machine_Occurrence : not null Representation.Machine_Occurrence_Access)
      renames Separated.Propagate_Machine_Occurrence;

   procedure Reraise_Library_Exception_If_Any is
   begin
      if Startup.Library_Exception_Set then
         declare
            X : Exception_Occurrence
               renames Startup.Library_Exception.X;
         begin
            if X.Id = null then
               Raise_Exception_No_Defer (
                  Unwind.Standard.Program_Error'Access,
                  Message => From_Finalize);
            else
               Reraise_From_Controlled_Operation (X);
            end if;
         end;
      end if;
   end Reraise_Library_Exception_If_Any;

   procedure Raise_Program_Error is
      Message : constant String := "not supported";
   begin
      Raise_Exception (
         Unwind.Standard.Program_Error'Access,
         Message => Message);
   end Raise_Program_Error;

   procedure Raise_From_rcheck (
      File : not null access Character;
      Line : Integer;
      E : not null Exception_Data_Access;
      Message : String)
   is
      File_S : String (1 .. Natural (strlen (File)));
      for File_S'Address use File.all'Address;
   begin
      Raise_Exception (E, File_S, Line, Message => Message);
   end Raise_From_rcheck;

   procedure rcheck_00 (File : not null access Character; Line : Integer) is
      Message : constant String := "access check failed";
   begin
      Raise_From_rcheck (
         File,
         Line,
         Unwind.Standard.Constraint_Error'Access,
         Message);
   end rcheck_00;

   procedure rcheck_02 (File : not null access Character; Line : Integer) is
      Message : constant String := "discriminant check failed";
   begin
      Raise_From_rcheck (
         File,
         Line,
         Unwind.Standard.Constraint_Error'Access,
         Message);
   end rcheck_02;

   procedure rcheck_03 (File : not null access Character; Line : Integer) is
   begin
      Raise_From_rcheck (
         File,
         Line,
         Unwind.Standard.Constraint_Error'Access,
         Divide_By_Zero);
   end rcheck_03;

   procedure Zero_Division (
      File : String := Ada.Debug.File;
      Line : Integer := Ada.Debug.Line) is
   begin
      Raise_Exception (
         Unwind.Standard.Constraint_Error'Access,
         File,
         Line,
         Message => Divide_By_Zero);
   end Zero_Division;

   procedure rcheck_04 (File : not null access Character; Line : Integer) is
   begin
      Raise_From_rcheck (
         File,
         Line,
         Unwind.Standard.Constraint_Error'Access,
         Explicit_Raise);
   end rcheck_04;

   procedure rcheck_05 (File : not null access Character; Line : Integer) is
      Message : constant String := "index check failed";
   begin
      Raise_From_rcheck (
         File,
         Line,
         Unwind.Standard.Constraint_Error'Access,
         Message);
   end rcheck_05;

   procedure rcheck_06 (File : not null access Character; Line : Integer) is
      Message : constant String := "invalid data";
   begin
      Raise_From_rcheck (
         File,
         Line,
         Unwind.Standard.Constraint_Error'Access,
         Message);
   end rcheck_06;

   procedure rcheck_07 (File : not null access Character; Line : Integer) is
      Message : constant String := "length check failed";
   begin
      Raise_From_rcheck (
         File,
         Line,
         Unwind.Standard.Constraint_Error'Access,
         Message);
   end rcheck_07;

   procedure rcheck_09 (File : not null access Character; Line : Integer) is
      Message : constant String := "null-exclusion check failed";
   begin
      Raise_From_rcheck (
         File,
         Line,
         Unwind.Standard.Constraint_Error'Access,
         Message);
   end rcheck_09;

   procedure rcheck_10 (File : not null access Character; Line : Integer) is
   begin
      Raise_From_rcheck (
         File,
         Line,
         Unwind.Standard.Constraint_Error'Access,
         Overflow_Check_Failed);
   end rcheck_10;

   procedure Overflow (
      File : String := Ada.Debug.File;
      Line : Integer := Ada.Debug.Line) is
   begin
      Raise_Exception (
         Unwind.Standard.Constraint_Error'Access,
         File,
         Line,
         Message => Overflow_Check_Failed);
   end Overflow;

   procedure rcheck_12 (File : not null access Character; Line : Integer) is
      Message : constant String := "range check failed";
   begin
      Raise_From_rcheck (
         File,
         Line,
         Unwind.Standard.Constraint_Error'Access,
         Message);
   end rcheck_12;

   procedure rcheck_13 (File : not null access Character; Line : Integer) is
      Message : constant String := "tag check failed";
   begin
      Raise_From_rcheck (
         File,
         Line,
         Unwind.Standard.Constraint_Error'Access,
         Message);
   end rcheck_13;

   procedure rcheck_14 (File : not null access Character; Line : Integer) is
      Message : constant String := "access before elaboration";
   begin
      Raise_From_rcheck (
         File,
         Line,
         Unwind.Standard.Program_Error'Access,
         Message => Message);
   end rcheck_14;

   procedure rcheck_15 (File : not null access Character; Line : Integer) is
      Message : constant String := "accessibility check failed";
   begin
      Raise_From_rcheck (
         File,
         Line,
         Unwind.Standard.Program_Error'Access,
         Message);
   end rcheck_15;

   procedure rcheck_22 (File : not null access Character; Line : Integer) is
   begin
      Raise_From_rcheck (
         File,
         Line,
         Unwind.Standard.Program_Error'Access,
         Explicit_Raise);
   end rcheck_22;

   procedure rcheck_23 (File : not null access Character; Line : Integer) is
      File_S : String (1 .. Natural (strlen (File)));
      for File_S'Address use File.all'Address;
   begin
      Raise_Exception_No_Defer (
         Unwind.Standard.Program_Error'Access,
         File_S,
         Line,
         Message => From_Finalize);
   end rcheck_23;

   procedure rcheck_24 (File : not null access Character; Line : Integer) is
      Message : constant String := "implicit return with No_Return";
   begin
      Raise_From_rcheck (
         File,
         Line,
         Unwind.Standard.Program_Error'Access,
         Message);
   end rcheck_24;

   procedure rcheck_25 (File : not null access Character; Line : Integer) is
      Message : constant String := "misaligned address value";
   begin
      Raise_From_rcheck (
         File,
         Line,
         Unwind.Standard.Program_Error'Access,
         Message);
   end rcheck_25;

   procedure rcheck_26 (File : not null access Character; Line : Integer) is
      Message : constant String := "missing return";
   begin
      Raise_From_rcheck (
         File,
         Line,
         Unwind.Standard.Program_Error'Access,
         Message);
   end rcheck_26;

   procedure rcheck_27 (File : not null access Character; Line : Integer) is
      Message : constant String := "overlaid controlled object";
   begin
      Raise_From_rcheck (
         File,
         Line,
         Unwind.Standard.Program_Error'Access,
         Message);
   end rcheck_27;

   procedure rcheck_30 (File : not null access Character; Line : Integer) is
      Message : constant String := "unchecked union restriction";
   begin
      Raise_From_rcheck (
         File,
         Line,
         Unwind.Standard.Program_Error'Access,
         Message);
   end rcheck_30;

   procedure rcheck_32 (File : not null access Character; Line : Integer) is
      Message : constant String := "empty storage pool";
   begin
      Raise_From_rcheck (
         File,
         Line,
         Unwind.Standard.Storage_Error'Access,
         Message);
   end rcheck_32;

   procedure rcheck_33 (File : not null access Character; Line : Integer) is
   begin
      Raise_From_rcheck (
         File,
         Line,
         Unwind.Standard.Storage_Error'Access,
         Explicit_Raise);
   end rcheck_33;

   procedure rcheck_35 (File : not null access Character; Line : Integer) is
      Message : constant String := "object too large";
   begin
      Raise_From_rcheck (
         File,
         Line,
         Unwind.Standard.Storage_Error'Access,
         Message);
   end rcheck_35;

   procedure Save_Exception (
      X : out Exception_Occurrence;
      E : not null Exception_Data_Access;
      File : String := "";
      Line : Integer := 0;
      Column : Integer := 0;
      Message : String := "") is
   begin
      Occurrences.Set_Exception_Message (E, File, Line, Column, Message, X);
      Occurrences.Backtrace (X);
   end Save_Exception;

   procedure Save_E (
      X : out Exception_Occurrence;
      E : not null Exception_Data_Access;
      Message : String) is
   begin
      Save_Exception (X, E, Message => Message);
   end Save_E;

   procedure Save_Exception_From_Here (
      X : out Exception_Occurrence;
      E : not null Exception_Data_Access;
      File : String := Ada.Debug.File;
      Line : Integer := Ada.Debug.Line) is
   begin
      Save_Exception (X, E, File, Line, Message => Explicit_Raise);
   end Save_Exception_From_Here;

   procedure Save_Exception_From_Here_With (
      X : out Exception_Occurrence;
      E : not null Exception_Data_Access;
      File : String := Ada.Debug.File;
      Line : Integer := Ada.Debug.Line;
      Message : String) is
   begin
      Save_Exception (X, E, File, Line, Message => Message);
   end Save_Exception_From_Here_With;

   --  at last of exclusion
   function ZZZ return Address is
   begin
      <<Code>>
      return Code'Address;
   end ZZZ;

   procedure Debug_Raise_Exception (E : not null Exception_Data_Access) is
      pragma Inspection_Point (E);
   begin
      null;
   end Debug_Raise_Exception;

end System.Unwind.Raising;
