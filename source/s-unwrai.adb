pragma Check_Policy (Trace, Off);
with System.Formatting;
with System.Runtime_Context;
with System.Soft_Links;
with System.Storage_Elements;
with System.Termination;
with System.Unwind.Standard;
package body System.Unwind.Raising is
   pragma Suppress (All_Checks);

   --  package separated for depending on zcx / sjlj
   package Separated is

      --  (a-exexpr-gcc.adb)
      procedure Propagate_Exception (
         X : Exception_Occurrence;
         Stack_Guard : Address);
      pragma No_Return (Propagate_Exception);

   end Separated;

   function strlen (s : not null access Character)
      return Storage_Elements.Storage_Count;
   pragma Import (Intrinsic, strlen, "__builtin_strlen");

   --  weak reference for System.Unwind.Tracebacks (ELF only ?)
   Call_Chain : access procedure (
      Current : not null Exception_Occurrence_Access);
   pragma Import (Ada, Call_Chain, "__drake_ref_call_chain");
   pragma Weak_External (Call_Chain);
   Report_Traceback : access procedure (
      Current : Exception_Occurrence);
   pragma Import (Ada, Report_Traceback, "__drake_ref_report_traceback");
   pragma Weak_External (Report_Traceback);

   --  (a-elchha.ads)
   procedure Last_Chance_Handler (
      Current : Exception_Occurrence);
   pragma No_Return (Last_Chance_Handler);
   procedure Last_Chance_Handler (
      Current : Exception_Occurrence)
   is
      subtype Fixed_String is String (Positive);
      Full_Name : Fixed_String;
      for Full_Name'Address use Current.Id.Full_Name;
   begin
      pragma Check (Trace, Ada.Debug.Put ("enter"));
      --  in GNAT runtime, task termination handler will be unset
      --  and Standard_Library.AdaFinal will be called here
      Termination.Error_New_Line;
      if Full_Name (1) = '_' then -- Standard'Abort_Signal
         Termination.Error_Put (
            "Execution terminated by abort of environment task");
         Termination.Error_New_Line;
      elsif Current.Num_Tracebacks > 0
         and then Report_Traceback'Address /= Null_Address
      then
         Termination.Error_Put ("Execution terminated by unhandled exception");
         Termination.Error_New_Line;
         Report_Traceback (Current);
      else
         Termination.Error_Put ("raised ");
         Termination.Error_Put (Full_Name (1 .. Current.Id.Name_Length - 1));
         if Current.Msg_Length > 0 then
            Termination.Error_Put (" : ");
            Termination.Error_Put (Current.Msg (1 .. Current.Msg_Length));
         end if;
         Termination.Error_New_Line;
      end if;
      Termination.Force_Abort;
   end Last_Chance_Handler;

   --  (a-exextr.adb)
   procedure Unhandled_Exception_Terminate (
      Current : not null Exception_Occurrence_Access);
   pragma No_Return (Unhandled_Exception_Terminate);
   procedure Unhandled_Exception_Terminate (
      Current : not null Exception_Occurrence_Access) is
   begin
      Last_Chance_Handler (Current.all);
   end Unhandled_Exception_Terminate;

   type Uninitialized_Exception_Occurrence is record
      X : Exception_Occurrence;
   end record;
   pragma Suppress_Initialization (Uninitialized_Exception_Occurrence);

   --  equivalent to Set_Exception_C_Msg (a-exexda.adb)
   procedure Set_Exception_Message (
      Id : Exception_Data_Access;
      File : String;
      Line : Integer;
      Column : Integer;
      Message : String;
      X : in out Exception_Occurrence);
   procedure Set_Exception_Message (
      Id : Exception_Data_Access;
      File : String;
      Line : Integer;
      Column : Integer;
      Message : String;
      X : in out Exception_Occurrence) is
   begin
      X.Id := Id;
      declare
         File_Length : constant Natural := File'Length;
         Last : Natural := 0;
      begin
         if File_Length > 0 then
            X.Msg (1 .. File_Length) := File;
            Last := File_Length + 1;
            X.Msg (Last) := ':';
         end if;
         if Line > 0 then
            declare
               Error : Boolean;
            begin
               Formatting.Image (
                  Formatting.Unsigned (Line),
                  X.Msg (Last + 1 .. X.Msg'Last),
                  Last,
                  Error => Error);
               if not Error and then Last < X.Msg'Last then
                  Last := Last + 1;
                  X.Msg (Last) := ':';
               end if;
            end;
         end if;
         if Column > 0 then
            declare
               Error : Boolean;
            begin
               Formatting.Image (
                  Formatting.Unsigned (Column),
                  X.Msg (Last + 1 .. X.Msg'Last),
                  Last,
                  Error => Error);
               if not Error and then Last < X.Msg'Last then
                  Last := Last + 1;
                  X.Msg (Last) := ':';
               end if;
            end;
         end if;
         if (File_Length > 0 or else Line > 0 or else Column > 0)
            and then Last < X.Msg'Last
         then
            Last := Last + 1;
            X.Msg (Last) := ' ';
         end if;
         declare
            Copy_Length : constant Natural := Integer'Min (
               Message'Length,
               X.Msg'Length - Last);
         begin
            X.Msg (Last + 1 .. Last + Copy_Length) :=
            Message (Message'First .. Message'First + Copy_Length - 1);
            Last := Last + Copy_Length;
         end;
         if Last < X.Msg'Last then
            --  no necessary
            X.Msg (Last + 1) := Character'Val (0);
         end if;
         X.Msg_Length := Last;
      end;
      X.Exception_Raised := False;
      X.Pid := Local_Partition_ID;
      X.Num_Tracebacks := 0;
   end Set_Exception_Message;

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

   package body Separated is separate;

   procedure Raise_Exception_No_Defer (
      E : not null Exception_Data_Access;
      File : String := "";
      Line : Integer := 0;
      Column : Integer := 0;
      Message : String := "")
   is
      X : Uninitialized_Exception_Occurrence;
   begin
      Set_Exception_Message (E, File, Line, Column, Message, X.X);
      Separated.Propagate_Exception (X.X, Stack_Guard => Null_Address);
   end Raise_Exception_No_Defer;

   procedure Raise_Exception (
      E : not null Exception_Data_Access;
      File : String := "";
      Line : Integer := 0;
      Column : Integer := 0;
      Message : String := "";
      Stack_Guard : Address := Null_Address)
   is
      X : Uninitialized_Exception_Occurrence;
   begin
      Set_Exception_Message (E, File, Line, Column, Message, X.X);
      if not ZCX_By_Default then
         Soft_Links.Abort_Defer.all;
      end if;
      Separated.Propagate_Exception (X.X, Stack_Guard => Stack_Guard);
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
   begin
      pragma Check (Trace, Ada.Debug.Put ("reraising..."));
      Separated.Propagate_Exception (X, Stack_Guard => Null_Address);
   end Reraise_No_Defer;

   procedure Reraise (X : Exception_Occurrence) is
   begin
      if not ZCX_By_Default then
         Soft_Links.Abort_Defer.all;
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

   procedure rcheck_21 (File : not null access Character; Line : Integer) is
   begin
      Raise_From_rcheck (
         File,
         Line,
         Unwind.Standard.Program_Error'Access,
         Explicit_Raise);
   end rcheck_21;

   procedure rcheck_22 (File : not null access Character; Line : Integer) is
      File_S : String (1 .. Natural (strlen (File)));
      for File_S'Address use File.all'Address;
   begin
      Raise_Exception_No_Defer (
         Unwind.Standard.Program_Error'Access,
         File_S,
         Line,
         Message => From_Finalize);
   end rcheck_22;

   procedure rcheck_23 (File : not null access Character; Line : Integer) is
      Message : constant String := "implicit return with No_Return";
   begin
      Raise_From_rcheck (
         File,
         Line,
         Unwind.Standard.Program_Error'Access,
         Message);
   end rcheck_23;

   procedure rcheck_24 (File : not null access Character; Line : Integer) is
      Message : constant String := "misaligned address value";
   begin
      Raise_From_rcheck (
         File,
         Line,
         Unwind.Standard.Program_Error'Access,
         Message);
   end rcheck_24;

   procedure rcheck_25 (File : not null access Character; Line : Integer) is
      Message : constant String := "missing return";
   begin
      Raise_From_rcheck (
         File,
         Line,
         Unwind.Standard.Program_Error'Access,
         Message);
   end rcheck_25;

   procedure rcheck_26 (File : not null access Character; Line : Integer) is
      Message : constant String := "overlaid controlled object";
   begin
      Raise_From_rcheck (
         File,
         Line,
         Unwind.Standard.Program_Error'Access,
         Message);
   end rcheck_26;

   procedure rcheck_29 (File : not null access Character; Line : Integer) is
      Message : constant String := "unchecked union restriction";
   begin
      Raise_From_rcheck (
         File,
         Line,
         Unwind.Standard.Program_Error'Access,
         Message);
   end rcheck_29;

   procedure rcheck_31 (File : not null access Character; Line : Integer) is
      Message : constant String := "empty storage pool";
   begin
      Raise_From_rcheck (
         File,
         Line,
         Unwind.Standard.Storage_Error'Access,
         Message);
   end rcheck_31;

   procedure rcheck_32 (File : not null access Character; Line : Integer) is
   begin
      Raise_From_rcheck (
         File,
         Line,
         Unwind.Standard.Storage_Error'Access,
         Explicit_Raise);
   end rcheck_32;

   procedure rcheck_34 (File : not null access Character; Line : Integer) is
      Message : constant String := "object too large";
   begin
      Raise_From_rcheck (
         File,
         Line,
         Unwind.Standard.Storage_Error'Access,
         Message);
   end rcheck_34;

   --  at last of exclusion
   function ZZZ return Address is
   begin
      <<Code>>
      return Code'Address;
   end ZZZ;

   function Triggered_By_Abort return Boolean is
      TLS : constant not null Runtime_Context.Task_Local_Storage_Access :=
         Runtime_Context.Get_Task_Local_Storage;
      X : constant not null Exception_Occurrence_Access :=
         TLS.Current_Exception'Access;
   begin
      return X.Id = Standard.Abort_Signal'Access;
   end Triggered_By_Abort;

end System.Unwind.Raising;
