pragma Check_Policy (Trace, Off);
with System.Address_To_Named_Access_Conversions;
with System.Formatting;
with System.Startup;
with System.Storage_Elements;
with System.Synchronous_Control;
with System.Termination;
with System.Unwind.Standard;
package body System.Unwind.Raising is
   pragma Suppress (All_Checks);
   use type Representation.Machine_Occurrence_Access;
   use type Representation.Unwind_Exception_Class;

   --  package separated for depending on libgcc
   package Separated is

      --  equivalent to Allocate_Occurrence (a-exexpr-gcc.adb)
      function New_Machine_Occurrence
         return not null Representation.Machine_Occurrence_Access;

      procedure Free (
         Machine_Occurrence : Representation.Machine_Occurrence_Access);

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

   --  for Set_Foreign_Occurrence

   Foreign_Exception : aliased Exception_Data;
   pragma Import (Ada, Foreign_Exception,
      "system__exceptions__foreign_exception");

   --  weak reference for System.Unwind.Backtrace

   Call_Chain : access procedure (Current : in out Exception_Occurrence);
   pragma Import (Ada, Call_Chain, "__drake_ref_call_chain");
   pragma Weak_External (Call_Chain);

   Report_Backtrace : access procedure (X : Exception_Occurrence);
   pragma Import (Ada, Report_Backtrace, "__drake_ref_report_backtrace");
   pragma Weak_External (Report_Backtrace);

   --  (a-elchha.ads)
   procedure Last_Chance_Handler (
      Current : Exception_Occurrence);
   pragma No_Return (Last_Chance_Handler);
   procedure Last_Chance_Handler (
      Current : Exception_Occurrence) is
   begin
      pragma Check (Trace, Ada.Debug.Put ("enter"));
      --  in GNAT runtime, task termination handler will be unset
      --  and Standard_Library.AdaFinal will be called here
      Report (Current, "");
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
      Backtrace (Machine_Occurrence.Occurrence);
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
      Machine_Occurrence := New_Machine_Occurrence (
         Stack_Guard => Null_Address);
      Set_Exception_Message (E, File, Line, Column, Message,
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
      Machine_Occurrence := New_Machine_Occurrence (
         Stack_Guard => Stack_Guard);
      Set_Exception_Message (E, File, Line, Column, Message,
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
      Machine_Occurrence := New_Machine_Occurrence (
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

   procedure Save_Exception (
      X : out Exception_Occurrence;
      E : not null Exception_Data_Access;
      File : String := "";
      Line : Integer := 0;
      Column : Integer := 0;
      Message : String := "") is
   begin
      Set_Exception_Message (E, File, Line, Column, Message, X);
      Backtrace (X);
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

   procedure Backtrace (X : in out Exception_Occurrence) is
   begin
      if Call_Chain'Address /= Null_Address then
         Call_Chain (X);
      end if;
   end Backtrace;

   procedure Set_Exception_Message (
      Id : not null Exception_Data_Access;
      File : String := "";
      Line : Integer := 0;
      Column : Integer := 0;
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
      X.Machine_Occurrence := Null_Address;
      X.Exception_Raised := False;
      X.Pid := Local_Partition_ID;
      X.Num_Tracebacks := 0;
   end Set_Exception_Message;

   --  at last of exclusion
   function ZZZ return Address is
   begin
      <<Code>>
      return Code'Address;
   end ZZZ;

   function New_Machine_Occurrence (Stack_Guard : Address)
      return not null Representation.Machine_Occurrence_Access
   is
      Result : constant not null Representation.Machine_Occurrence_Access :=
         Separated.New_Machine_Occurrence;
   begin
      Result.Stack_Guard := Stack_Guard;
      return Result;
   end New_Machine_Occurrence;

   procedure Free (
      Machine_Occurrence : Representation.Machine_Occurrence_Access)
      renames Separated.Free;

   procedure Set_Current_Machine_Occurrence (
      Machine_Occurrence : Representation.Machine_Occurrence_Access)
   is
      TLS : constant not null Runtime_Context.Task_Local_Storage_Access :=
         Runtime_Context.Get_Task_Local_Storage;
   begin
      TLS.Machine_Occurrence := Machine_Occurrence;
   end Set_Current_Machine_Occurrence;

   function Get_Current_Occurrence (
      TLS : not null Runtime_Context.Task_Local_Storage_Access)
      return Exception_Occurrence_Access
   is
      package MOA_Conv is
         new Address_To_Named_Access_Conversions (
            Representation.Machine_Occurrence,
            Representation.Machine_Occurrence_Access);
      Machine_Occurrence : constant
         not null Representation.Machine_Occurrence_Access :=
         TLS.Machine_Occurrence;
      Result : Exception_Occurrence_Access;
   begin
      if Machine_Occurrence.Header.exception_class =
         Representation.GNAT_Exception_Class
      then
         Result := Machine_Occurrence.Occurrence'Access;
      else
         Result := TLS.Foreign_Occurrence'Access;
         Result.Id := Foreign_Exception'Access;
         Result.Machine_Occurrence :=
            MOA_Conv.To_Address (Machine_Occurrence);
         Result.Msg_Length := 0;
         Result.Exception_Raised := True;
         Result.Pid := Local_Partition_ID;
         Result.Num_Tracebacks := 0;
      end if;
      return Result;
   end Get_Current_Occurrence;

   function Triggered_By_Abort return Boolean is
      TLS : constant not null Runtime_Context.Task_Local_Storage_Access :=
         Runtime_Context.Get_Task_Local_Storage;
      Machine_Occurrence : constant
         not null Representation.Machine_Occurrence_Access :=
         TLS.Machine_Occurrence;
      Result : Boolean;
   begin
      Result := Machine_Occurrence /= null
         and then Machine_Occurrence.Header.exception_class =
            Representation.GNAT_Exception_Class;
      if Result then
         declare
            subtype Fixed_String is String (Positive);
            Full_Name : Fixed_String;
            for Full_Name'Address use
               Machine_Occurrence.Occurrence.Id.Full_Name;
         begin
            Result := Full_Name (1) = '_'; -- Standard'Abort_Signal
         end;
      end if;
      return Result;
   end Triggered_By_Abort;

   procedure Report (X : Exception_Occurrence; Where : String) is
      subtype Buffer_Type is String (1 .. 256 + Exception_Msg_Max_Length);
      procedure Put (
         Buffer : in out Buffer_Type;
         Last : in out Natural;
         S : String);
      procedure Put (
         Buffer : in out Buffer_Type;
         Last : in out Natural;
         S : String)
      is
         First : constant Natural := Last + 1;
      begin
         Last := Last + S'Length;
         Buffer (First .. Last) := S;
      end Put;
      procedure Put_Upper (
         Buffer : in out Buffer_Type;
         Last : out Natural;
         Where : String);
      procedure Put_Upper (
         Buffer : in out Buffer_Type;
         Last : out Natural;
         Where : String)
      is
         type Character_Code is mod 2 ** Character'Size;
      begin
         Last := 0;
         if Where'Length > 0 then
            Put (Buffer, Last, Where);
            Buffer (1) := Character'Val (
               Character_Code'(Character'Pos (Buffer (1)))
               and not 16#20#); -- upper-case
         else
            Put (Buffer, Last, "Execution");
         end if;
      end Put_Upper;
      subtype Fixed_String is String (Positive);
      Full_Name : Fixed_String;
      for Full_Name'Address use X.Id.Full_Name;
      Buffer : Buffer_Type;
      Last : Natural;
   begin
      Termination.Error_Put_Line ("");
      if Full_Name (1) = '_' then -- Standard'Abort_Signal
         Put_Upper (Buffer, Last, Where);
         Put (Buffer, Last, " terminated by abort");
         if Where'Length = 0 then
            Put (Buffer, Last, " of environment task");
         end if;
         Termination.Error_Put_Line (Buffer (1 .. Last));
      elsif X.Num_Tracebacks > 0
         and then Report_Backtrace'Address /= Null_Address
      then
         Put_Upper (Buffer, Last, Where);
         Put (Buffer, Last, " terminated by unhandled exception");
         Termination.Error_Put_Line (Buffer (1 .. Last));
         Report_Backtrace (X);
      else
         Last := 0;
         if Where'Length > 0 then
            Put (Buffer, Last, "in ");
            Put (Buffer, Last, Where);
            Put (Buffer, Last, ", ");
         end if;
         Put (Buffer, Last, "raised ");
         Put (Buffer, Last, Full_Name (1 .. X.Id.Name_Length - 1));
         if X.Msg_Length > 0 then
            Put (Buffer, Last, " : ");
            Put (Buffer, Last, X.Msg (1 .. X.Msg_Length));
         end if;
         Termination.Error_Put_Line (Buffer (1 .. Last));
      end if;
   end Report;

   procedure Unhandled_Except_Handler (
      Machine_Occurrence : not null Representation.Machine_Occurrence_Access)
   is
      TLS : constant not null Runtime_Context.Task_Local_Storage_Access :=
         Runtime_Context.Get_Task_Local_Storage;
      Current : Exception_Occurrence_Access;
   begin
      pragma Check (Trace, Ada.Debug.Put ("enter"));
      TLS.Machine_Occurrence := Machine_Occurrence;
      Current := Get_Current_Occurrence (TLS);
      Unhandled_Exception_Terminate (Current);
   end Unhandled_Except_Handler;

   procedure Debug_Raise_Exception (E : not null Exception_Data_Access) is
      pragma Inspection_Point (E);
   begin
      null;
   end Debug_Raise_Exception;

end System.Unwind.Raising;
