pragma Check_Policy (Trace => Ignore);
with Ada; -- assertions
with System.Address_To_Named_Access_Conversions;
with System.Formatting;
with System.Long_Long_Integer_Types;
with System.Storage_Elements;
with System.Termination;
package body System.Unwind.Occurrences is
   pragma Suppress (All_Checks);
   use type Storage_Elements.Storage_Offset;
   use type Representation.Machine_Occurrence_Access;
   use type Representation.Unwind_Exception_Class;

   subtype Word_Unsigned is Long_Long_Integer_Types.Word_Unsigned;

   --  package separated for depending on libgcc
   package Separated is

      --  equivalent to Allocate_Occurrence (a-exexpr-gcc.adb)
      function New_Machine_Occurrence
         return not null Representation.Machine_Occurrence_Access;

      procedure Free (
         Machine_Occurrence : Representation.Machine_Occurrence_Access);

   end Separated;

   package body Separated is separate;

   --  for Save_Occurrence

   procedure memcpy (
      dst, src : Address;
      n : Storage_Elements.Storage_Count)
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_memcpy";

   --  for Set_Foreign_Occurrence

   package MOA_Conv is
      new Address_To_Named_Access_Conversions (
         Representation.Machine_Occurrence,
         Representation.Machine_Occurrence_Access);

   Foreign_Exception : aliased Exception_Data
      with Import,
         Convention => Ada,
         External_Name => "system__exceptions__foreign_exception";

   --  weak references for System.Unwind.Backtrace

   procedure Call_Chain (Current : in out Exception_Occurrence)
      with Import, -- weak linking
         Convention => Ada, External_Name => "ada__exceptions__call_chain";
   pragma Weak_External (Call_Chain);

   procedure Backtrace_Information (
      X : Exception_Occurrence;
      Params : Address;
      Put : not null access procedure (S : String; Params : Address);
      New_Line : not null access procedure (Params : Address))
      with Import, -- weak linking
         Convention => Ada, External_Name => "__drake_backtrace_information";
   pragma Weak_External (Backtrace_Information);

   --  buffered writing to standard error output

   type Information_Context_Type is record
      Item : String (
         1 ..
         256
            + Exception_Msg_Max_Length
            + Max_Tracebacks * (3 + (Standard'Address_Size + 3) / 4));
      Last : Natural;
   end record;
   pragma Suppress_Initialization (Information_Context_Type);

   procedure Put (S : String; Params : Address);
   procedure New_Line (Params : Address);

   procedure Put (S : String; Params : Address) is
      Context : Information_Context_Type;
      for Context'Address use Params;
      First : constant Positive := Context.Last + 1;
   begin
      Context.Last := Context.Last + S'Length;
      Context.Item (First .. Context.Last) := S;
   end Put;

   procedure New_Line (Params : Address) is
      Context : Information_Context_Type;
      for Context'Address use Params;
   begin
      Termination.Error_Put_Line (Context.Item (1 .. Context.Last));
      Context.Last := 0;
   end New_Line;

   --  (a-elchha.ads)
   procedure Last_Chance_Handler (
      Current : Exception_Occurrence);
   pragma No_Return (Last_Chance_Handler);

   procedure Last_Chance_Handler (
      Current : Exception_Occurrence) is
   begin
      pragma Check (Trace, Ada.Debug.Put ("enter"));
      --  Only flush Text_IO, similar to abort that flushes stdio and does not
      --    finalize any others (atexit, __attribute__((destructor)), etc).
      --  GNAT runtime calls System.Standard_Library.Adafinal to complete tasks
      --    here, but drake does not it according with abort.
      Flush_IO;
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

   --  implementation

   procedure Save_Occurrence (
      Target : out Exception_Occurrence;
      Source : Exception_Occurrence) is
   begin
      memcpy (
         Target'Address,
         Source'Address,
         Target.Tracebacks'Position
            + (Tracebacks_Array'Component_Size / Standard'Storage_Unit)
               * Storage_Elements.Storage_Offset (Source.Num_Tracebacks));
      Target.Machine_Occurrence := Null_Address;
      Target.Exception_Raised := False;
   end Save_Occurrence;

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
                  Word_Unsigned (Line),
                  X.Msg (Last + 1 .. X.Msg'Last),
                  Last,
                  Error => Error);
            end;
         end if;
         if Column > 0 then
            if Last < X.Msg'Last then
               Last := Last + 1;
               X.Msg (Last) := ':';
            end if;
            declare
               Error : Boolean;
            begin
               Formatting.Image (
                  Word_Unsigned (Column),
                  X.Msg (Last + 1 .. X.Msg'Last),
                  Last,
                  Error => Error);
            end;
         end if;
         if (File_Length > 0 or else Line > 0 or else Column > 0)
            and then Last < X.Msg'Last
         then
            Last := Last + 1;
            X.Msg (Last) := ' ';
         end if;
         declare
            Copy_Length : constant Natural :=
               Integer'Min (Message'Length, X.Msg'Length - Last);
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

   procedure Set_Foreign_Occurrence (
      X : in out Exception_Occurrence;
      Machine_Occurrence :
         not null Representation.Machine_Occurrence_Access) is
   begin
      X.Id := Foreign_Exception'Access;
      X.Machine_Occurrence := MOA_Conv.To_Address (Machine_Occurrence);
      X.Msg_Length := 0;
      X.Exception_Raised := True;
      X.Pid := Local_Partition_ID;
      X.Num_Tracebacks := 0;
   end Set_Foreign_Occurrence;

   function Get_Current_Occurrence (
      TLS : not null Runtime_Context.Task_Local_Storage_Access)
      return Exception_Occurrence_Access
   is
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
         Result := TLS.Secondary_Occurrence.Occurrence'Access;
         Set_Foreign_Occurrence (Result.all, Machine_Occurrence);
      end if;
      return Result;
   end Get_Current_Occurrence;

   procedure Set_Current_Machine_Occurrence (
      Machine_Occurrence : Representation.Machine_Occurrence_Access)
   is
      TLS : constant not null Runtime_Context.Task_Local_Storage_Access :=
         Runtime_Context.Get_Task_Local_Storage;
   begin
      TLS.Machine_Occurrence := Machine_Occurrence;
   end Set_Current_Machine_Occurrence;

   function Triggered_By_Abort return Boolean is
      TLS : constant not null Runtime_Context.Task_Local_Storage_Access :=
         Runtime_Context.Get_Task_Local_Storage;
   begin
      --  Strictly speaking, this conditional expression suppresses to call
      --    __gnat_rcheck_PE_Finalize_Raised_Exception from nested blocks
      --    excessively.
      --  However, TLS.Machine_Occurrence = null because Triggered_By_Abort is
      --    called from finalizers, without Begin_Handler/End_Handler.
      return TLS.Triggered_By_Abort;
   end Triggered_By_Abort;

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

   procedure Exception_Information (
      X : Exception_Occurrence;
      Params : Address;
      Put : not null access procedure (S : String; Params : Address);
      New_Line : not null access procedure (Params : Address)) is
   begin
      Put ("raised ", Params);
      declare
         Full_Name_All : String (1 .. X.Id.Name_Length - 1);
         for Full_Name_All'Address use X.Id.Full_Name;
      begin
         Put (Full_Name_All, Params);
      end;
      if X.Msg_Length > 0 then
         Put (" : ", Params);
         Put (X.Msg (1 .. X.Msg_Length), Params);
      end if;
      New_Line (Params);
      if X.Pid /= 0 then
         null; -- output X.Pid is unimplemented
      end if;
      if X.Num_Tracebacks > 0
         and then Backtrace_Information'Address /= Null_Address
      then
         Backtrace_Information (
            X,
            Params,
            Put => Put,
            New_Line => New_Line);
      end if;
   end Exception_Information;

   procedure Flush_IO is
   begin
      Flush_IO_Hook.all;
   end Flush_IO;

   procedure Put_Exception_Information (X : Exception_Occurrence) is
      Context : aliased Information_Context_Type;
   begin
      Context.Last := 0;
      Exception_Information (
         X,
         Context'Address,
         Put => Put'Access,
         New_Line => New_Line'Access);
   end Put_Exception_Information;

   procedure Default_Report (X : Exception_Occurrence; Where : String) is
      Full_Name_All : String (1 .. 1); -- at least
      for Full_Name_All'Address use X.Id.Full_Name;
      By_Abort : constant Boolean :=
         Full_Name_All (1) = '_'; -- Standard'Abort_Signal
      Backtrace : constant Boolean :=
         X.Num_Tracebacks > 0
         and then Backtrace_Information'Address /= Null_Address;
      Context : aliased Information_Context_Type;
   begin
      Termination.Error_Put_Line ("");
      Context.Last := 0;
      if Where'Length > 0 then
         Put (Where, Context'Address);
      elsif By_Abort or else Backtrace then
         Put ("Execution", Context'Address);
      end if;
      if By_Abort then
         Put (" terminated by abort", Context'Address);
         if Where'Length = 0 then
            Put (" of environment task", Context'Address);
         end if;
         New_Line (Context'Address);
      else
         if Context.Last > 0 then
            Put (" terminated by unhandled exception", Context'Address);
            New_Line (Context'Address);
         end if;
         Put_Exception_Information (X);
      end if;
   end Default_Report;

   procedure Report (X : Exception_Occurrence; Where : String) is
   begin
      Report_Hook (X, Where);
   end Report;

end System.Unwind.Occurrences;
