pragma Check_Policy (Trace => Ignore);
with Ada;
with System.Formatting.Address;
with System.Storage_Map;
with System.Termination;
with System.Unwind.Occurrences;
with System.Unwind.Raising;
package body System.Unwind.Backtrace is
   pragma Suppress (All_Checks);

   package Separated is

      --  equivalent to __gnat_backtrace (tracebak.c/tb-gcc.c)
      procedure Backtrace (
         Item : aliased out Tracebacks_Array;
         Last : out Natural;
         Exclude_Min : Address;
         Exclude_Max : Address;
         Skip_Frames : Natural);

   end Separated;

   package body Separated is separate;

   --  for Report_Backtrace

   type Information_Context_Type is record
      Item : String (
         1 ..
         256
            + Unwind.Exception_Msg_Max_Length
            + Unwind.Max_Tracebacks
               * (3 + (Standard'Address_Size + 3) / 4));
      Last : Natural;
   end record;
   pragma Suppress_Initialization (Information_Context_Type);

   procedure Put (S : String; Params : Address);
   procedure Put (S : String; Params : Address) is
      Context : Information_Context_Type;
      for Context'Address use Params;
      First : constant Positive := Context.Last + 1;
   begin
      Context.Last := Context.Last + S'Length;
      Context.Item (First .. Context.Last) := S;
   end Put;

   procedure New_Line (Params : Address);
   procedure New_Line (Params : Address) is
      Context : Information_Context_Type;
      for Context'Address use Params;
   begin
      Termination.Error_Put_Line (Context.Item (1 .. Context.Last));
      Context.Last := 0;
   end New_Line;

   --  implementation

   procedure Call_Chain (Current : in out Exception_Occurrence) is
      function Report return Boolean;
      function Report return Boolean is
      begin
         Report_Backtrace (Current);
         return True;
      end Report;
   begin
      if Exception_Tracebacks /= 0 and then Current.Num_Tracebacks = 0 then
         Separated.Backtrace (
            Current.Tracebacks,
            Current.Num_Tracebacks, -- Tracebacks_Array'First = 1
            Raising.AAA,
            Raising.ZZZ,
            3); -- Occurrences.Backtrace, Call_Chain and Separated.Backtrace
         pragma Check (Trace, Ada.Debug.Put ("Call_Chain"));
         pragma Check (Trace, Report);
      end if;
   end Call_Chain;

   procedure Backtrace_Information (
      X : Exception_Occurrence;
      Params : Address;
      Put : not null access procedure (S : String; Params : Address);
      New_Line : not null access procedure (Params : Address))
   is
      S : Formatting.Address.Address_String;
   begin
      Put ("Load address: 0x", Params);
      declare
         Item : constant Address := Storage_Map.Load_Address;
      begin
         Formatting.Address.Image (
            Item,
            S,
            Set => Formatting.Lower_Case);
         Put (S, Params);
      end;
      New_Line (Params);
      Put ("Call stack traceback locations:", Params);
      New_Line (Params);
      for I in 1 .. X.Num_Tracebacks loop
         Put ("0x", Params);
         declare
            Item : constant Address := X.Tracebacks (I);
         begin
            Formatting.Address.Image (
               Item,
               S,
               Set => Formatting.Lower_Case);
            Put (S, Params);
         end;
         if I < X.Num_Tracebacks then
            Put (" ", Params);
         end if;
      end loop;
      New_Line (Params);
   end Backtrace_Information;

   procedure Report_Backtrace (X : Exception_Occurrence) is
      Context : Information_Context_Type;
   begin
      Context.Last := 0;
      Occurrences.Exception_Information (
         X,
         Context'Address,
         Put => Put'Access,
         New_Line => New_Line'Access);
   end Report_Backtrace;

end System.Unwind.Backtrace;
