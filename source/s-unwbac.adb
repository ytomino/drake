pragma Check_Policy (Trace => Ignore);
with Ada; -- assertions
with System.Formatting.Address;
with System.Storage_Map;
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
         Exclude_Max : Address);

   end Separated;

   package body Separated is separate;

   --  implementation

   procedure Call_Chain (Current : in out Exception_Occurrence) is
      function Report return Boolean;
      function Report return Boolean is
      begin
         Occurrences.Put_Exception_Information (Current);
         return True;
      end Report;
   begin
      if Exception_Tracebacks /= 0 and then Current.Num_Tracebacks = 0 then
         Separated.Backtrace (
            Current.Tracebacks,
            Current.Num_Tracebacks, -- Tracebacks_Array'First = 1
            Raising.AAA,
            Raising.ZZZ);
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

end System.Unwind.Backtrace;
