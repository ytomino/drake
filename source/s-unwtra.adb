with System.Termination;
with System.Unwind.Raising;
package body System.Unwind.Traceback is
   pragma Suppress (All_Checks);

   package Separated is

      --  equivalent to __gnat_backtrace (tracebak.c/tb-gcc.c)
      procedure Get_Traceback (
         Traceback : out Tracebacks_Array;
         Length : out Natural;
         Exclude_Min : Address;
         Exclude_Max : Address;
         Skip_Frames : Natural);

   end Separated;

   package body Separated is separate;

   procedure Put (S : String; Params : Address);
   procedure Put (S : String; Params : Address) is
      pragma Unreferenced (Params);
   begin
      Termination.Error_Put (S);
   end Put;

   procedure New_Line (Params : Address);
   procedure New_Line (Params : Address) is
      pragma Unreferenced (Params);
   begin
      Termination.Error_New_Line;
   end New_Line;

   --  implementation

   procedure Call_Chain (Current : not null Exception_Occurrence_Access) is
   begin
      if Exception_Tracebacks /= 0 and Current.Num_Tracebacks = 0 then
         Separated.Get_Traceback (
            Current.Tracebacks,
            Current.Num_Tracebacks,
            Raising.AAA,
            Raising.ZZZ,
            3); -- Propagate_Exception, Call_Chain, Get_Traceback
      end if;
   end Call_Chain;

   procedure Report_Traceback (Current : Exception_Occurrence) is
   begin
      Exception_Information (
         Current,
         Null_Address,
         Put => Put'Access,
         New_Line => New_Line'Access);
   end Report_Traceback;

end System.Unwind.Traceback;
