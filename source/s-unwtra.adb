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

   procedure Put_Exception_Information is
      new Exception_Information (
         Termination.Error_Put,
         Termination.Error_New_Line);

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

   procedure Report_Traceback (Current : Exception_Occurrence)
      renames Put_Exception_Information;

end System.Unwind.Traceback;
