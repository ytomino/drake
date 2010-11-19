with Ada.Calendar.Inside;
with C.time;
with C.sys.types;
package body Ada.Calendar.Time_Zones is
   pragma Suppress (All_Checks);
   use type C.sys.types.time_t;

   function UTC_Time_Offset (Date : Time := Clock) return Time_Offset is
      GMT_Time : aliased C.sys.types.time_t;
      Sub_Second : Inside.Second_Duration;
      Local_TM : aliased C.time.struct_tm;
      Local_Time : aliased C.sys.types.time_t;
      Dummy : C.time.struct_tm_ptr;
      pragma Unreferenced (Dummy);
   begin
      Inside.Split (Date, GMT_Time, Sub_Second);
      Dummy := C.time.localtime_r (GMT_Time'Access, Local_TM'Access);
      Local_Time := C.time.timegm (Local_TM'Access);
      return Time_Offset ((Local_Time - GMT_Time) / 60);
   end UTC_Time_Offset;

end Ada.Calendar.Time_Zones;
