with Ada.Exception_Identification.From_Here;
with System.Native_Calendar;
with C.sys.types;
with C.time;
package body System.Native_Calendar.Time_Zones is
   use Ada.Exception_Identification.From_Here;
   use type C.sys.types.time_t;

   function UTC_Time_Offset (Date : Time) return Time_Offset is
      --  FreeBSD does not have timezone variable
      GMT_Time : aliased constant Native_Time :=
         To_Native_Time (Duration (Date));
      Local_TM_Buf : aliased C.time.struct_tm;
      Local_TM : C.time.struct_tm_ptr;
      Local_Time : aliased C.sys.types.time_t;
   begin
      Local_TM := C.time.localtime_r (
         GMT_Time.tv_sec'Access,
         Local_TM_Buf'Access);
      Local_Time := C.time.timegm (Local_TM);
      if Local_Time = -1 then -- to pass negative UNIX time (?)
         Raise_Exception (Time_Error'Identity);
      end if;
      return Time_Offset ((Local_Time - GMT_Time.tv_sec) / 60);
   end UTC_Time_Offset;

begin
   C.time.tzset;
end System.Native_Calendar.Time_Zones;
