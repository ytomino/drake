with C.time;
package body Ada.Calendar.Inside.Time_Zones is
   pragma Suppress (All_Checks);
   use type C.signed_long;

   Time_Offset_Value : Time_Offset;

   function UTC_Time_Offset (Date : Time := Clock) return Time_Offset is
      pragma Unreferenced (Date);
   begin
      return Time_Offset_Value;
   end UTC_Time_Offset;

begin
   C.time.tzset;
   Time_Offset_Value := Time_Offset (C.time.timezone / (-60));
end Ada.Calendar.Inside.Time_Zones;
