with C.time;
package body System.Native_Calendar.Time_Zones is
   use type C.signed_long;

   Time_Offset_Value : Time_Offset;

   --  implementation

   function UTC_Time_Offset (Date : Time) return Time_Offset is
      pragma Unreferenced (Date);
   begin
      return Time_Offset_Value;
   end UTC_Time_Offset;

begin
   C.time.tzset;
   Time_Offset_Value := Time_Offset (C.time.timezone / (-60));
end System.Native_Calendar.Time_Zones;
