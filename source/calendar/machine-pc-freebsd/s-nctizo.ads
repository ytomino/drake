pragma License (Unrestricted);
--  implementation unit specialized for FreeBSD
with Ada.Calendar;
package System.Native_Calendar.Time_Zones is

   function UTC_Time_Offset (Date : Time) return Time_Offset;
   pragma Pure_Function (UTC_Time_Offset);

   Time_Error : exception
      renames Ada.Calendar.Time_Error;

end System.Native_Calendar.Time_Zones;
