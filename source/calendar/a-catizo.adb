with Ada.Calendar.Time_Zones.Inside;
package body Ada.Calendar.Time_Zones is
   pragma Suppress (All_Checks);

   function UTC_Time_Offset (Date : Time := Clock) return Time_Offset is
   begin
      return Inside.UTC_Time_Offset (Date);
   end UTC_Time_Offset;

end Ada.Calendar.Time_Zones;
