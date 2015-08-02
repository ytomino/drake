with System.Native_Calendar.Time_Zones;
package body Ada.Calendar.Time_Zones is

   function UTC_Time_Offset (Date : Time := Clock) return Time_Offset is
   begin
      return Time_Offset (
         System.Native_Calendar.Time_Zones.UTC_Time_Offset (Duration (Date)));
   end UTC_Time_Offset;

end Ada.Calendar.Time_Zones;
