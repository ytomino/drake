pragma License (Unrestricted);
--  implementation unit specialized for Windows
package System.Native_Calendar.Time_Zones is

   function UTC_Time_Offset (Date : Time) return Time_Offset;
   pragma Pure_Function (UTC_Time_Offset);

end System.Native_Calendar.Time_Zones;
