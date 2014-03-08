pragma License (Unrestricted);
--  implementation unit specialized for Windows
package Ada.Calendar.Inside.Time_Zones is

   function UTC_Time_Offset (Date : Time := Clock) return Time_Offset;
   pragma Pure_Function (UTC_Time_Offset);

end Ada.Calendar.Inside.Time_Zones;
