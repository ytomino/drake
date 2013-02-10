pragma License (Unrestricted);
--  implementation unit
package Ada.Calendar.Inside.Time_Zones is

   function UTC_Time_Offset (Date : Time := Clock) return Time_Offset;
   pragma Pure_Function (UTC_Time_Offset);

end Ada.Calendar.Inside.Time_Zones;
