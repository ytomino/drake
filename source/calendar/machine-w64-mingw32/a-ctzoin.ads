pragma License (Unrestricted);
--  implementation unit specialized for Windows
package Ada.Calendar.Time_Zones.Inside is

   function UTC_Time_Offset (Date : Time := Clock) return Time_Offset;
   pragma Pure_Function (UTC_Time_Offset);

end Ada.Calendar.Time_Zones.Inside;
