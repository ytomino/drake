pragma License (Unrestricted);
package Ada.Calendar.Time_Zones is

   --  Time zone manipulation:

   type Time_Offset is range -28 * 60 .. 28 * 60;

   Unknown_Zone_Error : exception;

   function UTC_Time_Offset (Date : Time := Clock) return Time_Offset;

end Ada.Calendar.Time_Zones;
