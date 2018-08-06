with Ada.Exception_Identification.From_Here;
with System.Native_Calendar;
package body Ada.Calendar.Time_Zones is
   use Exception_Identification.From_Here;

   function UTC_Time_Offset (Date : Time := Clock) return Time_Offset is
      Result : Time_Offset;
      Error : Boolean;
   begin
      System.Native_Calendar.UTC_Time_Offset (
         Duration (Date),
         Time_Zone => System.Native_Calendar.Time_Offset (Result),
         Error => Error);
      if Error then
         Raise_Exception (Time_Error'Identity);
      end if;
      return Result;
   end UTC_Time_Offset;

begin
   System.Native_Calendar.Initialize_Time_Zones;
end Ada.Calendar.Time_Zones;
