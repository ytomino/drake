with C.winbase;
with C.windef;
package body System.Native_Calendar.Time_Zones is
   use type C.windef.DWORD;

   Time_Offset_Value : Time_Offset := 0;

   procedure Initialize;
   procedure Initialize is
      Info : aliased C.winbase.TIME_ZONE_INFORMATION;
   begin
      if C.winbase.GetTimeZoneInformation (Info'Access) /=
         C.winbase.TIME_ZONE_ID_INVALID
      then
         Time_Offset_Value := -Time_Offset (Info.Bias); -- reverse sign
      end if;
   end Initialize;

   --  implementation

   function UTC_Time_Offset (Date : Time) return Time_Offset is
      pragma Unreferenced (Date);
   begin
      return Time_Offset_Value;
   end UTC_Time_Offset;

begin
   Initialize;
end System.Native_Calendar.Time_Zones;
