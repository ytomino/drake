with System.Address_To_Named_Access_Conversions;
with System.Formatting.Address;
package body Ada.Task_Identification is

   function Image (T : Task_Id) return String is
   begin
      if T = null then
         return "";
      else
         declare
            package Conv is
               new System.Address_To_Named_Access_Conversions (
                  System.Tasks.Task_Record,
                  Task_Id);
            N : constant not null access constant String := Name (T);
            Result : String (
               1 ..
               N'Length + 1 + System.Formatting.Address.Address_String'Length);
            Last : Natural := 0;
         begin
            if N'Length /= 0 then
               Last := N'Length;
               Result (1 .. Last) := N.all;
               Last := Last + 1;
               Result (Last) := ':';
            end if;
            System.Formatting.Address.Image (
               Conv.To_Address (T),
               Result (
                  Last + 1 ..
                  Last + System.Formatting.Address.Address_String'Length),
               Set => System.Formatting.Upper_Case);
            Last := Last + System.Formatting.Address.Address_String'Length;
            return Result (1 .. Last);
         end;
      end if;
   end Image;

end Ada.Task_Identification;
