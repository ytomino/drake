with System.Address_Image;
package body Ada.Task_Identification is
   pragma Suppress (All_Checks);

   function Image (T : Task_Id) return String is
   begin
      if T = null then
         return "";
      else
         --  address of Task_Record
         return System.Address_Image (T.all'Address);
      end if;
   end Image;

end Ada.Task_Identification;
