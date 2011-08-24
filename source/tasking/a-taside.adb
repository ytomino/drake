with System.Address_Image;
package body Ada.Task_Identification is
   pragma Suppress (All_Checks);

   function Image (T : Task_Id) return String is
   begin
      if T = null then
         return "";
      else
         declare
            N : String renames Name (T);
            A : String renames System.Address_Image (T.all'Address);
         begin
            if N = "" then
               return A;
            else
               return N & ":" & A;
            end if;
         end;
      end if;
   end Image;

end Ada.Task_Identification;
