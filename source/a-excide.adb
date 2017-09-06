package body Ada.Exception_Identification is
   pragma Suppress (All_Checks);

   function Exception_Name (Id : Exception_Id) return String is
   begin
      if Id = null then
         raise Constraint_Error;
      else
         declare
            Full_Name_All : String (1 .. Id.Name_Length - 1);
            for Full_Name_All'Address use Id.Full_Name;
         begin
            return Full_Name_All;
         end;
      end if;
   end Exception_Name;

end Ada.Exception_Identification;
