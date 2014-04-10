package body Ada.Exception_Identification is
   pragma Suppress (All_Checks);

   function Exception_Name (Id : Exception_Id) return String is
   begin
      if Id = null then
         raise Constraint_Error;
      else
         declare
            subtype Fixed_String is String (Positive);
            Full_Name : Fixed_String;
            for Full_Name'Address use Id.Full_Name;
         begin
            return Full_Name (1 .. Id.Name_Length - 1);
         end;
      end if;
   end Exception_Name;

end Ada.Exception_Identification;
