with System.UTF_Conversions.From_8_To_16;
with System.UTF_Conversions.From_8_To_32;
package body Ada.Exceptions is
   pragma Suppress (All_Checks);
   use type System.Standard_Library.Exception_Data_Ptr;

   function Exception_Identity (X : Exception_Occurrence)
      return Exception_Id is
   begin
      return Exceptions.Exception_Id (X.Id);
   end Exception_Identity;

   function Exception_Information (X : Exception_Occurrence) return String is
   begin
      if X.Id = null then
         raise Constraint_Error;
      else
         declare
            Max_Length : constant := 256 +
               System.Unwind.Exception_Msg_Max_Length +
               System.Unwind.Max_Tracebacks * (3 + Standard'Address_Size / 4);
            Result : String (1 .. Max_Length);
            Last : Natural := 0;
            procedure Put (S : String);
            procedure Put (S : String) is
               First : constant Positive := Last + 1;
            begin
               Last := Last + S'Length;
               Result (First .. Last) := S;
            end Put;
            procedure New_Line;
            procedure New_Line is
            begin
               Last := Last + 1;
               Result (Last) := Character'Val (10);
            end New_Line;
            procedure Fill is new System.Unwind.Exception_Information (
               Put,
               New_Line);
         begin
            Fill (System.Unwind.Exception_Occurrence (X));
            return Result (1 .. Last);
         end;
      end if;
   end Exception_Information;

   function Exception_Message (X : Exception_Occurrence) return String is
   begin
      if X.Id = null then
         raise Constraint_Error;
      else
         return X.Msg (1 .. X.Msg_Length);
      end if;
   end Exception_Message;

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
            return Full_Name (1 .. Id.Name_Length);
         end;
      end if;
   end Exception_Name;

   function Exception_Name (X : Exception_Occurrence) return String is
   begin
      return Exception_Name (Exception_Id (X.Id));
   end Exception_Name;

   procedure Reraise_Occurrence (X : Exception_Occurrence) is
   begin
      if X.Id /= null then
         Reraise_Occurrence_Always (X);
      end if;
   end Reraise_Occurrence;

   function Save_Occurrence (
      Source : Exception_Occurrence)
      return Exception_Occurrence_Access is
   begin
      return Result : constant Exception_Occurrence_Access :=
         new Exception_Occurrence
      do
         Save_Occurrence (Result.all, Source);
      end return;
   end Save_Occurrence;

   function Wide_Exception_Name (Id : Exception_Id) return Wide_String is
   begin
      return System.UTF_Conversions.From_8_To_16.Convert (Exception_Name (Id));
   end Wide_Exception_Name;

   function Wide_Exception_Name (X : Exception_Occurrence)
      return Wide_String is
   begin
      return Wide_Exception_Name (Exception_Id (X.Id));
   end Wide_Exception_Name;

   function Wide_Wide_Exception_Name (Id : Exception_Id)
      return Wide_Wide_String is
   begin
      return System.UTF_Conversions.From_8_To_32.Convert (Exception_Name (Id));
   end Wide_Wide_Exception_Name;

   function Wide_Wide_Exception_Name (X : Exception_Occurrence)
      return Wide_Wide_String is
   begin
      return Wide_Wide_Exception_Name (Exception_Id (X.Id));
   end Wide_Wide_Exception_Name;

end Ada.Exceptions;
