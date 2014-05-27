with Ada.Unchecked_Conversion;
with System.UTF_Conversions.From_8_To_16;
with System.UTF_Conversions.From_8_To_32;
package body Ada.Exceptions is
   pragma Suppress (All_Checks);
   use type System.Unwind.Exception_Data_Access;

   function Exception_Identity (X : Exception_Occurrence)
      return Exception_Id
   is
      function To_Exception_Id is
         new Unchecked_Conversion (
            System.Unwind.Exception_Data_Access,
            Exception_Id);
   begin
      return Exception_Id (To_Exception_Id (X.Id));
   end Exception_Identity;

   function Exception_Information (X : Exception_Occurrence) return String is
   begin
      if X.Id = null then
         raise Constraint_Error;
      else
         declare
            Max_Length : constant := 256
               + System.Unwind.Exception_Msg_Max_Length
               + System.Unwind.Max_Tracebacks
                  * (3 + (Standard'Address_Size + 3) / 4);
            type Result_Type is record
               Item : String (1 .. Max_Length);
               Last : Natural;
            end record;
            pragma Suppress_Initialization (Result_Type);
            procedure Put (S : String; Params : System.Address);
            procedure Put (S : String; Params : System.Address) is
               Result : Result_Type;
               for Result'Address use Params;
               First : constant Positive := Result.Last + 1;
            begin
               Result.Last := Result.Last + S'Length;
               Result.Item (First .. Result.Last) := S;
            end Put;
            procedure New_Line (Params : System.Address);
            procedure New_Line (Params : System.Address) is
               Result : Result_Type;
               for Result'Address use Params;
            begin
               Result.Last := Result.Last + 1;
               Result.Item (Result.Last) := Character'Val (10);
            end New_Line;
            Result : aliased Result_Type;
         begin
            Result.Last := 0;
            System.Unwind.Exception_Information (
               System.Unwind.Exception_Occurrence (X),
               Result'Address,
               Put => Put'Access,
               New_Line => New_Line'Access);
            return Result.Item (1 .. Result.Last);
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

   function Exception_Name (X : Exception_Occurrence) return String is
   begin
      return Exception_Name (Exception_Identity (X));
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
      return Wide_Exception_Name (Exception_Identity (X));
   end Wide_Exception_Name;

   function Wide_Wide_Exception_Name (Id : Exception_Id)
      return Wide_Wide_String is
   begin
      return System.UTF_Conversions.From_8_To_32.Convert (Exception_Name (Id));
   end Wide_Wide_Exception_Name;

   function Wide_Wide_Exception_Name (X : Exception_Occurrence)
      return Wide_Wide_String is
   begin
      return Wide_Wide_Exception_Name (Exception_Identity (X));
   end Wide_Wide_Exception_Name;

end Ada.Exceptions;
