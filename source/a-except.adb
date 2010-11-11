with System.Unwind.Raising;
with System.UTF_Conversions;
package body Ada.Exceptions is
   pragma Suppress (All_Checks);
   use type System.Standard_Library.Exception_Data_Ptr;

   function Exception_Identity (X : Exception_Occurrence)
      return Exception_Id is
   begin
      return Exceptions.Exception_Id (X.Id);
   end Exception_Identity;

   function Exception_Information (X : Exception_Occurrence) return String is
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
      procedure Fill is
         new System.Unwind.Exception_Information (Put, New_Line);
   begin
      Fill (System.Unwind.Exception_Occurrence (X));
      return Result (1 .. Last);
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

   procedure Raise_Exception (E : Exception_Id; Message : String := "") is
      Actual_E : Exception_Id := E;
   begin
      if Actual_E = null then
         Actual_E := Constraint_Error'Identity;
      end if;
      System.Unwind.Raising.Raise_Exception (
         System.Standard_Library.Exception_Data_Ptr (Actual_E),
         Message => Message);
   end Raise_Exception;

   procedure Reraise_Occurrence (X : Exception_Occurrence) is
   begin
      if X.Id /= null then
         Reraise_Occurrence_Always (X);
      end if;
   end Reraise_Occurrence;

   procedure Reraise_Occurrence_Always (X : Exception_Occurrence) is
   begin
      System.Unwind.Raising.Reraise (System.Unwind.Exception_Occurrence (X));
   end Reraise_Occurrence_Always;

   procedure Save_Occurrence (
      Target : out Exception_Occurrence;
      Source : Exception_Occurrence) is
   begin
      System.Unwind.Save_Occurrence_No_Private (
         System.Unwind.Exception_Occurrence (Target),
         System.Unwind.Exception_Occurrence (Source));
   end Save_Occurrence;

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
      S : constant String := Exception_Name (Id);
      W : Wide_String (1 .. S'Length);
      L : Natural;
      Error : Boolean; --  ignored
   begin
      System.UTF_Conversions.UTF_8_To_UTF_16 (S, W, L, Error);
      return W (1 .. L);
   end Wide_Exception_Name;

   function Wide_Exception_Name (X : Exception_Occurrence)
      return Wide_String is
   begin
      return Wide_Exception_Name (Exception_Id (X.Id));
   end Wide_Exception_Name;

   function Wide_Wide_Exception_Name (Id : Exception_Id)
      return Wide_Wide_String
   is
      S : constant String := Exception_Name (Id);
      W : Wide_Wide_String (1 .. S'Length);
      L : Natural;
      Error : Boolean; --  ignored
   begin
      System.UTF_Conversions.UTF_8_To_UTF_32 (S, W, L, Error);
      return W (1 .. L);
   end Wide_Wide_Exception_Name;

   function Wide_Wide_Exception_Name (X : Exception_Occurrence)
      return Wide_Wide_String is
   begin
      return Wide_Wide_Exception_Name (Exception_Id (X.Id));
   end Wide_Wide_Exception_Name;

end Ada.Exceptions;
