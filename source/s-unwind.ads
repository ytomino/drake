pragma License (Unrestricted);
--  runtime unit
with Ada.Unchecked_Deallocation;
with System.Standard_Library;
package System.Unwind is
   pragma Preelaborate;

   --  RM 11.4.1(18) (s-parame.ads)
   Default_Exception_Msg_Max_Length : constant := 200;

   --  (s-traent.ads)
   subtype Traceback_Entry is Address;

   --  (a-except-2005.ads)
   Exception_Msg_Max_Length : constant := Default_Exception_Msg_Max_Length;
   Max_Tracebacks : constant := 50;
   type Tracebacks_Array is array (1 .. Max_Tracebacks) of Traceback_Entry;
   pragma Suppress_Initialization (Tracebacks_Array);

   --  (a-except-2005.ads)
   type Exception_Occurrence is record
      Id : Standard_Library.Exception_Data_Ptr;
      Msg_Length : Natural := 0;
      Msg : String (1 .. Exception_Msg_Max_Length);
      Cleanup_Flag : Boolean := False;
      Exception_Raised : Boolean := False;
      Pid : Natural := 0;
      Num_Tracebacks : Natural range 0 .. Max_Tracebacks := 0;
      Tracebacks : Tracebacks_Array;
      Private_Data : Address := Null_Address;
   end record;

   type Exception_Occurrence_Access is access all Exception_Occurrence;
   procedure Free is new Ada.Unchecked_Deallocation (
      Exception_Occurrence,
      Exception_Occurrence_Access);

   --  implementation for catching object (a-except-2005.adb)
   procedure Save_Occurrence_No_Private (
      Target : out Exception_Occurrence;
      Source : Exception_Occurrence);
   pragma Export (Ada, Save_Occurrence_No_Private,
      "ada__exceptions__save_occurrence");

   --  equivalent to Append_Info_Exception_Information (a-exexda.adb)
   generic
      with procedure Put (S : String);
      with procedure New_Line;
   procedure Exception_Information (X : Exception_Occurrence);

end System.Unwind;
