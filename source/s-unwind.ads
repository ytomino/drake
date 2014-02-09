pragma License (Unrestricted);
--  runtime unit
package System.Unwind is
   pragma Pure;

   --  exception data type (s-stalib.ads)
   type Raise_Action is access procedure;
   type Exception_Code is mod 2 ** Integer'Size;
   for Exception_Code'Size use Integer'Size;
   type Exception_Data;
   type Exception_Data_Access is access constant Exception_Data;
   for Exception_Data_Access'Storage_Size use 0;
   type Exception_Data is record
      Not_Handled_By_Others : Boolean;
      Lang : Character;
      Name_Length : Natural;
      Full_Name : Address;
      HTable_Ptr : Exception_Data_Access;
      Import_Code : Exception_Code;
      Raise_Hook : Raise_Action;
   end record;
   pragma Suppress_Initialization (Exception_Data);

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
      Id : Exception_Data_Access;
      Msg_Length : Natural := 0;
      Msg : String (1 .. Exception_Msg_Max_Length);
      Exception_Raised : Boolean := False;
      Pid : Natural := 0;
      Num_Tracebacks : Natural range 0 .. Max_Tracebacks := 0;
      Tracebacks : Tracebacks_Array;
   end record;

   type Exception_Occurrence_Access is access all Exception_Occurrence;
   for Exception_Occurrence_Access'Storage_Size use 0;

   --  implementation for catching object (a-except-2005.adb)
   procedure Save_Occurrence (
      Target : out Exception_Occurrence;
      Source : Exception_Occurrence);
   pragma Export (Ada, Save_Occurrence,
      "ada__exceptions__save_occurrence");

   --  equivalent to Append_Info_Exception_Information (a-exexda.adb)
   generic
      with procedure Put (S : String);
      with procedure New_Line;
   procedure Exception_Information (X : Exception_Occurrence);

end System.Unwind;
