pragma License (Unrestricted);
--  with Ada.Streams;
with Ada.Exception_Identification;
private with System.Unwind;
package Ada.Exceptions is
   pragma Preelaborate;

--  type Exception_Id is private;
--  pragma Preelaborable_Initialization (Exception_Id);
   subtype Exception_Id is Exception_Identification.Exception_Id;
   function "=" (Left, Right : Exception_Id) return Boolean -- CB41003
      renames Exception_Identification."=";
--  Null_Id : constant Exception_Id;
   Null_Id : Exception_Id
      renames Exception_Identification.Null_Id;

   function Exception_Name (Id : Exception_Id) return String
      renames Exception_Identification.Exception_Name;
   function Wide_Exception_Name (Id : Exception_Id) return Wide_String;
   function Wide_Wide_Exception_Name (Id : Exception_Id)
      return Wide_Wide_String;

   type Exception_Occurrence is limited private;
   pragma Preelaborable_Initialization (Exception_Occurrence);
   type Exception_Occurrence_Access is access all Exception_Occurrence;
   Null_Occurrence : constant Exception_Occurrence;

   procedure Raise_Exception (E : Exception_Id; Message : String := "")
      renames Exception_Identification.Raise_Exception;
   function Exception_Message (X : Exception_Occurrence) return String;
   procedure Reraise_Occurrence (X : Exception_Occurrence);

   --  extended
   --  Same as Reraise_Occurrence without checking Null_Occurrence.
   procedure Unchecked_Reraise_Occurrence (X : Exception_Occurrence)
      with Import,
         Convention => Ada,
         External_Name => "ada__exceptions__reraise_occurrence_always";
   pragma No_Return (Unchecked_Reraise_Occurrence);

   function Exception_Identity (X : Exception_Occurrence)
      return Exception_Id;
   function Exception_Name (X : Exception_Occurrence) return String;
      --  Same as Exception_Name (Exception_Identity (X)).
   function Wide_Exception_Name (X : Exception_Occurrence)
      return Wide_String;
      --  Same as Wide_Exception_Name (Exception_Identity (X)).
   function Wide_Wide_Exception_Name (X : Exception_Occurrence)
      return Wide_Wide_String;
      --  Same as Wide_Wide_Exception_Name (Exception_Identity (X)).
   function Exception_Information (X : Exception_Occurrence) return String;

   procedure Save_Occurrence (
      Target : out Exception_Occurrence;
      Source : Exception_Occurrence)
      with Import,
         Convention => Ada,
         External_Name => "ada__exceptions__save_occurrence";
   function Save_Occurrence (
      Source : Exception_Occurrence)
      return Exception_Occurrence_Access;

   --  extended
   --  Effective alternatives of:
   --    begin raise E; when X : E => Save_Occurrence (Target, X); end;
   procedure Save_Exception (
      Target : out Exception_Occurrence;
      E : Exception_Id;
      Message : String := "")
      with Import,
         Convention => Ada, External_Name => "__drake_save_exception";
   procedure Save_Exception_From_Here (
      Target : out Exception_Occurrence;
      E : Exception_Id;
      File : String := Debug.File;
      Line : Integer := Debug.Line)
      with Import,
         Convention => Ada,
         External_Name => "__drake_save_exception_from_here";
   procedure Save_Exception_From_Here (
      Target : out Exception_Occurrence;
      E : Exception_Id;
      File : String := Debug.File;
      Line : Integer := Debug.Line;
      Message : String)
      with Import,
         Convention => Ada,
         External_Name => "__drake_save_exception_from_here_with";

--  procedure Read_Exception_Occurrence (
--    Stream : not null access Ada.Streams.Root_Stream_Type'Class;
--    Item : out Exception_Occurrence);
--  procedure Write_Exception_Occurrence (
--    Stream : not null access Ada.Streams.Root_Stream_Type'Class;
--    Item : Exception_Occurrence);

--  for Exception_Occurrence'Read use Read_Exception_Occurrence;
--  for Exception_Occurrence'Write use Write_Exception_Occurrence;

private

   type Exception_Occurrence is new System.Unwind.Exception_Occurrence;

   Null_Occurrence : constant Exception_Occurrence := (
      Id => null,
      Machine_Occurrence => System.Null_Address,
      Msg_Length => 0,
      Msg => (others => ' '),
      Exception_Raised => False,
      Pid => 0,
      Num_Tracebacks => 0,
      Tracebacks => (others => System.Null_Address));

   --  optionally required by compiler (a-except-2005.ads)
   --  for raising, Raise_Exception may be called if not existing (exp_ch6.adb)
   procedure Raise_Exception_Always (E : Exception_Id; Message : String := "")
      renames Raise_Exception;

   --  required by compiler (a-except-2005.ads)
   --  for reraising (exp_ch11.adb)
   procedure Reraise_Occurrence_Always (X : Exception_Occurrence)
      renames Unchecked_Reraise_Occurrence;

   --  required by compiler (a-except-2005.ads)
   --  for reraising from when all others (exp_ch11.adb)
   procedure Reraise_Occurrence_No_Defer (X : Exception_Occurrence)
      with Import,
         Convention => Ada,
         External_Name => "ada__exceptions__reraise_occurrence_no_defer";
   pragma No_Return (Reraise_Occurrence_No_Defer);

   --  optionally required by compiler (a-except-2005.ads)
   --  raise Program_Error if not existing (exp_ch7.adb)
   procedure Raise_From_Controlled_Operation (X : Exception_Occurrence)
      with Import,
         Convention => Ada,
         External_Name => "__gnat_raise_from_controlled_operation";

   --  required by compiler (a-except-2005.ads)
   --  for finalizer (exp_ch7.adb)
   function Triggered_By_Abort return Boolean
      with Import,
         Convention => Ada,
         External_Name => "ada__exceptions__triggered_by_abort";

   --  required by compiler (a-except-2005.ads)
   --  for intrinsic function Exception_Name (exp_intr.adb)
   function Exception_Name_Simple (X : Exception_Occurrence) return String
      renames Exception_Name;

   --  required by compiler (a-except-2005.ads)
   --  ??? (exp_ch11.adb, sem_ch11.adb)
   subtype Code_Loc is System.Address;

   --  not required for gcc (a-except-2005.ads)
--  procedure Poll;

end Ada.Exceptions;
