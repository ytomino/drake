pragma License (Unrestricted);
--  extended unit
private with System.Unwind;
package Ada.Exception_Identification is
   --  This package is the "Pure" version of Ada.Exceptions.
   pragma Pure;

   type Exception_Id is private;
   pragma Preelaborable_Initialization (Exception_Id);
   Null_Id : constant Exception_Id;

   function Exception_Name (Id : Exception_Id) return String;

   procedure Raise_Exception (E : Exception_Id; Message : String := "");
   pragma No_Return (Raise_Exception);
   pragma Import (Ada, Raise_Exception, "ada__exceptions__raise_exception");

   --  [gcc-4.7] compiler lose sight of System if this has a nested package

--  package Implementation is
--
--    procedure Raise_Exception_From_Here (
--       E : Exception_Id;
--       File : String := Debug.File;
--       Line : Integer := Debug.Line);
--    pragma No_Return (Raise_Exception_From_Here);
--    pragma Import (Ada, Raise_Exception_From_Here,
--       "__drake_raise_exception_from_here");
--
--    procedure Raise_Exception_From_Here_With (
--       E : Exception_Id;
--       File : String := Debug.File;
--       Line : Integer := Debug.Line;
--       Message : String);
--    pragma No_Return (Raise_Exception_From_Here_With);
--    pragma Import (Ada, Raise_Exception_From_Here_With,
--       "__drake_raise_exception_from_here_with");
--
--  end Implementation;

   procedure Implementation_Raise_Exception_From_Here (
      E : Exception_Id;
      File : String := Debug.File;
      Line : Integer := Debug.Line);
   pragma No_Return (Implementation_Raise_Exception_From_Here);
   pragma Import (Ada, Implementation_Raise_Exception_From_Here,
      "__drake_raise_exception_from_here");

   procedure Implementation_Raise_Exception_From_Here_With (
      E : Exception_Id;
      File : String := Debug.File;
      Line : Integer := Debug.Line;
      Message : String);
   pragma No_Return (Implementation_Raise_Exception_From_Here_With);
   pragma Import (Ada, Implementation_Raise_Exception_From_Here_With,
      "__drake_raise_exception_from_here_with");

   --  These functions raise a new occurrence of the identified exception
   --    with source location.
   procedure Raise_Exception_From_Here (
      E : Exception_Id;
      File : String := Debug.File;
      Line : Integer := Debug.Line)
      renames Implementation_Raise_Exception_From_Here;
   procedure Raise_Exception_From_Here (
      E : Exception_Id;
      File : String := Debug.File;
      Line : Integer := Debug.Line;
      Message : String)
      renames Implementation_Raise_Exception_From_Here_With;

private

   type Exception_Id is new System.Unwind.Exception_Data_Access;

   Null_Id : constant Exception_Id := null;

end Ada.Exception_Identification;
