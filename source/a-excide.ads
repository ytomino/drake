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

   --  These functions raise a new occurrence of the identified exception
   --    with source location.
   procedure Raise_Exception_From_Here (
      E : Exception_Id;
      File : String := Debug.File;
      Line : Integer := Debug.Line)
      with No_Return, Import, Convention => Ada,
         External_Name => "__drake_raise_exception_from_here";
   procedure Raise_Exception_From_Here (
      E : Exception_Id;
      File : String := Debug.File;
      Line : Integer := Debug.Line;
      Message : String)
      with No_Return, Import, Convention => Ada,
         External_Name => "__drake_raise_exception_from_here_with";

private

   type Exception_Id is new System.Unwind.Exception_Data_Access;

   Null_Id : constant Exception_Id := null;

end Ada.Exception_Identification;
