pragma License (Unrestricted);
--  extended unit
package Ada.Exception_Identification.From_Here is
   --  For shorthand.
   pragma Pure;

   procedure Raise_Exception (
      E : Exception_Id;
      File : String := Debug.File;
      Line : Integer := Debug.Line)
      renames Raise_Exception_From_Here;
   procedure Raise_Exception (
      E : Exception_Id;
      File : String := Debug.File;
      Line : Integer := Debug.Line;
      Message : String)
      renames Raise_Exception_From_Here;

end Ada.Exception_Identification.From_Here;
