pragma License (Unrestricted);
--  implementation unit
package Ada.Directories.Inside is

   procedure Copy_File (
      Source_Name,
      Target_Name : String;
      Form : String := "";
      Overwrite : Boolean := True);

end Ada.Directories.Inside;
