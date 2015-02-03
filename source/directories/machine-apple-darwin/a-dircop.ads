pragma License (Unrestricted);
--  implementation unit specialized for Darwin
private package Ada.Directories.Copying is

   procedure Copy_File (
      Source_Name : String;
      Target_Name : String;
      Overwrite : Boolean := True);

end Ada.Directories.Copying;
