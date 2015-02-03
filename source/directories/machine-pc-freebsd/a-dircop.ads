pragma License (Unrestricted);
--  implementation unit specialized for FreeBSD (or Linux)
private package Ada.Directories.Copying is

   procedure Copy_File (
      Source_Name : String;
      Target_Name : String;
      Overwrite : Boolean := True);

   procedure Replace_File (
      Source_Name : String;
      Target_Name : String);

end Ada.Directories.Copying;
