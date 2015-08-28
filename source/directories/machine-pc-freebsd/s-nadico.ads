pragma License (Unrestricted);
--  implementation unit specialized for FreeBSD (or Linux)
package System.Native_Directories.Copying is
   pragma Preelaborate;

   procedure Copy_File (
      Source_Name : String;
      Target_Name : String;
      Overwrite : Boolean := True);

   procedure Replace_File (
      Source_Name : String;
      Target_Name : String);

end System.Native_Directories.Copying;
