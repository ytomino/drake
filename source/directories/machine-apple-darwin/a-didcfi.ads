pragma License (Unrestricted);
--  implementation unit specialized for Darwin
package Ada.Directories.Inside.Do_Copy_File is

   procedure Copy_File (
      Source_Name : String;
      Target_Name : String;
      Overwrite : Boolean := True);

end Ada.Directories.Inside.Do_Copy_File;
