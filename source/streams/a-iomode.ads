pragma License (Unrestricted);
--  extended unit
package Ada.IO_Modes is
   --  This package provides the root type of File_Mode.
   pragma Pure;

   type File_Mode is (In_File, Out_File, Append_File);

end Ada.IO_Modes;
