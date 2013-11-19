pragma License (Unrestricted);
--  extended unit
package Ada.IO_Text_Modes is
   --  This package provides the types for the parameters Form of Text_IO.
   pragma Pure;

   --  the types for the parameters Form of Text_IO

   type File_External is (
      Terminal,
      UTF_8); -- default
   type File_External_Encoding is new File_External range UTF_8 .. UTF_8;
   function Locale return File_External_Encoding
      renames UTF_8;

   type File_New_Line is (
      LF, -- "lm=lf"
      CR, -- "lm=cr"
      CR_LF); -- "lm=m"
   function By_Target return File_New_Line
      renames LF;

   type File_SUB is (
      Ordinary, -- "sub=none" or default
      End_Of_File); -- "sub=eof"

end Ada.IO_Text_Modes;
