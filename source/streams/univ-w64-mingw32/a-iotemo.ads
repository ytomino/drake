pragma License (Unrestricted);
--  extended unit
package Ada.IO_Text_Modes is
   --  This package provides the types for the parameters Form of Text_IO.
   pragma Pure;

   --  the types for the parameters Form of Text_IO

   type File_External is (
      Terminal,
      Locale, -- "external=dbcs" or default
      UTF_8); -- "external=utf-8" or "wcem=8"
   type File_External_Encoding is new File_External range Locale .. UTF_8;

   type File_New_Line is (
      LF, -- "lm=lf"
      CR, -- "lm=cr"
      CR_LF); -- "lm=m"
   function By_Target return File_New_Line
      renames CR_LF;

   type File_SUB is (
      Ordinary, -- "sub=none" or default
      End_Of_File); -- "sub=eof"

end Ada.IO_Text_Modes;
