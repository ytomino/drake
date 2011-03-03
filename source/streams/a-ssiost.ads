pragma License (Unrestricted);
--  extended package
package Ada.Streams.Stream_IO.Standards is

   function Standard_Input return not null access constant File_Type;
   pragma Inline (Standard_Input);
   function Standard_Output return not null access constant File_Type;
   pragma Inline (Standard_Output);
   function Standard_Error return not null access constant File_Type;
   pragma Inline (Standard_Error);

end Ada.Streams.Stream_IO.Standards;
