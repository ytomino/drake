pragma License (Unrestricted);
--  extended package
package Ada.Streams.Stream_IO.Standards is

   function Standard_Input return not null access constant File_Type;
   function Standard_Output return not null access constant File_Type;
   function Standard_Error return not null access constant File_Type;

end Ada.Streams.Stream_IO.Standards;
