pragma License (Unrestricted);
--  extended unit
package Ada.Streams.Stream_IO.Standard_Files is
   --  There are Stream_IO version file objects of
   --    Standard_Input, Standard_Output, and Standard_Error.

   function Standard_Input return not null access constant File_Type;
   function Standard_Output return not null access constant File_Type;
   function Standard_Error return not null access constant File_Type;

   pragma Pure_Function (Standard_Input);
   pragma Pure_Function (Standard_Output);
   pragma Pure_Function (Standard_Error);
   pragma Inline (Standard_Input);
   pragma Inline (Standard_Output);
   pragma Inline (Standard_Error);

end Ada.Streams.Stream_IO.Standard_Files;
