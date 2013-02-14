with Ada.Streams.Stream_IO.Inside.Standard_Files;
package body Ada.Streams.Stream_IO.Standard_Files is

   Standard_Input_Object : aliased File_Type;
   Standard_Output_Object : aliased File_Type;
   Standard_Error_Object : aliased File_Type;

   --  implementation

   function Standard_Input return not null access constant File_Type is
   begin
      return Standard_Input_Object'Access;
   end Standard_Input;

   function Standard_Output return not null access constant File_Type is
   begin
      return Standard_Output_Object'Access;
   end Standard_Output;

   function Standard_Error return not null access constant File_Type is
   begin
      return Standard_Error_Object'Access;
   end Standard_Error;

begin
   Reference (Standard_Input_Object).all :=
      Inside.Standard_Files.Standard_Input;
   Reference (Standard_Output_Object).all :=
      Inside.Standard_Files.Standard_Output;
   Reference (Standard_Error_Object).all :=
      Inside.Standard_Files.Standard_Error;
end Ada.Streams.Stream_IO.Standard_Files;
