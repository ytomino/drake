with Ada.Streams.Stream_IO.Inside;
package body Ada.Streams.Stream_IO.Standards is

   Standard_Input_Object : aliased File_Type := (
      Finalization.Limited_Controlled with
      Stream => Inside.Standard_Input);

   Standard_Output_Object : aliased File_Type := (
      Finalization.Limited_Controlled with
      Stream => Inside.Standard_Output);

   Standard_Error_Object : aliased File_Type := (
      Finalization.Limited_Controlled with
      Stream => Inside.Standard_Error);

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

end Ada.Streams.Stream_IO.Standards;
