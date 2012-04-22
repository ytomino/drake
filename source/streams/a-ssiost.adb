with Ada.Streams.Stream_IO.Inside.Standards;
package body Ada.Streams.Stream_IO.Standards is

   Standard_Input_Object : aliased File_Type;
   Standard_Output_Object : aliased File_Type;
   Standard_Error_Object : aliased File_Type;

   --  implementation

   function Standard_Input return not null access constant File_Type is
      Ref : constant not null access Inside.Non_Controlled_File_Type :=
         Reference (Standard_Input_Object);
   begin
      if not Inside.Is_Open (Ref.all) then
         Ref.all := Inside.Standards.Standard_Input;
      end if;
      return Standard_Input_Object'Access;
   end Standard_Input;

   function Standard_Output return not null access constant File_Type is
      Ref : constant not null access Inside.Non_Controlled_File_Type :=
         Reference (Standard_Output_Object);
   begin
      if not Inside.Is_Open (Ref.all) then
         Ref.all := Inside.Standards.Standard_Output;
      end if;
      return Standard_Output_Object'Access;
   end Standard_Output;

   function Standard_Error return not null access constant File_Type is
      Ref : constant not null access Inside.Non_Controlled_File_Type :=
         Reference (Standard_Error_Object);
   begin
      if not Inside.Is_Open (Ref.all) then
         Ref.all := Inside.Standards.Standard_Error;
      end if;
      return Standard_Error_Object'Access;
   end Standard_Error;

end Ada.Streams.Stream_IO.Standards;
