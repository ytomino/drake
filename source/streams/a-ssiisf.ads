pragma License (Unrestricted);
--  implementation unit
package Ada.Streams.Stream_IO.Inside.Standard_Files is
   pragma Elaborate_Body;

   Standard_Input : constant Non_Controlled_File_Type;
   Standard_Output : constant Non_Controlled_File_Type;
   Standard_Error : constant Non_Controlled_File_Type;

private

   Standard_Input_Name : aliased System.Native_IO.Name_String (0 .. 6) := (
      System.Native_IO.Name_Character'Val (Character'Pos ('*')),
      System.Native_IO.Name_Character'Val (Character'Pos ('s')),
      System.Native_IO.Name_Character'Val (Character'Pos ('t')),
      System.Native_IO.Name_Character'Val (Character'Pos ('d')),
      System.Native_IO.Name_Character'Val (Character'Pos ('i')),
      System.Native_IO.Name_Character'Val (Character'Pos ('n')),
      System.Native_IO.Name_Character'Val (0));

   Standard_Input_Stream : aliased Stream_Type := (
      Handle => System.Native_IO.Uninitialized_Standard_Input,
      Mode => In_File,
      Kind => Standard_Handle,
      Name => Standard_Input_Name (0)'Access,
      Name_Length => 6,
      Form => Default_Form,
      Buffer => System.Null_Address,
      Buffer_Inline => 0,
      Buffer_Length => Uninitialized_Buffer,
      Buffer_Index => 0,
      Reading_Index => 0,
      Writing_Index => 0,
      Dispatcher => (Tags.No_Tag, null));

   Standard_Output_Name : aliased System.Native_IO.Name_String (0 .. 7) := (
      System.Native_IO.Name_Character'Val (Character'Pos ('*')),
      System.Native_IO.Name_Character'Val (Character'Pos ('s')),
      System.Native_IO.Name_Character'Val (Character'Pos ('t')),
      System.Native_IO.Name_Character'Val (Character'Pos ('d')),
      System.Native_IO.Name_Character'Val (Character'Pos ('o')),
      System.Native_IO.Name_Character'Val (Character'Pos ('u')),
      System.Native_IO.Name_Character'Val (Character'Pos ('t')),
      System.Native_IO.Name_Character'Val (0));

   Standard_Output_Stream : aliased Stream_Type := (
      Handle => System.Native_IO.Uninitialized_Standard_Output,
      Mode => Out_File,
      Kind => Standard_Handle,
      Name => Standard_Output_Name (0)'Access,
      Name_Length => 7,
      Form => Default_Form,
      Buffer => System.Null_Address,
      Buffer_Inline => 0,
      Buffer_Length => Uninitialized_Buffer,
      Buffer_Index => 0,
      Reading_Index => 0,
      Writing_Index => 0,
      Dispatcher => (Tags.No_Tag, null));

   Standard_Error_Name : aliased System.Native_IO.Name_String (0 .. 7) := (
      System.Native_IO.Name_Character'Val (Character'Pos ('*')),
      System.Native_IO.Name_Character'Val (Character'Pos ('s')),
      System.Native_IO.Name_Character'Val (Character'Pos ('t')),
      System.Native_IO.Name_Character'Val (Character'Pos ('d')),
      System.Native_IO.Name_Character'Val (Character'Pos ('e')),
      System.Native_IO.Name_Character'Val (Character'Pos ('r')),
      System.Native_IO.Name_Character'Val (Character'Pos ('r')),
      System.Native_IO.Name_Character'Val (0));

   Standard_Error_Stream : aliased Stream_Type := (
      Handle => System.Native_IO.Uninitialized_Standard_Error,
      Mode => Out_File,
      Kind => Standard_Handle,
      Name => Standard_Error_Name (0)'Access,
      Name_Length => 7,
      Form => Default_Form,
      Buffer => System.Null_Address,
      Buffer_Inline => 0,
      Buffer_Length => Uninitialized_Buffer,
      Buffer_Index => 0,
      Reading_Index => 0,
      Writing_Index => 0,
      Dispatcher => (Tags.No_Tag, null));

   Standard_Input : constant Non_Controlled_File_Type :=
      Standard_Input_Stream'Access;
   Standard_Output : constant Non_Controlled_File_Type :=
      Standard_Output_Stream'Access;
   Standard_Error : constant Non_Controlled_File_Type :=
      Standard_Error_Stream'Access;

end Ada.Streams.Stream_IO.Inside.Standard_Files;
