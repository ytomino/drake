pragma License (Unrestricted);
--  implementation unit
package Ada.Streams.Naked_Stream_IO.Standard_Files is
   pragma Elaborate_Body;

   Standard_Input : aliased constant Non_Controlled_File_Type;
   Standard_Output : aliased constant Non_Controlled_File_Type;
   Standard_Error : aliased constant Non_Controlled_File_Type;

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
      Mode => IO_Modes.In_File,
      Kind => Standard_Handle,
      Has_Full_Name => False,
      Name => Standard_Input_Name (0)'Access,
      Form => Default_Form,
      Buffer_Inline => 0,
      Buffer => System.Null_Address,
      Buffer_Length => Uninitialized_Buffer,
      Buffer_Index => 0,
      Reading_Index => 0,
      Writing_Index => 0,
      Closer => null,
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
      Mode => IO_Modes.Out_File,
      Kind => Standard_Handle,
      Has_Full_Name => False,
      Name => Standard_Output_Name (0)'Access,
      Form => Default_Form,
      Buffer_Inline => 0,
      Buffer => System.Null_Address,
      Buffer_Length => Uninitialized_Buffer,
      Buffer_Index => 0,
      Reading_Index => 0,
      Writing_Index => 0,
      Closer => null,
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
      Mode => IO_Modes.Out_File,
      Kind => Standard_Handle,
      Has_Full_Name => False,
      Name => Standard_Error_Name (0)'Access,
      Form => Default_Form,
      Buffer_Inline => 0,
      Buffer => System.Null_Address,
      Buffer_Length => Uninitialized_Buffer,
      Buffer_Index => 0,
      Reading_Index => 0,
      Writing_Index => 0,
      Closer => null,
      Dispatcher => (Tags.No_Tag, null));

   Standard_Input : aliased constant Non_Controlled_File_Type :=
      Standard_Input_Stream'Access;
   Standard_Output : aliased constant Non_Controlled_File_Type :=
      Standard_Output_Stream'Access;
   Standard_Error : aliased constant Non_Controlled_File_Type :=
      Standard_Error_Stream'Access;

end Ada.Streams.Naked_Stream_IO.Standard_Files;
