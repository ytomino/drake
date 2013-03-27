pragma License (Unrestricted);
--  implementation unit
package Ada.Streams.Stream_IO.Inside.Standard_Files is

   Standard_Input : constant Non_Controlled_File_Type;
   Standard_Output : constant Non_Controlled_File_Type;
   Standard_Error : constant Non_Controlled_File_Type;

private
   use type C.char_array;

   Empty_Form : aliased C.char_array (0 .. 0) := (0 => C.char'Val (0));

   Standard_Input_Name : aliased C.char_array (0 .. 6) :=
      "*stdin" & C.char'Val (0);

   Standard_Input_Stream : aliased Stream_Type := (
      Handle => 0,
      Mode => In_File,
      Kind => Standard_Handle,
      Name => Standard_Input_Name'Address,
      Name_Length => 6,
      Form => Empty_Form'Address,
      Form_Length => 0,
      Buffer => System.Null_Address,
      Buffer_Inline => 0,
      Buffer_Length => Uninitialized_Buffer,
      Buffer_Index => 0,
      Reading_Index => 0,
      Writing_Index => 0,
      Dispatcher => (Tags.No_Tag, null));

   Standard_Output_Name : aliased C.char_array (0 .. 7) :=
      "*stdout" & C.char'Val (0);

   Standard_Output_Stream : aliased Stream_Type := (
      Handle => 1,
      Mode => Out_File,
      Kind => Standard_Handle,
      Name => Standard_Output_Name'Address,
      Name_Length => 7,
      Form => Empty_Form'Address,
      Form_Length => 0,
      Buffer => System.Null_Address,
      Buffer_Inline => 0,
      Buffer_Length => Uninitialized_Buffer,
      Buffer_Index => 0,
      Reading_Index => 0,
      Writing_Index => 0,
      Dispatcher => (Tags.No_Tag, null));

   Standard_Error_Name : aliased C.char_array (0 .. 7) :=
      "*stderr" & C.char'Val (0);

   Standard_Error_Stream : aliased Stream_Type := (
      Handle => 2,
      Mode => Out_File,
      Kind => Standard_Handle,
      Name => Standard_Error_Name'Address,
      Name_Length => 7,
      Form => Empty_Form'Address,
      Form_Length => 0,
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
