pragma License (Unrestricted);
--  implementation unit
private with C.winbase;
package Ada.Streams.Stream_IO.Inside.Standard_Files is
   pragma Elaborate_Body;

   Standard_Input : constant Non_Controlled_File_Type;
   Standard_Output : constant Non_Controlled_File_Type;
   Standard_Error : constant Non_Controlled_File_Type;

private
   use type C.char_array;

   Empty_Form : aliased C.char_array (0 .. 0) := (0 => C.char'Val (0));

   Standard_Input_Name : aliased C.winnt.WCHAR_array (0 .. 6) := (
      C.winnt.WCHAR'Val (Wide_Character'Pos ('*')),
      C.winnt.WCHAR'Val (Wide_Character'Pos ('s')),
      C.winnt.WCHAR'Val (Wide_Character'Pos ('t')),
      C.winnt.WCHAR'Val (Wide_Character'Pos ('d')),
      C.winnt.WCHAR'Val (Wide_Character'Pos ('i')),
      C.winnt.WCHAR'Val (Wide_Character'Pos ('n')),
      C.winnt.WCHAR'Val (0));

   Standard_Input_Stream : aliased Stream_Type := (
      Handle => C.winbase.INVALID_HANDLE_VALUE, -- overwrite when init
      Mode => In_File,
      Kind => Standard_Handle,
      Name => Standard_Input_Name (0)'Access,
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

   Standard_Output_Name : aliased C.winnt.WCHAR_array (0 .. 7) := (
      C.winnt.WCHAR'Val (Wide_Character'Pos ('*')),
      C.winnt.WCHAR'Val (Wide_Character'Pos ('s')),
      C.winnt.WCHAR'Val (Wide_Character'Pos ('t')),
      C.winnt.WCHAR'Val (Wide_Character'Pos ('d')),
      C.winnt.WCHAR'Val (Wide_Character'Pos ('o')),
      C.winnt.WCHAR'Val (Wide_Character'Pos ('u')),
      C.winnt.WCHAR'Val (Wide_Character'Pos ('t')),
      C.winnt.WCHAR'Val (0));

   Standard_Output_Stream : aliased Stream_Type := (
      Handle => C.winbase.INVALID_HANDLE_VALUE, -- overwrite when init
      Mode => Out_File,
      Kind => Standard_Handle,
      Name => Standard_Output_Name (0)'Access,
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

   Standard_Error_Name : aliased C.winnt.WCHAR_array (0 .. 7) := (
      C.winnt.WCHAR'Val (Wide_Character'Pos ('*')),
      C.winnt.WCHAR'Val (Wide_Character'Pos ('s')),
      C.winnt.WCHAR'Val (Wide_Character'Pos ('t')),
      C.winnt.WCHAR'Val (Wide_Character'Pos ('d')),
      C.winnt.WCHAR'Val (Wide_Character'Pos ('e')),
      C.winnt.WCHAR'Val (Wide_Character'Pos ('r')),
      C.winnt.WCHAR'Val (Wide_Character'Pos ('r')),
      C.winnt.WCHAR'Val (0));

   Standard_Error_Stream : aliased Stream_Type := (
      Handle => C.winbase.INVALID_HANDLE_VALUE, -- overwrite when init
      Mode => Out_File,
      Kind => Standard_Handle,
      Name => Standard_Error_Name (0)'Access,
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
