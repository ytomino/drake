package body Ada.Streams.Stream_IO.Inside.Standard_Files is
begin
   Standard_Input_Stream.Handle :=
      C.winbase.GetStdHandle (C.winbase.STD_INPUT_HANDLE);
   Standard_Output_Stream.Handle :=
      C.winbase.GetStdHandle (C.winbase.STD_OUTPUT_HANDLE);
   Standard_Error_Stream.Handle :=
      C.winbase.GetStdHandle (C.winbase.STD_ERROR_HANDLE);
end Ada.Streams.Stream_IO.Inside.Standard_Files;
