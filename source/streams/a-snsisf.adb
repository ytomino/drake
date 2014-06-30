package body Ada.Streams.Naked_Stream_IO.Standard_Files is
begin
   System.Native_IO.Initialize (
      Standard_Input_Stream.Handle,
      Standard_Output_Stream.Handle,
      Standard_Error_Stream.Handle);
end Ada.Streams.Naked_Stream_IO.Standard_Files;
