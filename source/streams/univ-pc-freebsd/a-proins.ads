pragma License (Unrestricted);
--  implementation unit
with Ada.Streams.Stream_IO.Inside;
with C.sys.types;
package Ada.Processes.Inside is

   procedure Spawn (
      Child : out C.sys.types.pid_t;
      Command_Line : String;
      Directory : String;
      Search_Path : Boolean;
      Input : Streams.Stream_IO.Inside.Handle_Type;
      Output : Streams.Stream_IO.Inside.Handle_Type;
      Error : Streams.Stream_IO.Inside.Handle_Type);

end Ada.Processes.Inside;
