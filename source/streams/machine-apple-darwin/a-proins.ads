pragma License (Unrestricted);
--  implementation unit
with System.Native_IO;
with C.sys.types;
package Ada.Processes.Inside is

   procedure Spawn (
      Child : out C.sys.types.pid_t;
      Command_Line : String;
      Directory : String;
      Search_Path : Boolean;
      Input : System.Native_IO.Handle_Type;
      Output : System.Native_IO.Handle_Type;
      Error : System.Native_IO.Handle_Type);

end Ada.Processes.Inside;
