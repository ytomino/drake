with Ada.Exception_Identification.From_Here;
with Ada.Streams.Stream_IO.Inside;
with System.Native_IO;
with C.unistd;
package body Ada.Streams.Stream_IO.Pipes is
   use Exception_Identification.From_Here;
   use type System.Native_IO.Handle_Type;

   procedure Create (Reading, Writing : out File_Type) is
      Handles : array (0 .. 1) of aliased System.Native_IO.Handle_Type;
   begin
      if C.unistd.pipe (Handles (0)'Access) = -1 then
         Raise_Exception (Use_Error'Identity);
      else
         System.Native_IO.Set_Close_On_Exec (Handles (0));
         Inside.Open (
            Reading,
            Handles (0),
            In_File,
            To_Close => True);
         System.Native_IO.Set_Close_On_Exec (Handles (1));
         Inside.Open (
            Writing,
            Handles (1),
            Out_File,
            To_Close => True);
      end if;
   end Create;

end Ada.Streams.Stream_IO.Pipes;
