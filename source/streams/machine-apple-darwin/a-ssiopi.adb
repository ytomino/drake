with Ada.Exception_Identification.From_Here;
with Ada.Streams.Stream_IO.Naked;
with System.Native_IO;
with C.unistd;
package body Ada.Streams.Stream_IO.Pipes is
   use Exception_Identification.From_Here;
   use type System.Native_IO.Handle_Type;

   procedure Create (Reading, Writing : out File_Type) is
      Handles : array (0 .. 1) of aliased System.Native_IO.Handle_Type;
   begin
      if C.unistd.pipe (Handles (0)'Access) < 0 then
         Raise_Exception (Use_Error'Identity);
      else
         System.Native_IO.Set_Close_On_Exec (Handles (0));
         Naked.Open (
            Reading,
            In_File,
            Handles (0),
            To_Close => True);
         System.Native_IO.Set_Close_On_Exec (Handles (1));
         Naked.Open (
            Writing,
            Out_File,
            Handles (1),
            To_Close => True);
      end if;
   end Create;

end Ada.Streams.Stream_IO.Pipes;
