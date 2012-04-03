with Ada.Streams.Stream_IO.Inside;
with C.unistd;
package body Ada.Streams.Stream_IO.Pipes is
   use type Inside.Handle_Type;

   procedure Create (Reading, Writing : out File_Type) is
      Handles : array (0 .. 1) of aliased Inside.Handle_Type;
   begin
      if C.unistd.pipe (Handles (0)'Access) = -1 then
         raise Use_Error;
      else
         Inside.Open (
            Reading,
            Handles (0),
            In_File,
            To_Close => True);
         Inside.Open (
            Writing,
            Handles (1),
            Out_File,
            To_Close => True);
      end if;
   end Create;

end Ada.Streams.Stream_IO.Pipes;
