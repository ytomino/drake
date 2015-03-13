with Ada.Streams.Stream_IO.Naked;
with System.Native_IO;
package body Ada.Streams.Stream_IO.Pipes is

   procedure Create (Reading, Writing : out File_Type) is
      Handles : array (0 .. 1) of aliased System.Native_IO.Handle_Type;
   begin
      System.Native_IO.Open_Pipe (Handles (0), Handles (1));
      Naked.Open (Reading, In_File, Handles (0), To_Close => True);
      Naked.Open (Writing, Out_File, Handles (1), To_Close => True);
   end Create;

end Ada.Streams.Stream_IO.Pipes;
