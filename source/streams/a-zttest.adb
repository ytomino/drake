with Ada.Text_IO.Text_Streams;
package body Ada.Wide_Wide_Text_IO.Text_Streams is

   function Stream (File : File_Type) return Stream_Access is
   begin
      return Stream_Access (
         Text_IO.Text_Streams.Stream (Text_IO.File_Type (File)));
   end Stream;

end Ada.Wide_Wide_Text_IO.Text_Streams;
