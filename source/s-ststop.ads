pragma License (Unrestricted);
--  implementation package required by compiler
with Ada.IO_Exceptions;
with Ada.Streams;
package System.Strings.Stream_Ops is
   pragma Pure;

   --  required for String'Read by compiler (s-ststop.ads)
   procedure String_Read_Blk_IO (
      Strm : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : out String);

   --  required for String'Write by compiler (s-ststop.ads)
   procedure String_Write_Blk_IO (
      Strm : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : String);

   --  required for String'Input by compiler (s-ststop.ads)
   function String_Input_Blk_IO (
      Strm : not null access Ada.Streams.Root_Stream_Type'Class)
      return String;

   --  required for String'Output by compiler (s-ststop.ads)
   procedure String_Output_Blk_IO (
      Strm : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : String);

   --  for shorthand
   End_Error : exception renames Ada.IO_Exceptions.End_Error;
   Data_Error : exception renames Ada.IO_Exceptions.Data_Error;

end System.Strings.Stream_Ops;
