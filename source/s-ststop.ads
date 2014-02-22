pragma License (Unrestricted);
--  implementation unit required by compiler
with Ada.Streams.Block_Transmission;
with Ada.Streams.Block_Transmission.Strings;
with Ada.Streams.Block_Transmission.Wide_Strings;
with Ada.Streams.Block_Transmission.Wide_Wide_Strings;
package System.Strings.Stream_Ops is
   pragma Pure;

   --  required for String'Read by compiler (s-ststop.ads)
   procedure String_Read_Blk_IO (
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : out String)
      renames Ada.Streams.Block_Transmission.Strings.Read;

   --  required for String'Write by compiler (s-ststop.ads)
   procedure String_Write_Blk_IO (
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : String)
      renames Ada.Streams.Block_Transmission.Strings.Write;

   --  required for String'Input by compiler (s-ststop.ads)
   function String_Input_Blk_IO is
      new Ada.Streams.Block_Transmission.Input (
         Character,
         Positive,
         String,
         String_Read_Blk_IO);

   --  required for String'Output by compiler (s-ststop.ads)
   procedure String_Output_Blk_IO is
      new Ada.Streams.Block_Transmission.Output (
         Character,
         Positive,
         String,
         String_Write_Blk_IO);

   --  required for Wide_String'Read by compiler (s-ststop.ads)
   procedure Wide_String_Read_Blk_IO (
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : out Wide_String)
      renames Ada.Streams.Block_Transmission.Wide_Strings.Read;

   --  required for Wide_String'Write by compiler (s-ststop.ads)
   procedure Wide_String_Write_Blk_IO (
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : Wide_String)
      renames Ada.Streams.Block_Transmission.Wide_Strings.Write;

   --  required for Wide_String'Input by compiler (s-ststop.ads)
   function Wide_String_Input_Blk_IO is
      new Ada.Streams.Block_Transmission.Input (
         Wide_Character,
         Positive,
         Wide_String,
         Wide_String_Read_Blk_IO);

   --  required for Wide_String'Output by compiler (s-ststop.ads)
   procedure Wide_String_Output_Blk_IO is
      new Ada.Streams.Block_Transmission.Output (
         Wide_Character,
         Positive,
         Wide_String,
         Wide_String_Write_Blk_IO);

   --  required for Wide_Wide_String'Read by compiler (s-ststop.ads)
   procedure Wide_Wide_String_Read_Blk_IO (
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : out Wide_Wide_String)
      renames Ada.Streams.Block_Transmission.Wide_Wide_Strings.Read;

   --  required for Wide_Wide_String'Write by compiler (s-ststop.ads)
   procedure Wide_Wide_String_Write_Blk_IO (
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : Wide_Wide_String)
      renames Ada.Streams.Block_Transmission.Wide_Wide_Strings.Write;

   --  required for Wide_Wide_String'Input by compiler (s-ststop.ads)
   function Wide_Wide_String_Input_Blk_IO is
      new Ada.Streams.Block_Transmission.Input (
         Wide_Wide_Character,
         Positive,
         Wide_Wide_String,
         Wide_Wide_String_Read_Blk_IO);

   --  required for Wide_Wide_String'Output by compiler (s-ststop.ads)
   procedure Wide_Wide_String_Output_Blk_IO is
      new Ada.Streams.Block_Transmission.Output (
         Wide_Wide_Character,
         Positive,
         Wide_Wide_String,
         Wide_Wide_String_Write_Blk_IO);

end System.Strings.Stream_Ops;
