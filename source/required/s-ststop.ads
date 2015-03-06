pragma License (Unrestricted);
--  implementation unit required by compiler
with Ada.Streams.Block_Transmission.Strings;
with Ada.Streams.Block_Transmission.Wide_Strings;
with Ada.Streams.Block_Transmission.Wide_Wide_Strings;
with System.Storage_Elements;
package System.Strings.Stream_Ops is
   pragma Pure;

   pragma Suppress (All_Checks); -- for instantiation

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

   --  required for System.Storage_Elements.Storage_Array'Read (s-ststop.ads)
   procedure Storage_Array_Read_Blk_IO is
      new Ada.Streams.Block_Transmission.Read (
         Storage_Elements.Storage_Element,
         Storage_Elements.Storage_Offset,
         Storage_Elements.Storage_Array);

   --  required for System.Storage_Elements.Storage_Array'Write (s-ststop.ads)
   procedure Storage_Array_Write_Blk_IO is
      new Ada.Streams.Block_Transmission.Write (
         Storage_Elements.Storage_Element,
         Storage_Elements.Storage_Offset,
         Storage_Elements.Storage_Array);

   --  required for System.Storage_Elements.Storage_Array'Input (s-ststop.ads)
   function Storage_Array_Input_Blk_IO is
      new Ada.Streams.Block_Transmission.Input (
         Storage_Elements.Storage_Element,
         Storage_Elements.Storage_Offset,
         Storage_Elements.Storage_Array,
         Storage_Array_Read_Blk_IO);

   --  required for System.Storage_Elements.Storage_Array'Output (s-ststop.ads)
   procedure Storage_Array_Output_Blk_IO is
      new Ada.Streams.Block_Transmission.Output (
         Storage_Elements.Storage_Element,
         Storage_Elements.Storage_Offset,
         Storage_Elements.Storage_Array,
         Storage_Array_Write_Blk_IO);

   --  required for Ada.Streams.Stream_Element_Array'Read (s-ststop.ads)
   procedure Stream_Element_Array_Read_Blk_IO (
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : out Ada.Streams.Stream_Element_Array)
      renames Ada.Streams.Block_Transmission.Stream_Element_Arrays.Read;

   --  required for Ada.Streams.Stream_Element_Array'Write (s-ststop.ads)
   procedure Stream_Element_Array_Write_Blk_IO (
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : Ada.Streams.Stream_Element_Array)
      renames Ada.Streams.Block_Transmission.Stream_Element_Arrays.Write;

   --  required for Ada.Streams.Stream_Element_Array'Input (s-ststop.ads)
   function Stream_Element_Array_Input_Blk_IO is
      new Ada.Streams.Block_Transmission.Input (
         Ada.Streams.Stream_Element,
         Ada.Streams.Stream_Element_Offset,
         Ada.Streams.Stream_Element_Array,
         Stream_Element_Array_Read_Blk_IO);

   --  required for Ada.Streams.Stream_Element_Array'Output (s-ststop.ads)
   procedure Stream_Element_Array_Output_Blk_IO is
      new Ada.Streams.Block_Transmission.Output (
         Ada.Streams.Stream_Element,
         Ada.Streams.Stream_Element_Offset,
         Ada.Streams.Stream_Element_Array,
         Stream_Element_Array_Write_Blk_IO);

end System.Strings.Stream_Ops;
