package body System.Strings.Stream_Ops is
   pragma Suppress (All_Checks);
   use type Ada.Streams.Stream_Element_Offset;

   procedure String_Read_Blk_IO (
      Strm : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : out String)
   is
      Item_As : Ada.Streams.Stream_Element_Array (
         Ada.Streams.Stream_Element_Offset (Item'First) ..
         Ada.Streams.Stream_Element_Offset (Item'Last));
      for Item_As'Address use Item'Address;
      Last : Ada.Streams.Stream_Element_Offset;
   begin
      Ada.Streams.Read (Strm.all, Item_As, Last);
      if Last < Item_As'Last then
         raise End_Error;
      end if;
   end String_Read_Blk_IO;

   procedure String_Write_Blk_IO (
      Strm : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : String)
   is
      Item_As : Ada.Streams.Stream_Element_Array (
         Ada.Streams.Stream_Element_Offset (Item'First) ..
         Ada.Streams.Stream_Element_Offset (Item'Last));
      for Item_As'Address use Item'Address;
   begin
      Ada.Streams.Write (Strm.all, Item_As);
   end String_Write_Blk_IO;

   function String_Input_Blk_IO (
      Strm : not null access Ada.Streams.Root_Stream_Type'Class)
      return String
   is
      First, Last : Integer;
   begin
      Integer'Read (Strm, First);
      Integer'Read (Strm, Last);
      if First <= 0 or else Last < First - 1 then
         raise Data_Error;
      end if;
      return Result : String (First .. Last) do
         String_Read_Blk_IO (Strm, Result);
      end return;
   end String_Input_Blk_IO;

   procedure String_Output_Blk_IO (
      Strm : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : String) is
   begin
      Integer'Write (Strm, Item'First);
      Integer'Write (Strm, Item'Last);
      String_Write_Blk_IO (Strm, Item);
   end String_Output_Blk_IO;

end System.Strings.Stream_Ops;
