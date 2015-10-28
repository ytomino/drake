with Ada.Exception_Identification.From_Here;
package body Ada.Streams.Block_Transmission is
   pragma Suppress (All_Checks);
   use Exception_Identification.From_Here;

   procedure Read (
      Stream : not null access Root_Stream_Type'Class;
      Item : out Array_Type) is
   begin
      if Element_Type'Size = Element_Type'Stream_Size
         and then Array_Type'Component_Size = Element_Type'Stream_Size
      then
         declare
            Item_As : Stream_Element_Array (
               1 ..
               Stream_Element_Offset (
                  (Element_Type'Stream_Size / Stream_Element'Size)
                  * Item'Length));
            for Item_As'Address use Item'Address;
         begin
            Stream_Element_Arrays.Read (Stream, Item_As);
         end;
      else
         Array_Type'Read (Stream, Item);
      end if;
   end Read;

   procedure Write (
      Stream : not null access Root_Stream_Type'Class;
      Item : Array_Type) is
   begin
      if Element_Type'Size = Element_Type'Stream_Size
         and then Array_Type'Component_Size = Element_Type'Stream_Size
      then
         declare
            Item_As : Stream_Element_Array (
               1 ..
               Stream_Element_Offset (
                  (Element_Type'Stream_Size / Stream_Element'Size)
                  * Item'Length));
            for Item_As'Address use Item'Address;
         begin
            Stream_Element_Arrays.Write (Stream, Item_As);
         end;
      else
         Array_Type'Write (Stream, Item);
      end if;
   end Write;

   function Input (
      Stream : not null access Root_Stream_Type'Class)
      return Array_Type
   is
      First, Last : Index_Type'Base;
   begin
      Index_Type'Read (Stream, First);
      Index_Type'Read (Stream, Last);
      if First < Index_Type'First or else Last < Index_Type'Pred (First) then
         Raise_Exception (Data_Error'Identity);
      end if;
      return Result : Array_Type (First .. Last) do
         Read (Stream, Result);
      end return;
   end Input;

   procedure Output (
      Stream : not null access Root_Stream_Type'Class;
      Item : Array_Type) is
   begin
      Index_Type'Write (Stream, Item'First);
      Index_Type'Write (Stream, Item'Last);
      Write (Stream, Item);
   end Output;

   package body Stream_Element_Arrays is

      procedure Read (
         Stream : not null access Root_Stream_Type'Class;
         Item : out Stream_Element_Array)
      is
         Previous_Last : Stream_Element_Offset;
         Last : Stream_Element_Offset := Item'First - 1;
      begin
         while Last < Item'Last loop
            Previous_Last := Last;
            Streams.Read (
               Stream.all,
               Item (Previous_Last + 1 .. Item'Last),
               Last);
            if Last <= Previous_Last then
               Raise_Exception (End_Error'Identity);
            end if;
         end loop;
      end Read;

      procedure Write (
         Stream : not null access Root_Stream_Type'Class;
         Item : Stream_Element_Array) is
      begin
         Streams.Write (Stream.all, Item);
      end Write;

   end Stream_Element_Arrays;

end Ada.Streams.Block_Transmission;
