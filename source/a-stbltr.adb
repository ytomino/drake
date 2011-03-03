package body Ada.Streams.Block_Transmission is
   pragma Suppress (All_Checks);
   use type Ada.Streams.Stream_Element_Offset;

   procedure Read (
      Stream : not null access Root_Stream_Type'Class;
      Item : out Array_Type) is
   begin
      if Array_Type'Component_Size = Element_Type'Size
         and then Element_Type'Size mod Stream_Element'Size = 0
      then
         declare
            Length : constant Natural := Item'Length;
            Item_As : Stream_Element_Array (
               1 ..
               Streams.Stream_Element_Offset (
                  (Element_Type'Stream_Size / Stream_Element'Size) * Length));
            for Item_As'Address use Item'Address;
            Last : Stream_Element_Offset;
         begin
            Read (Stream.all, Item_As, Last);
            if Last < Item_As'Last then
               raise End_Error;
            end if;
         end;
      else
         Array_Type'Read (Stream, Item);
      end if;
   end Read;

   procedure Write (
      Stream : not null access Root_Stream_Type'Class;
      Item : Array_Type) is
   begin
      if Array_Type'Component_Size = Element_Type'Size
         and then Element_Type'Size mod Stream_Element'Size = 0
      then
         declare
            Length : constant Natural := Item'Length;
            Item_As : Stream_Element_Array (
               1 ..
               Streams.Stream_Element_Offset (
                  (Element_Type'Stream_Size / Stream_Element'Size) * Length));
            for Item_As'Address use Item'Address;
         begin
            Write (Stream.all, Item_As);
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
         raise Data_Error;
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

end Ada.Streams.Block_Transmission;
