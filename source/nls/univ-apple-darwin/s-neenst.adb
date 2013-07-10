with Ada.Exceptions;
package body System.Native_Encoding.Encoding_Streams is
   use type Ada.Streams.Stream_Element;
   use type Ada.Streams.Stream_Element_Array;

   subtype Stream_Element_Offset is Ada.Streams.Stream_Element_Offset;

   function Open (
      Internal : Encoding_Id;
      External : Encoding_Id;
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Substitute : Ada.Streams.Stream_Element := Default_Substitute)
      return Encoding
   is
      pragma Suppress (Accessibility_Check);
   begin
      return Result : Encoding do
         Result.Internal := Internal;
         Result.External := External;
         Result.Stream := Stream;
         Result.Substitute := Substitute;
         Result.Reading_Last := -1;
         Result.Writing_Last := -1;
      end return;
   end Open;

   function Is_Open (Object : Encoding) return Boolean is
   begin
      return Object.Stream /= null;
   end Is_Open;

   function Stream (Object : aliased in out Encoding)
      return not null access Ada.Streams.Root_Stream_Type'Class is
   begin
      if not Is_Open (Object) then
         Ada.Exceptions.Raise_Exception_From_Here (Status_Error'Identity);
      end if;
      return Object'Unchecked_Access;
   end Stream;

   overriding procedure Read (
      Object : in out Encoding;
      Item : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset) is
   begin
      if not Is_Open (Object.Reading_Converter) then
         Open (
            Object.Reading_Converter,
            From => Object.External,
            To => Object.Internal);
      end if;
      Last := Item'First - 1;
      declare
         End_Error_Received : Boolean := False;
         Buffer : Ada.Streams.Stream_Element_Array
            renames Object.Reading_Buffer;
         Buffer_Last : Stream_Element_Offset
            renames Object.Reading_Last;
      begin
         loop
            --  filling
            if not End_Error_Received then
               begin
                  Ada.Streams.Read (
                     Object.Stream.all,
                     Buffer (Buffer_Last + 1 .. Buffer'Last),
                     Buffer_Last);
               exception
                  when End_Error =>
                     End_Error_Received := True;
               end;
            end if;
            exit when Buffer_Last < Buffer'First;
            --  try to convert one multi-byte character
            declare
               Taken : Stream_Element_Offset;
               Out_Buffer : Ada.Streams.Stream_Element_Array (
                  0 ..
                  Expanding - 1);
               Out_Last : Stream_Element_Offset;
               Status : Error_Status;
            begin
               Convert (
                  Object.Reading_Converter,
                  Buffer (Buffer'First .. Buffer_Last),
                  Taken,
                  Out_Buffer,
                  Out_Last,
                  Status);
               case Status is
                  when Fine =>
                     null;
                  when Incomplete =>
                     exit; -- wait tail-bytes
                  when Illegal_Sequence =>
                     Out_Buffer (Out_Buffer'First) := Object.Substitute;
                     Out_Last := Out_Buffer'First;
                     if Taken < Buffer'First then
                        Taken := Buffer'First; -- skip one byte at least
                     end if;
               end case;
               --  write one converted multi-byte character
               declare
                  New_Last : constant Stream_Element_Offset :=
                     Last + (Out_Last - Out_Buffer'First + 1);
               begin
                  exit when New_Last > Item'Last; -- overflow
                  Item (Last + 1 .. New_Last) :=
                     Out_Buffer (Out_Buffer'First .. Out_Last);
                  Last := New_Last;
               end;
               --  drop one multi-byte character
               declare
                  New_Buffer_Last : constant Stream_Element_Offset :=
                     Buffer_Last - (Taken - Buffer'First + 1);
               begin
                  Buffer (Buffer'First .. New_Buffer_Last) :=
                     Buffer (Taken + 1 .. Buffer_Last);
                  Buffer_Last := New_Buffer_Last;
               end;
            end;
         end loop;
         if Last = Item'First - 1 -- do not use "<" since underflow
            and then Buffer_Last < Buffer'First
            and then End_Error_Received
         then
            Ada.Exceptions.Raise_Exception_From_Here (End_Error'Identity);
         end if;
      end;
   end Read;

   overriding procedure Write (
      Object : in out Encoding;
      Item : Ada.Streams.Stream_Element_Array) is
   begin
      if not Is_Open (Object.Writing_Converter) then
         Open (
            Object.Writing_Converter,
            From => Object.Internal,
            To => Object.External);
      end if;
      declare
         Item_Index : Stream_Element_Offset := Item'First;
         Buffer : Ada.Streams.Stream_Element_Array
            renames Object.Writing_Buffer;
         Buffer_Last : Stream_Element_Offset
            renames Object.Writing_Last;
      begin
         loop
            --  filling
            if Item_Index <= Item'Last then
               declare
                  Rest : constant Stream_Element_Offset :=
                     Stream_Element_Offset'Min (
                        Item'Last - Item_Index + 1,
                        Buffer'Last - Buffer_Last);
                  New_Buffer_Last : constant Stream_Element_Offset :=
                     Buffer_Last + Rest;
                  New_Item_Index : constant Stream_Element_Offset :=
                     Item_Index + Rest;
               begin
                  Buffer (Buffer_Last + 1 .. New_Buffer_Last) :=
                     Item (Item_Index .. New_Item_Index - 1);
                  Buffer_Last := New_Buffer_Last;
                  Item_Index := New_Item_Index;
               end;
            else
               exit when Buffer_Last < Buffer'First;
            end if;
            --  try to convert one multi-byte character
            declare
               Taken : Stream_Element_Offset;
               Out_Buffer : Ada.Streams.Stream_Element_Array (
                  0 ..
                  Expanding - 1);
               Out_Last : Stream_Element_Offset;
               Status : Error_Status;
            begin
               Convert (
                  Object.Writing_Converter,
                  Buffer (Buffer'First .. Buffer_Last),
                  Taken,
                  Out_Buffer,
                  Out_Last,
                  Status);
               case Status is
                  when Fine =>
                     null;
                  when Incomplete =>
                     exit; -- wait tail-bytes
                  when Illegal_Sequence =>
                     Out_Buffer (Out_Buffer'First) := Object.Substitute;
                     Out_Last := Out_Buffer'First;
                     if Taken < Buffer'First then
                        Taken := Buffer'First; -- skip one byte at least
                     end if;
               end case;
               --  write one converted multi-byte character
               Ada.Streams.Write (
                  Object.Stream.all,
                  Out_Buffer (Out_Buffer'First .. Out_Last));
               --  drop one multi-byte character
               declare
                  New_Buffer_Last : constant Stream_Element_Offset :=
                     Buffer_Last - (Taken - Buffer'First + 1);
               begin
                  Buffer (Buffer'First .. New_Buffer_Last) :=
                     Buffer (Taken + 1 .. Buffer_Last);
                  Buffer_Last := New_Buffer_Last;
               end;
            end;
         end loop;
      end;
   end Write;

end System.Native_Encoding.Encoding_Streams;
