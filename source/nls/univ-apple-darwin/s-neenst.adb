with Ada.Exceptions;
package body System.Native_Encoding.Encoding_Streams is
   use type Ada.Streams.Stream_Element;
   use type Ada.Streams.Stream_Element_Array;

   subtype Stream_Element_Offset is Ada.Streams.Stream_Element_Offset;

   procedure Set_Substitute_To_Reading_Converter (Object : in out Encoding);
   procedure Set_Substitute_To_Reading_Converter (Object : in out Encoding) is
   begin
      Set_Substitute (
         Object.Reading_Converter,
         Object.Substitute (1 .. Object.Substitute_Length));
   end Set_Substitute_To_Reading_Converter;

   procedure Set_Substitute_To_Writing_Converter (Object : in out Encoding);
   procedure Set_Substitute_To_Writing_Converter (Object : in out Encoding) is
      S2 : Ada.Streams.Stream_Element_Array (1 .. Max_Substitute_Length);
      S2_Length : Ada.Streams.Stream_Element_Offset;
   begin
      --  convert substitute from internal to external
      Convert (
         Object.Writing_Converter,
         Object.Substitute (1 .. Object.Substitute_Length),
         S2,
         S2_Length);
      Set_Substitute (
         Object.Writing_Converter,
         S2 (1 .. S2_Length));
   end Set_Substitute_To_Writing_Converter;

   procedure Adjust_Buffer (
      Buffer : in out Buffer_Type;
      First : in out Ada.Streams.Stream_Element_Offset;
      Last : in out Ada.Streams.Stream_Element_Offset);
   procedure Adjust_Buffer (
      Buffer : in out Buffer_Type;
      First : in out Ada.Streams.Stream_Element_Offset;
      Last : in out Ada.Streams.Stream_Element_Offset) is
   begin
      if First >= Buffer_Type'First + Half_Buffer_Length then
         --  shift
         declare
            New_Last : constant Ada.Streams.Stream_Element_Offset :=
               Buffer_Type'First + Last - First;
         begin
            Buffer (Buffer_Type'First .. New_Last) := Buffer (First .. Last);
            First := Buffer_Type'First;
            Last := New_Last;
         end;
      end if;
   end Adjust_Buffer;

   --  implementation

   function Open (
      Internal : Encoding_Id;
      External : Encoding_Id;
      Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Encoding
   is
      pragma Suppress (Accessibility_Check);
   begin
      return Result : Encoding do
         Result.Internal := Internal;
         Result.External := External;
         Result.Stream := Stream;
         Result.Substitute_Length := -1;
         Result.Reading_First := Buffer_Type'First;
         Result.Reading_Last := Buffer_Type'First - 1;
         Result.Reading_Converted_First := Buffer_Type'First;
         Result.Reading_Converted_Last := Buffer_Type'First - 1;
         Result.Reading_Status := Continuing;
         Result.Writing_First := Buffer_Type'First;
         Result.Writing_Last := Buffer_Type'First - 1;
      end return;
   end Open;

   function Is_Open (Object : Encoding) return Boolean is
   begin
      return Object.Stream /= null;
   end Is_Open;

   function Substitute (Object : Encoding)
      return Ada.Streams.Stream_Element_Array is
   begin
      if Object.Substitute_Length < 0 then
         return Default_Substitute (Object.Internal);
      else
         return Object.Substitute (1 .. Object.Substitute_Length);
      end if;
   end Substitute;

   procedure Set_Substitute (
      Object : in out Encoding;
      Substitute : Ada.Streams.Stream_Element_Array) is
   begin
      if Substitute'Length > Object.Substitute'Length then
         raise Constraint_Error;
      end if;
      Object.Substitute_Length := Substitute'Length;
      Object.Substitute (1 .. Object.Substitute_Length) := Substitute;
      --  set to converters
      if Is_Open (Object.Reading_Converter) then
         Set_Substitute_To_Reading_Converter (Object);
      end if;
      if Is_Open (Object.Writing_Converter) then
         Set_Substitute_To_Writing_Converter (Object);
      end if;
   end Set_Substitute;

   function Stream (Object : aliased in out Encoding)
      return not null access Ada.Streams.Root_Stream_Type'Class is
   begin
      if not Is_Open (Object) then
         Ada.Exceptions.Raise_Exception_From_Here (Status_Error'Identity);
      end if;
      return Object'Unchecked_Access;
   end Stream;

   procedure Finish (Object : in out Encoding) is
   begin
      if Is_Open (Object.Writing_Converter) then
         declare
            Buffer_First : Stream_Element_Offset
               renames Object.Writing_First;
            Buffer_Last : Stream_Element_Offset
               renames Object.Writing_Last;
            Out_Buffer : Ada.Streams.Stream_Element_Array (0 .. 63);
            Out_Last : Stream_Element_Offset := -1;
            Finishing_Status : Finishing_Status_Type;
         begin
            if Buffer_First <= Buffer_Last then
               --  put substitute instead of incomplete sequence in the buffer
               Put_Substitute (
                  Object.Writing_Converter,
                  Out_Buffer (Out_Last + 1 .. Out_Buffer'Last),
                  Out_Last);
            end if;
            --  finish
            Convert_No_Check (
               Object.Writing_Converter,
               Out_Buffer (Out_Last + 1 .. Out_Buffer'Last),
               Out_Last,
               Finishing_Status);
            --  write
            Ada.Streams.Write (
               Object.Stream.all,
               Out_Buffer (Out_Buffer'First .. Out_Last));
         end;
      end if;
   end Finish;

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
         if Object.Substitute_Length >= 0 then
            Set_Substitute_To_Reading_Converter (Object);
         end if;
      end if;
      Last := Item'First - 1;
      declare
         Buffer : Ada.Streams.Stream_Element_Array
            renames Object.Reading_Buffer;
         Buffer_First : Stream_Element_Offset
            renames Object.Reading_First;
         Buffer_Last : Stream_Element_Offset
            renames Object.Reading_Last;
         Converted : Ada.Streams.Stream_Element_Array
            renames Object.Reading_Converted_Buffer;
         Converted_First : Stream_Element_Offset
            renames Object.Reading_Converted_First;
         Converted_Last : Stream_Element_Offset
            renames Object.Reading_Converted_Last;
      begin
         while Last /= Item'Last loop
            --  filling
            if Object.Reading_Status = Continuing then
               Adjust_Buffer (Buffer, Buffer_First, Buffer_Last);
               if Buffer_Last /= Buffer'Last then
                  begin
                     Ada.Streams.Read (
                        Object.Stream.all,
                        Buffer (Buffer_Last + 1 .. Buffer'Last),
                        Buffer_Last);
                  exception
                     when End_Error =>
                        Object.Reading_Status := Finishing;
                  end;
               end if;
            end if;
            --  converting
            if Buffer_First <= Buffer_Last then
               Adjust_Buffer (Converted, Converted_First, Converted_Last);
               --  try to convert subsequence
               declare
                  Taken : Stream_Element_Offset;
                  Status : Status_Type;
               begin
                  Convert_No_Check (
                     Object.Reading_Converter,
                     Buffer (Buffer_First .. Buffer_Last),
                     Taken,
                     Converted (Converted_Last + 1 .. Converted'Last),
                     Converted_Last,
                     Status);
                  case Status is
                     when Fine =>
                        null;
                     when Insufficient =>
                        if Converted_Last < Converted_First then
                           raise Constraint_Error; -- Converted is too smaller
                        end if;
                     when Incomplete =>
                        if Object.Reading_Status /= Continuing then
                           --  put substitute instead of incomplete sequence
                           Put_Substitute (
                              Object.Reading_Converter,
                              Converted (Converted_Last + 1 .. Converted'Last),
                              Converted_Last);
                           --  to finishing phase
                           Buffer_First := Buffer_Last + 1;
                        elsif Converted_Last < Converted_First then
                           exit; -- wait tail-bytes
                        end if;
                     when Illegal_Sequence =>
                        Put_Substitute (
                           Object.Reading_Converter,
                           Converted (Converted_Last + 1 .. Converted'Last),
                           Converted_Last);
                        --  skip one element
                        Taken := Stream_Element_Offset'Min (
                           Buffer_Last,
                           Buffer_First
                              + Min_Size_In_From_Stream_Elements (
                                 Object.Writing_Converter)
                              - 1);
                  end case;
                  --  drop converted subsequence
                  Buffer_First := Taken + 1;
               end;
            elsif Object.Reading_Status = Finishing then
               Adjust_Buffer (Converted, Converted_First, Converted_Last);
               --  finish
               declare
                  Status : Finishing_Status_Type;
               begin
                  Convert_No_Check (
                     Object.Reading_Converter,
                     Converted (Converted_Last + 1 .. Converted'Last),
                     Converted_Last,
                     Status);
                  case Status is
                     when Fine =>
                        Object.Reading_Status := Ended;
                     when Insufficient =>
                        if Converted_Last < Converted_First then
                           raise Constraint_Error; -- Converted is too smaller
                        end if;
                  end case;
               end;
            end if;
            --  copy converted elements
            declare
               Move_Length : constant Stream_Element_Offset :=
                  Stream_Element_Offset'Min (
                     Item'Last - Last,
                     Converted_Last - Converted_First + 1);
               New_Last : constant Stream_Element_Offset :=
                  Last + Move_Length;
               New_Converted_First : constant Stream_Element_Offset :=
                  Converted_First + Move_Length;
            begin
               Item (Last + 1 .. New_Last) :=
                  Converted (Converted_First .. New_Converted_First - 1);
               Last := New_Last;
               Converted_First := New_Converted_First;
            end;
            exit when Object.Reading_Status = Ended
               and then Converted_Last < Converted_First;
         end loop;
         if Last = Item'First - 1 -- do not use "<" since underflow
            and then Object.Reading_Status = Ended
            and then Converted_Last < Converted_First
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
         if Object.Substitute_Length >= 0 then
            Set_Substitute_To_Writing_Converter (Object);
         end if;
      end if;
      declare
         Item_Last : Stream_Element_Offset := Item'First - 1;
         Buffer : Ada.Streams.Stream_Element_Array
            renames Object.Writing_Buffer;
         Buffer_First : Stream_Element_Offset
            renames Object.Writing_First;
         Buffer_Last : Stream_Element_Offset
            renames Object.Writing_Last;
      begin
         loop
            --  filling
            if Item_Last /= Item'Last then
               Adjust_Buffer (Buffer, Buffer_First, Buffer_Last);
               if Buffer_Last /= Buffer'Last then
                  declare
                     Rest : constant Stream_Element_Offset :=
                        Stream_Element_Offset'Min (
                           Item'Last - Item_Last,
                           Buffer'Last - Buffer_Last);
                     New_Buffer_Last : constant Stream_Element_Offset :=
                        Buffer_Last + Rest;
                     New_Item_Last : constant Stream_Element_Offset :=
                        Item_Last + Rest;
                  begin
                     Buffer (Buffer_Last + 1 .. New_Buffer_Last) :=
                        Item (Item_Last + 1 .. New_Item_Last);
                     Buffer_Last := New_Buffer_Last;
                     Item_Last := New_Item_Last;
                  end;
               end if;
            else
               exit when Buffer_Last < Buffer_First;
            end if;
            --  try to convert subsequence
            declare
               Taken : Stream_Element_Offset;
               Out_Buffer : Ada.Streams.Stream_Element_Array (0 .. 63);
               Out_Last : Stream_Element_Offset;
               Status : Status_Type;
            begin
               Convert_No_Check (
                  Object.Writing_Converter,
                  Buffer (Buffer_First .. Buffer_Last),
                  Taken,
                  Out_Buffer,
                  Out_Last,
                  Status);
               case Status is
                  when Fine =>
                     null;
                  when Insufficient =>
                     if Out_Last < Out_Buffer'First then
                        raise Constraint_Error; -- Out_Buffer is too smaller
                     end if;
                  when Incomplete =>
                     if Out_Last < Out_Buffer'First then
                        exit; -- wait tail-bytes
                     end if;
                  when Illegal_Sequence =>
                     Put_Substitute (
                        Object.Writing_Converter,
                        Out_Buffer,
                        Out_Last);
                     --  skip one element
                     Taken := Stream_Element_Offset'Min (
                        Buffer_Last,
                        Buffer_First
                           + Min_Size_In_From_Stream_Elements (
                              Object.Writing_Converter)
                           - 1);
               end case;
               --  write converted subsequence
               Ada.Streams.Write (
                  Object.Stream.all,
                  Out_Buffer (Out_Buffer'First .. Out_Last));
               --  drop converted subsequence
               Buffer_First := Taken + 1;
            end;
         end loop;
      end;
   end Write;

end System.Native_Encoding.Encoding_Streams;
