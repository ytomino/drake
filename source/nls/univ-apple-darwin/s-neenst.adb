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
         Result.Reading_Last := -1;
         Result.Writing_Last := -1;
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
               Out_Buffer : Ada.Streams.Stream_Element_Array (0 .. 63);
               Out_Last : Stream_Element_Offset;
               Status : Status_Type;
            begin
               Convert_No_Check (
                  Object.Reading_Converter,
                  Buffer (Buffer'First .. Buffer_Last),
                  Taken,
                  Out_Buffer,
                  Out_Last,
                  Status);
               case Status is
                  when Fine =>
                     null;
                  when Insufficient =>
                     raise Constraint_Error; -- Out_Buffer is too smaller
                  when Incomplete =>
                     exit; -- wait tail-bytes
                  when Illegal_Sequence =>
                     Put_Substitute (
                        Object.Reading_Converter,
                        Out_Buffer,
                        Out_Last);
                     --  skip one element
                     Taken := Stream_Element_Offset'Min (
                        Buffer'Last,
                        Buffer'First
                           + Min_Size_In_From_Stream_Elements (
                              Object.Writing_Converter)
                           - 1);
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
         if Object.Substitute_Length >= 0 then
            Set_Substitute_To_Writing_Converter (Object);
         end if;
      end if;
      declare
         Item_Last : Stream_Element_Offset := Item'First - 1;
         Buffer : Ada.Streams.Stream_Element_Array
            renames Object.Writing_Buffer;
         Buffer_Last : Stream_Element_Offset
            renames Object.Writing_Last;
      begin
         loop
            --  filling
            if Item_Last /= Item'Last then
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
            else
               exit when Buffer_Last < Buffer'First;
            end if;
            --  try to convert one multi-byte character
            declare
               Taken : Stream_Element_Offset;
               Out_Buffer : Ada.Streams.Stream_Element_Array (0 .. 63);
               Out_Last : Stream_Element_Offset;
               Status : Status_Type;
            begin
               Convert_No_Check (
                  Object.Writing_Converter,
                  Buffer (Buffer'First .. Buffer_Last),
                  Taken,
                  Out_Buffer,
                  Out_Last,
                  Status);
               case Status is
                  when Fine =>
                     null;
                  when Insufficient =>
                     raise Constraint_Error; -- Out_Buffer is too smaller
                  when Incomplete =>
                     exit; -- wait tail-bytes
                  when Illegal_Sequence =>
                     Put_Substitute (
                        Object.Writing_Converter,
                        Out_Buffer,
                        Out_Last);
                     --  skip one element
                     Taken := Stream_Element_Offset'Min (
                        Buffer'Last,
                        Buffer'First
                           + Min_Size_In_From_Stream_Elements (
                              Object.Writing_Converter)
                           - 1);
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
