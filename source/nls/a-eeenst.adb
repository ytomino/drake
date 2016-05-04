package body Ada.Environment_Encoding.Encoding_Streams is
   use type Streams.Stream_Element;
   use type Streams.Stream_Element_Array;
   use type System.Address;

   subtype Stream_Element_Offset is Streams.Stream_Element_Offset;

   package Native
      renames System.Native_Environment_Encoding;

   procedure Adjust_Buffer (
      Buffer : in out Buffer_Type;
      First : in out Streams.Stream_Element_Offset;
      Last : in out Streams.Stream_Element_Offset);
   procedure Adjust_Buffer (
      Buffer : in out Buffer_Type;
      First : in out Streams.Stream_Element_Offset;
      Last : in out Streams.Stream_Element_Offset) is
   begin
      if First >= Buffer_Type'First + Half_Buffer_Length then
         --  shift
         declare
            New_Last : constant Stream_Element_Offset :=
               Buffer_Type'First + Last - First;
         begin
            Buffer (Buffer_Type'First .. New_Last) := Buffer (First .. Last);
            First := Buffer_Type'First;
            Last := New_Last;
         end;
      end if;
   end Adjust_Buffer;

   procedure Initialize (Context : in out Reading_Context_Type);
   procedure Initialize (Context : in out Reading_Context_Type) is
   begin
      Context.First := Buffer_Type'First;
      Context.Last := Buffer_Type'First - 1;
      Context.Converted_First := Buffer_Type'First;
      Context.Converted_Last := Buffer_Type'First - 1;
      Context.Status := Continuing;
   end Initialize;

   procedure Set_Substitute_To_Reading_Converter (
      Object : in out Converter;
      Substitute : Streams.Stream_Element_Array)
      renames Set_Substitute; -- inherited

   procedure Read (
      Stream : not null access Streams.Root_Stream_Type'Class;
      Item : out Streams.Stream_Element_Array;
      Last : out Streams.Stream_Element_Offset;
      Object : Converter;
      Context : in out Reading_Context_Type);
   procedure Read (
      Stream : not null access Streams.Root_Stream_Type'Class;
      Item : out Streams.Stream_Element_Array;
      Last : out Streams.Stream_Element_Offset;
      Object : Converter;
      Context : in out Reading_Context_Type)
   is
      I : Stream_Element_Offset := Item'First;
   begin
      if I > Item'Last then
         Last := I - 1;
      else
         loop
            --  filling
            if Context.Status = Continuing then
               Adjust_Buffer (Context.Buffer, Context.First, Context.Last);
               if Context.Last /= Buffer_Type'Last then
                  declare
                     Old_Context_Last : constant Stream_Element_Offset :=
                        Context.Last;
                  begin
                     Streams.Read (
                        Stream.all,
                        Context.Buffer (Context.Last + 1 .. Buffer_Type'Last),
                        Context.Last);
                     if Old_Context_Last = Context.Last then -- read zero
                        Context.Status := Finishing;
                     end if;
                  exception
                     when IO_Exceptions.End_Error =>
                        Context.Status := Finishing;
                  end;
               end if;
            end if;
            --  converting
            if Context.Status <= Finishing then
               Adjust_Buffer (
                  Context.Converted_Buffer,
                  Context.Converted_First,
                  Context.Converted_Last);
               --  try to convert subsequence
               declare
                  Taken : Stream_Element_Offset;
                  Status : Native.Subsequence_Status_Type;
               begin
                  Convert_No_Check (
                     Object,
                     Context.Buffer (Context.First .. Context.Last),
                     Taken,
                     Context.Converted_Buffer (
                        Context.Converted_Last + 1 .. Buffer_Type'Last),
                     Context.Converted_Last,
                     Finish => Context.Status > Continuing,
                     Status => Status);
                  case Status is
                     when Native.Finished =>
                        Context.Status := Ended;
                     when Native.Success =>
                        null;
                     when Native.Overflow =>
                        if Context.Converted_Last <
                           Context.Converted_First
                        then
                           raise Constraint_Error;
                           --  Converted_Buffer is too smaller
                        end if;
                     when Native.Truncated =>
                        pragma Assert (Context.Status = Continuing);
                        if Context.Converted_Last <
                           Context.Converted_First
                        then
                           if I = Stream_Element_Offset'First then
                              raise Constraint_Error; -- AARM 13.13.1(11/2)
                           end if;
                           Last := I - 1;
                           exit; -- wait tail-bytes
                        end if;
                     when Native.Illegal_Sequence =>
                        declare
                           Is_Overflow : Boolean;
                        begin
                           Put_Substitute (
                              Object,
                              Context.Converted_Buffer (
                                 Context.Converted_Last + 1 ..
                                 Buffer_Type'Last),
                              Context.Converted_Last,
                              Is_Overflow);
                           if Is_Overflow then
                              if I = Stream_Element_Offset'First then
                                 raise Constraint_Error; -- same as above
                              end if;
                              Last := I - 1;
                              exit; -- wait a next try
                           end if;
                        end;
                        --  skip one element
                        Taken := Stream_Element_Offset'Min (
                           Context.Last,
                           Context.First
                              + Min_Size_In_From_Stream_Elements (Object)
                              - 1);
                  end case;
                  --  drop converted subsequence
                  Context.First := Taken + 1;
               end;
            end if;
            --  copy converted elements
            if Context.Converted_First <= Context.Converted_Last then
               declare
                  Move_Length : constant Stream_Element_Offset :=
                     Stream_Element_Offset'Min (
                        Item'Last - I + 1,
                        Context.Converted_Last
                           - Context.Converted_First
                           + 1);
                  New_Last : constant Stream_Element_Offset :=
                     I + (Move_Length - 1);
                  New_Context_Converted_First : constant
                     Stream_Element_Offset :=
                     Context.Converted_First + Move_Length;
               begin
                  Item (I .. New_Last) :=
                     Context.Converted_Buffer (
                        Context.Converted_First ..
                        New_Context_Converted_First - 1);
                  Context.Converted_First := New_Context_Converted_First;
                  if New_Last >= Item'Last then
                     Last := New_Last;
                     exit;
                  end if;
                  I := New_Last + 1;
               end;
            end if;
            if Context.Converted_Last < Context.Converted_First then
               if Context.Status = Ended and then I <= Item'First then
                  if I = Stream_Element_Offset'First then
                     raise Constraint_Error; -- same as above
                  end if;
                  Last := I - 1;
                  exit;
               end if;
            end if;
         end loop;
      end if;
   end Read;

   procedure Initialize (Context : in out Writing_Context_Type);
   procedure Initialize (Context : in out Writing_Context_Type) is
   begin
      Context.First := Buffer_Type'First;
      Context.Last := Buffer_Type'First - 1;
   end Initialize;

   procedure Set_Substitute_To_Writing_Converter (
      Object : in out Converter;
      Substitute : Streams.Stream_Element_Array);
   procedure Set_Substitute_To_Writing_Converter (
      Object : in out Converter;
      Substitute : Streams.Stream_Element_Array)
   is
      Substitute_Last : Stream_Element_Offset := Substitute'First - 1;
      S2 : Streams.Stream_Element_Array (1 .. Max_Substitute_Length);
      S2_Last : Stream_Element_Offset := S2'First - 1;
   begin
      --  convert substitute from internal to external
      loop
         declare
            Status : Native.Substituting_Status_Type;
         begin
            Convert_No_Check (
               Object,
               Substitute (Substitute_Last + 1 .. Substitute'Last),
               Substitute_Last,
               S2 (S2_Last + 1 .. S2'Last),
               S2_Last,
               Finish => True,
               Status => Status);
            case Status is
               when Native.Finished =>
                  exit;
               when Native.Success =>
                  null;
               when Native.Overflow =>
                  raise Constraint_Error;
            end case;
         end;
      end loop;
      Set_Substitute (
         Object,
         S2 (1 .. S2_Last));
   end Set_Substitute_To_Writing_Converter;

   procedure Write (
      Stream : not null access Streams.Root_Stream_Type'Class;
      Item : Streams.Stream_Element_Array;
      Object : Converter;
      Context : in out Writing_Context_Type);
   procedure Write (
      Stream : not null access Streams.Root_Stream_Type'Class;
      Item : Streams.Stream_Element_Array;
      Object : Converter;
      Context : in out Writing_Context_Type)
   is
      I : Stream_Element_Offset := Item'First;
      End_Of_Item : Boolean := I > Item'Last;
   begin
      loop
         --  filling
         if not End_Of_Item then
            Adjust_Buffer (Context.Buffer, Context.First, Context.Last);
            if Context.Last /= Buffer_Type'Last then
               declare
                  Rest : constant Stream_Element_Offset :=
                     Stream_Element_Offset'Min (
                        Item'Last - I + 1,
                        Buffer_Type'Last - Context.Last);
                  New_Context_Last : constant Stream_Element_Offset :=
                     Context.Last + Rest;
                  New_Item_Last : constant Stream_Element_Offset :=
                     I + (Rest - 1);
               begin
                  Context.Buffer (Context.Last + 1 .. New_Context_Last) :=
                     Item (I .. New_Item_Last);
                  Context.Last := New_Context_Last;
                  End_Of_Item := New_Item_Last >= Item'Last;
                  if not End_Of_Item then
                     I := New_Item_Last + 1;
                  end if;
               end;
            end if;
         else
            exit when Context.Last < Context.First;
         end if;
         --  try to convert subsequence
         declare
            Taken : Stream_Element_Offset;
            Out_Buffer : Streams.Stream_Element_Array (0 .. 63);
            Out_Last : Stream_Element_Offset;
            Status : Native.Continuing_Status_Type;
         begin
            Convert_No_Check (
               Object,
               Context.Buffer (Context.First .. Context.Last),
               Taken,
               Out_Buffer,
               Out_Last,
               Status => Status);
            case Status is
               when Native.Success =>
                  null;
               when Native.Overflow =>
                  if Out_Last < Out_Buffer'First then
                     raise Constraint_Error; -- Out_Buffer is too smaller
                  end if;
               when Native.Truncated =>
                  if Out_Last < Out_Buffer'First then
                     exit; -- wait tail-bytes
                  end if;
               when Native.Illegal_Sequence =>
                  declare
                     Is_Overflow : Boolean;
                  begin
                     Put_Substitute (
                        Object,
                        Out_Buffer,
                        Out_Last,
                        Is_Overflow);
                     if Is_Overflow then
                        exit; -- wait a next try
                     end if;
                  end;
                  --  skip one element
                  Taken := Stream_Element_Offset'Min (
                     Context.Last,
                     Context.First
                        + Min_Size_In_From_Stream_Elements (Object)
                        - 1);
            end case;
            --  write converted subsequence
            Streams.Write (
               Stream.all,
               Out_Buffer (Out_Buffer'First .. Out_Last));
            --  drop converted subsequence
            Context.First := Taken + 1;
         end;
      end loop;
   end Write;

   procedure Finish (
      Stream : not null access Streams.Root_Stream_Type'Class;
      Object : Converter;
      Context : in out Writing_Context_Type);
   procedure Finish (
      Stream : not null access Streams.Root_Stream_Type'Class;
      Object : Converter;
      Context : in out Writing_Context_Type)
   is
      Out_Buffer : Streams.Stream_Element_Array (0 .. 63);
      Out_Last : Stream_Element_Offset := -1;
      Status : Native.Finishing_Status_Type;
   begin
      if Context.First <= Context.Last then
         --  put substitute instead of incomplete sequence in the buffer
         declare
            Is_Overflow : Boolean; -- ignore
         begin
            Put_Substitute (
               Object,
               Out_Buffer (Out_Last + 1 .. Out_Buffer'Last),
               Out_Last,
               Is_Overflow);
         end;
         Initialize (Context); -- reset indexes
      end if;
      --  finish
      loop
         Convert_No_Check (
            Object,
            Out_Buffer (Out_Last + 1 .. Out_Buffer'Last),
            Out_Last,
            Finish => True,
            Status => Status);
         Streams.Write (
            Stream.all,
            Out_Buffer (Out_Buffer'First .. Out_Last));
         case Status is
            when Native.Finished =>
               exit;
            when Native.Success =>
               null;
            when Native.Overflow =>
               if Out_Last < Out_Buffer'First then
                  raise Constraint_Error; -- Out_Buffer is too smaller
               end if;
         end case;
      end loop;
   end Finish;

   --  implementation of only reading

   function Is_Open (Object : In_Type) return Boolean is
   begin
      return Object.Stream /= System.Null_Address;
   end Is_Open;

   function Open (
      Decoder : Converter;
      Stream : not null access Streams.Root_Stream_Type'Class)
      return In_Type
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Open (Decoder) or else raise Status_Error);
      function To_Address (Value : access Streams.Root_Stream_Type'Class)
         return System.Address
         with Import, Convention => Intrinsic;
   begin
      return Result : In_Type do
         Result.Stream := To_Address (Stream);
         Result.Reading_Converter := Decoder'Unrestricted_Access;
         Initialize (Result.Reading_Context);
      end return;
   end Open;

   function Stream (
      Object : aliased in out In_Type)
      return not null access Streams.Root_Stream_Type'Class
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Open (Object) or else raise Status_Error);
   begin
      return Object'Unchecked_Access;
   end Stream;

   overriding procedure Read (
      Object : in out In_Type;
      Item : out Streams.Stream_Element_Array;
      Last : out Streams.Stream_Element_Offset)
   is
      function To_Pointer (Value : System.Address)
         return access Streams.Root_Stream_Type'Class
         with Import, Convention => Intrinsic;
   begin
      Read (
         To_Pointer (Object.Stream),
         Item,
         Last,
         Object.Reading_Converter.all,
         Object.Reading_Context);
   end Read;

   --  implementation of only writing

   function Is_Open (Object : Out_Type) return Boolean is
   begin
      return Object.Stream /= System.Null_Address;
   end Is_Open;

   function Open (
      Encoder : Converter;
      Stream : not null access Streams.Root_Stream_Type'Class)
      return Out_Type
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Open (Encoder) or else raise Status_Error);
      function To_Address (Value : access Streams.Root_Stream_Type'Class)
         return System.Address
         with Import, Convention => Intrinsic;
   begin
      return Result : Out_Type do
         Result.Stream := To_Address (Stream);
         Result.Writing_Converter := Encoder'Unrestricted_Access;
         Initialize (Result.Writing_Context);
      end return;
   end Open;

   function Stream (
      Object : aliased in out Out_Type)
      return not null access Streams.Root_Stream_Type'Class
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Open (Object) or else raise Status_Error);
   begin
      return Object'Unchecked_Access;
   end Stream;

   procedure Finish (
      Object : in out Out_Type)
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Open (Object) or else raise Status_Error);
      function To_Pointer (Value : System.Address)
         return access Streams.Root_Stream_Type'Class
         with Import, Convention => Intrinsic;
   begin
      Finish (
         To_Pointer (Object.Stream),
         Object.Writing_Converter.all,
         Object.Writing_Context);
   end Finish;

   overriding procedure Write (
      Object : in out Out_Type;
      Item : Streams.Stream_Element_Array)
   is
      function To_Pointer (Value : System.Address)
         return access Streams.Root_Stream_Type'Class
         with Import, Convention => Intrinsic;
   begin
      Write (
         To_Pointer (Object.Stream),
         Item,
         Object.Writing_Converter.all,
         Object.Writing_Context);
   end Write;

   --  implementation of bidirectional

   function Is_Open (Object : Inout_Type) return Boolean is
   begin
      return Object.Stream /= System.Null_Address;
   end Is_Open;

   function Open (
      Internal : Encoding_Id;
      External : Encoding_Id;
      Stream : not null access Streams.Root_Stream_Type'Class)
      return Inout_Type
   is
      function To_Address (Value : access Streams.Root_Stream_Type'Class)
         return System.Address
         with Import, Convention => Intrinsic;
   begin
      return Result : Inout_Type do
         Result.Internal := Internal;
         Result.External := External;
         Result.Stream := To_Address (Stream);
         Result.Substitute_Length := -1;
         Initialize (Result.Reading_Context);
         Initialize (Result.Writing_Context);
      end return;
   end Open;

   function Substitute (
      Object : Inout_Type)
      return Streams.Stream_Element_Array
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Open (Object) or else raise Status_Error);
   begin
      if Object.Substitute_Length < 0 then
         return Default_Substitute (Object.Internal);
      else
         return Object.Substitute (1 .. Object.Substitute_Length);
      end if;
   end Substitute;

   procedure Set_Substitute (
      Object : in out Inout_Type;
      Substitute : Streams.Stream_Element_Array)
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Open (Object) or else raise Status_Error);
   begin
      if Substitute'Length > Object.Substitute'Length then
         raise Constraint_Error;
      end if;
      Object.Substitute_Length := Substitute'Length;
      Object.Substitute (1 .. Object.Substitute_Length) := Substitute;
      --  set to converters
      if Is_Open (Object.Reading_Converter) then
         Set_Substitute_To_Reading_Converter (
            Object.Reading_Converter,
            Object.Substitute (1 .. Object.Substitute_Length));
      end if;
      if Is_Open (Object.Writing_Converter) then
         Set_Substitute_To_Writing_Converter (
            Object.Writing_Converter,
            Object.Substitute (1 .. Object.Substitute_Length));
      end if;
   end Set_Substitute;

   function Stream (
      Object : aliased in out Inout_Type)
      return not null access Streams.Root_Stream_Type'Class
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Open (Object) or else raise Status_Error);
   begin
      return Object'Unchecked_Access;
   end Stream;

   procedure Finish (
      Object : in out Inout_Type)
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Open (Object) or else raise Status_Error);
      function To_Pointer (Value : System.Address)
         return access Streams.Root_Stream_Type'Class
         with Import, Convention => Intrinsic;
   begin
      if Is_Open (Object.Writing_Converter) then
         Finish (
            To_Pointer (Object.Stream),
            Object.Writing_Converter,
            Object.Writing_Context);
      end if;
   end Finish;

   overriding procedure Read (
      Object : in out Inout_Type;
      Item : out Streams.Stream_Element_Array;
      Last : out Streams.Stream_Element_Offset)
   is
      function To_Pointer (Value : System.Address)
         return access Streams.Root_Stream_Type'Class
         with Import, Convention => Intrinsic;
   begin
      if not Is_Open (Object.Reading_Converter) then
         Open (
            Object.Reading_Converter,
            From => Native.Encoding_Id (Object.External),
            To => Native.Encoding_Id (Object.Internal));
         if Object.Substitute_Length >= 0 then
            Set_Substitute_To_Reading_Converter (
               Object.Reading_Converter,
               Object.Substitute (1 .. Object.Substitute_Length));
         end if;
      end if;
      Read (
         To_Pointer (Object.Stream),
         Item,
         Last,
         Object.Reading_Converter,
         Object.Reading_Context);
   end Read;

   overriding procedure Write (
      Object : in out Inout_Type;
      Item : Streams.Stream_Element_Array)
   is
      function To_Pointer (Value : System.Address)
         return access Streams.Root_Stream_Type'Class
         with Import, Convention => Intrinsic;
   begin
      if not Is_Open (Object.Writing_Converter) then
         Open (
            Object.Writing_Converter,
            From => Native.Encoding_Id (Object.Internal),
            To => Native.Encoding_Id (Object.External));
         if Object.Substitute_Length >= 0 then
            Set_Substitute_To_Writing_Converter (
               Object.Writing_Converter,
               Object.Substitute (1 .. Object.Substitute_Length));
         end if;
      end if;
      Write (
         To_Pointer (Object.Stream),
         Item,
         Object.Writing_Converter,
         Object.Writing_Context);
   end Write;

end Ada.Environment_Encoding.Encoding_Streams;
