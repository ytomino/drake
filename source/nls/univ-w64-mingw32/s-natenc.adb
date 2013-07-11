pragma Check_Policy (Trace, Off);
with Ada.Exceptions;
with System.UTF_Conversions;
with System.UTF_Conversions.From_32_To_16;
with System.UTF_Conversions.From_16_To_32;
with C.winbase;
with C.winerror;
with C.winnt;
package body System.Native_Encoding is
   use type Ada.Streams.Stream_Element_Offset;
   use type C.windef.WINBOOL;

   function Is_Open (Object : Converter) return Boolean is
   begin
      return Object.From /= Invalid_Encoding_Id;
   end Is_Open;

   procedure Convert (
      Object : Converter;
      Item : Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset;
      Out_Item : out Ada.Streams.Stream_Element_Array;
      Out_Last : out Ada.Streams.Stream_Element_Offset;
      Status : out Error_Status) is
   begin
      if not Is_Open (Object) then
         Ada.Exceptions.Raise_Exception_From_Here (Status_Error'Identity);
      end if;
      Convert_No_Check (Object, Item, Last, Out_Item, Out_Last, Status);
   end Convert;

   procedure Convert (
      Object : Converter;
      Item : Ada.Streams.Stream_Element_Array;
      Out_Item : out Ada.Streams.Stream_Element_Array;
      Out_Last : out Ada.Streams.Stream_Element_Offset;
      Substitute : Ada.Streams.Stream_Element := Default_Substitute) is
   begin
      if not Is_Open (Object) then
         Ada.Exceptions.Raise_Exception_From_Here (Status_Error'Identity);
      end if;
      Convert_No_Check (Object, Item, Out_Item, Out_Last, Substitute);
   end Convert;

   procedure Convert_No_Check (
      Object : Converter;
      Item : Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset;
      Out_Item : out Ada.Streams.Stream_Element_Array;
      Out_Last : out Ada.Streams.Stream_Element_Offset;
      Status : out Error_Status)
   is
      Buffer : aliased C.winnt.WCHAR_array (1 .. 2);
      Buffer_As_W : aliased Wide_String (1 .. 2);
      for Buffer_As_W'Address use Buffer'Address;
      Buffer_As_SEA : aliased Ada.Streams.Stream_Element_Array (1 .. 4);
      for Buffer_As_SEA'Address use Buffer'Address;
      Buffer_Length : C.signed_int;
   begin
      pragma Check (Trace, Ada.Debug.Put ("enter"));
      case Object.From is
         when UTF_8 =>
            if Item'Length = 0 then
               Last := Item'First - 1;
               Out_Last := Out_Item'First - 1;
               Status := Incomplete;
               pragma Check (Trace, Ada.Debug.Put ("incomplete"));
               return;
            else
               declare
                  Item_As_S : aliased String (1 .. Item'Length);
                  for Item_As_S'Address use Item'Address;
                  Item_As_C : aliased C.char_array (1 .. Item'Length);
                  for Item_As_C'Address use Item'Address;
                  Item_Length : C.signed_int;
                  Dummy_Code : UTF_Conversions.UCS_4;
                  Error : Boolean;
               begin
                  UTF_Conversions.From_UTF_8 (
                     Item_As_S,
                     Integer (Item_Length),
                     Dummy_Code,
                     Error => Error);
                  if Error then
                     Last := Item'First - 1;
                     Out_Last := Out_Item'First - 1;
                     Status := Illegal_Sequence;
                     pragma Check (Trace, Ada.Debug.Put ("illegal sequence"));
                     return;
                  end if;
                  Buffer_Length := C.winnls.MultiByteToWideChar (
                     C.windef.UINT (Object.From),
                     C.winnls.MB_ERR_INVALID_CHARS,
                     Item_As_C (1)'Access,
                     Item_Length,
                     Buffer (1)'Access,
                     Buffer'Length);
                  if Buffer_Length = 0 then
                     Last := Item'First - 1;
                     Out_Last := Out_Item'First - 1;
                     Status := Illegal_Sequence;
                     pragma Check (Trace, Ada.Debug.Put ("illegal sequence"));
                     return;
                  end if;
                  Last := Item'First
                     + Ada.Streams.Stream_Element_Offset (Item_Length)
                     - 1;
               end;
            end if;
         when UTF_16 =>
            if Item'Length < 2 then
               Last := Item'First - 1;
               Out_Last := Out_Item'First - 1;
               Status := Incomplete;
               pragma Check (Trace, Ada.Debug.Put ("imcomplete"));
               return;
            else
               if Item'Length < 4 then
                  Last := Item'First + 1;
               else
                  Last := Item'First + 3;
               end if;
               Buffer_As_SEA (1 .. Last - Item'First + 1) :=
                  Item (Item'First .. Last);
               declare
                  Dummy_Code : UTF_Conversions.UCS_4;
                  Error : Boolean;
               begin
                  UTF_Conversions.From_UTF_16 (
                     Buffer_As_W,
                     Integer (Buffer_Length),
                     Dummy_Code,
                     Error => Error);
                  if Error then
                     Last := Item'First - 1;
                     Out_Last := Out_Item'First - 1;
                     Status := Illegal_Sequence;
                     pragma Check (Trace, Ada.Debug.Put ("illegal sequence"));
                     return;
                  end if;
               end;
               Last := 2 * Ada.Streams.Stream_Element_Offset (Buffer_Length);
            end if;
         when UTF_32 =>
            if Item'Length < 4 then
               Last := Item'First - 1;
               Out_Last := Out_Item'First - 1;
               Status := Incomplete;
               pragma Check (Trace, Ada.Debug.Put ("incomplete"));
               return;
            else
               declare
                  Code : UTF_Conversions.UCS_4;
                  Code_As_SEA : Ada.Streams.Stream_Element_Array (1 .. 4);
                  for Code_As_SEA'Address use Code'Address;
                  Error : Boolean;
               begin
                  Last := Item'First + 3;
                  Code_As_SEA := Item (Item'First .. Last);
                  UTF_Conversions.To_UTF_16 (
                     Code,
                     Buffer_As_W,
                     Integer (Buffer_Length),
                     Error => Error);
                  if Error then
                     Last := Item'First - 1;
                     Out_Last := Out_Item'First - 1;
                     Status := Illegal_Sequence;
                     pragma Check (Trace, Ada.Debug.Put ("illegal sequence"));
                     return;
                  end if;
               end;
            end if;
         when others =>
            if Item'Length = 0 then
               Last := Item'First - 1;
               Out_Last := Out_Item'First - 1;
               Status := Incomplete;
               pragma Check (Trace, Ada.Debug.Put ("incomplete"));
               return;
            else
               declare
                  Item_As_C : aliased C.char_array (1 .. Item'Length);
                  for Item_As_C'Address use Item'Address;
                  Item_Length : C.signed_int;
               begin
                  if C.winnls.IsDBCSLeadByteEx (
                     C.windef.UINT (Object.From),
                     C.char'Pos (Item_As_C (1))) /= 0
                  then
                     Item_Length := 2;
                  else
                     Item_Length := 1;
                  end if;
                  Last := Item'First
                     + Ada.Streams.Stream_Element_Offset (Item_Length)
                     - 1;
                  Buffer_Length := C.winnls.MultiByteToWideChar (
                     C.windef.UINT (Object.From),
                     C.winnls.MB_ERR_INVALID_CHARS,
                     Item_As_C (1)'Access,
                     Item_Length,
                     Buffer (1)'Access,
                     Buffer'Length);
               end;
            end if;
      end case;
      pragma Check (Trace, Ada.Debug.Put ("Item'First =" & Item'First'Img));
      pragma Check (Trace, Ada.Debug.Put ("Last =" & Last'Img));
      case Object.To is
         when UTF_16 =>
            declare
               Buffer_As_SEA_Length : constant
                  Ada.Streams.Stream_Element_Offset :=
                     2 * Ada.Streams.Stream_Element_Offset (Buffer_Length);
            begin
               if Out_Item'Length < Buffer_As_SEA_Length then
                  Last := Item'First - 1;
                  Out_Last := Out_Item'First - 1;
                  Status := Incomplete;
                  pragma Check (Trace, Ada.Debug.Put ("incomplete"));
                  return;
               else
                  Out_Last := Out_Item'First + Buffer_As_SEA_Length - 1;
                  Out_Item (Out_Item'First .. Out_Last) :=
                     Buffer_As_SEA (1 .. Buffer_As_SEA_Length);
                  Status := Fine;
               end if;
            end;
         when UTF_32 =>
            if Out_Item'Length < 4 then
               Last := Item'First - 1;
               Out_Last := Out_Item'First - 1;
               Status := Incomplete;
               pragma Check (Trace, Ada.Debug.Put ("incomplete"));
               return;
            else
               declare
                  Out_Code : UTF_Conversions.UCS_4;
                  Out_Code_As_SEA : Ada.Streams.Stream_Element_Array (1 .. 4);
                  for Out_Code_As_SEA'Address use Out_Code'Address;
                  Buffer_Used : Natural;
                  Error : Boolean;
               begin
                  UTF_Conversions.From_UTF_16 (
                     Buffer_As_W (1 .. Integer (Buffer_Length)),
                     Buffer_Used,
                     Out_Code,
                     Error => Error);
                  if Error or else Buffer_Used /= Integer (Buffer_Length) then
                     Last := Item'First - 1;
                     Out_Last := Out_Item'First - 1;
                     Status := Illegal_Sequence;
                     pragma Check (Trace, Ada.Debug.Put ("illegal sequence"));
                     return;
                  end if;
                  Out_Last := Out_Item'First + 3;
                  Out_Item (Out_Item'First .. Out_Last) := Out_Code_As_SEA;
                  Status := Fine;
               end;
            end if;
         when others => -- including UTF_8
            declare
               Out_Item_As_C : aliased C.char_array (1 .. Out_Item'Length);
               for Out_Item_As_C'Address use Out_Item'Address;
               Out_Length : C.signed_int;
            begin
               Out_Length := C.winnls.WideCharToMultiByte (
                  C.windef.UINT (Object.To),
                  0, -- MB_ERR_INVALID_CHARS ?
                  Buffer (1)'Access,
                  Buffer_Length,
                  Out_Item_As_C (1)'Access,
                  Out_Item_As_C'Length,
                  null,
                  null);
               if Out_Length = 0 then
                  Last := Item'First - 1;
                  Out_Last := Out_Item'First - 1;
                  case C.winbase.GetLastError is
                     when C.winerror.ERROR_INSUFFICIENT_BUFFER =>
                        Status := Incomplete;
                        pragma Check (Trace, Ada.Debug.Put ("incomplete"));
                     when others =>
                        Status := Illegal_Sequence;
                        pragma Check (Trace,
                           Ada.Debug.Put ("illegal sequence"));
                  end case;
                  return;
               end if;
               Out_Last := Out_Item'First
                  + Ada.Streams.Stream_Element_Offset (Out_Length)
                  - 1;
               Status := Fine;
            end;
      end case;
      pragma Check (Trace,
         Ada.Debug.Put ("Out_Item'First =" & Out_Item'First'Img));
      pragma Check (Trace, Ada.Debug.Put ("Out_Last =" & Out_Last'Img));
      pragma Check (Trace, Ada.Debug.Put ("leave"));
   end Convert_No_Check;

   procedure Convert_No_Check (
      Object : Converter;
      Item : Ada.Streams.Stream_Element_Array;
      Out_Item : out Ada.Streams.Stream_Element_Array;
      Out_Last : out Ada.Streams.Stream_Element_Offset;
      Substitute : Ada.Streams.Stream_Element := Default_Substitute)
   is
      Item_Length : constant Ada.Streams.Stream_Element_Offset := Item'Length;
      Buffer : aliased C.winnt.WCHAR_array (1 .. C.size_t (Item_Length));
      Buffer_As_W : aliased Wide_String (1 .. Natural (Item_Length));
      for Buffer_As_W'Address use Buffer'Address;
      Buffer_As_SEA : aliased
         Ada.Streams.Stream_Element_Array (1 .. 2 * Item_Length);
      for Buffer_As_SEA'Address use Buffer'Address;
      Buffer_Length : C.signed_int;
   begin
      case Object.From is
         when UTF_16 =>
            Buffer_As_SEA (1 .. Item_Length) := Item;
            Buffer_Length := C.signed_int (Item_Length / 2);
            if Item_Length rem 2 /= 0 then
               Buffer_Length := Buffer_Length + 1;
               Buffer (C.size_t (Buffer_Length)) :=
                  C.winnt.WCHAR'Val (Substitute);
            end if;
         when UTF_32 =>
            declare
               WW : Wide_Wide_String (1 .. Natural (Item_Length / 4));
               WW_As_SEA : Ada.Streams.Stream_Element_Array (1 .. Item_Length);
               for WW_As_SEA'Address use WW'Address;
            begin
               WW_As_SEA := Item; -- alignment
               UTF_Conversions.From_32_To_16.Convert (
                  WW,
                  Buffer_As_W,
                  Natural (Buffer_Length),
                  Substitute => Wide_Character'Val (Substitute));
            end;
            if Item_Length rem 4 /= 0 then
               Buffer_Length := Buffer_Length + 1;
               Buffer (C.size_t (Buffer_Length)) :=
                  C.winnt.WCHAR'Val (Substitute);
            end if;
         when others => -- including UTF_8
            declare
               Item_As_C : aliased C.char_array (1 .. C.size_t (Item_Length));
               for Item_As_C'Address use Item'Address;
            begin
               Buffer_Length := C.winnls.MultiByteToWideChar (
                  C.windef.UINT (Object.From),
                  0, -- no C.winnls.MB_ERR_INVALID_CHARS for converting all
                  Item_As_C (1)'Access,
                  C.signed_int (Item_Length),
                  Buffer (1)'Access,
                  Buffer'Length);
            end;
      end case;
      case Object.To is
         when UTF_16 =>
            declare
               Buffer_Length_In_SEA : constant
                  Ada.Streams.Stream_Element_Offset :=
                     2 * Ada.Streams.Stream_Element_Offset (Buffer_Length);
            begin
               Out_Last := Out_Item'First + Buffer_Length_In_SEA - 1;
               if Out_Last > Out_Item'Length then
                  raise Constraint_Error; -- overflow
               end if;
               Out_Item (Out_Item'First .. Out_Last) :=
                  Buffer_As_SEA (1 .. Buffer_Length_In_SEA);
            end;
         when UTF_32 =>
            declare
               Out_WW : Wide_Wide_String (1 .. Natural (Buffer_Length));
               Out_WW_As_SEA : Ada.Streams.Stream_Element_Array (
                  1 ..
                  4 * Ada.Streams.Stream_Element_Offset (Buffer_Length));
               for Out_WW_As_SEA'Address use Out_WW'Address;
               Out_WW_Length : Natural;
            begin
               UTF_Conversions.From_16_To_32.Convert (
                  Buffer_As_W,
                  Out_WW,
                  Out_WW_Length,
                  Substitute => Wide_Wide_Character'Val (Substitute));
               declare
                  Out_WW_Length_In_SEA : constant
                     Ada.Streams.Stream_Element_Offset :=
                        4 * Ada.Streams.Stream_Element_Offset (Out_WW_Length);
               begin
                  Out_Last := Out_Item'First + Out_WW_Length_In_SEA - 1;
                  if Out_Last > Out_Item'Last then
                     raise Constraint_Error; -- overflow
                  end if;
                  Out_Item (Out_Item'First .. Out_Last) :=
                     Out_WW_As_SEA (1 .. Out_WW_Length_In_SEA); -- un-alignment
               end;
            end;
         when others => -- including UTF_8
            declare
               Out_Item_As_C : aliased C.char_array (1 .. Out_Item'Length);
               for Out_Item_As_C'Address use Out_Item'Address;
               Out_Length : C.signed_int;
               Substitute_C : aliased C.char_array (1 .. 2);
               Substitute_P : C.char_const_ptr;
            begin
               if Object.To = UTF_8 then
                  Substitute_P := null;
               else
                  Substitute_C (1) := C.char'Val (Substitute);
                  Substitute_C (2) := C.char'Val (0);
                  Substitute_P := Substitute_C (1)'Unchecked_Access;
               end if;
               Out_Length := C.winnls.WideCharToMultiByte (
                  C.windef.UINT (Object.To),
                  0, -- no C.winnls.MB_ERR_INVALID_CHARS for converting all
                  Buffer (1)'Access,
                  Buffer_Length,
                  Out_Item_As_C (1)'Access,
                  Out_Item_As_C'Length,
                  Substitute_P,
                  null);
               if Out_Length = 0 and then Item_Length /= 0 then
                  raise Constraint_Error; -- overflow
               end if;
               Out_Last := Out_Item'First
                  + Ada.Streams.Stream_Element_Offset (Out_Length)
                  - 1;
            end;
      end case;
   end Convert_No_Check;

   procedure Open (Object : out Converter; From, To : Encoding_Id) is
   begin
      Object.From := From;
      Object.To := To;
   end Open;

end System.Native_Encoding;
