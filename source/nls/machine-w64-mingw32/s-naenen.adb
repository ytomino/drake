pragma Check_Policy (Trace => Ignore);
with Ada.Exception_Identification.From_Here;
with System.Address_To_Constant_Access_Conversions;
with System.UTF_Conversions.From_16_To_32;
with System.UTF_Conversions.From_32_To_16;
with System.Zero_Terminated_WStrings;
with C.winbase;
with C.winerror;
with C.winnt;
package body System.Native_Environment_Encoding is
   use Ada.Exception_Identification.From_Here;
   use type UTF_Conversions.From_Status_Type;
   use type UTF_Conversions.To_Status_Type;
   use type C.signed_int; -- C.windef.WINBOOL

   package LPCSTR_Conv is
      new Address_To_Constant_Access_Conversions (C.char, C.winnt.LPCSTR);

   procedure Default_Substitute (
      Encoding : Encoding_Id;
      Item : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset;
      Is_Overflow : out Boolean);
   procedure Default_Substitute (
      Encoding : Encoding_Id;
      Item : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset;
      Is_Overflow : out Boolean) is
   begin
      case Encoding is
         when UTF_16 =>
            Last := Item'First - 1;
            Is_Overflow := Item'First + 1 > Item'Last;
            if not Is_Overflow then
               case Default_Bit_Order is
                  when High_Order_First =>
                     Last := Last + 1;
                     Item (Last) := 0;
                     Last := Last + 1;
                     Item (Last) := Character'Pos ('?');
                  when Low_Order_First =>
                     Last := Last + 1;
                     Item (Last) := Character'Pos ('?');
                     Last := Last + 1;
                     Item (Last) := 0;
               end case;
            end if;
         when UTF_32 =>
            Last := Item'First - 1;
            Is_Overflow := Item'First + 3 > Item'Last;
            if not Is_Overflow then
               case Default_Bit_Order is
                  when High_Order_First =>
                     Last := Last + 1;
                     Item (Last) := 0;
                     Last := Last + 1;
                     Item (Last) := 0;
                     Last := Last + 1;
                     Item (Last) := 0;
                     Last := Last + 1;
                     Item (Last) := Character'Pos ('?');
                  when Low_Order_First =>
                     Last := Last + 1;
                     Item (Last) := Character'Pos ('?');
                     Last := Last + 1;
                     Item (Last) := 0;
                     Last := Last + 1;
                     Item (Last) := 0;
                     Last := Last + 1;
                     Item (Last) := 0;
               end case;
            end if;
         when others =>
            Last := Item'First - 1;
            Is_Overflow := Item'First > Item'Last;
            if not Is_Overflow then
               Last := Last + 1;
               Item (Last) := Character'Pos ('?');
            end if;
      end case;
   end Default_Substitute;

   --  implementation

   function Get_Image (Encoding : Encoding_Id) return String is
      Info : aliased C.winnls.CPINFOEX;
   begin
      if C.winnls.GetCPInfoEx (C.windef.UINT (Encoding), 0, Info'Access) =
         C.windef.FALSE
      then
         Raise_Exception (Use_Error'Identity); -- ?
      end if;
      return Zero_Terminated_WStrings.Value (Info.CodePageName (0)'Access);
   end Get_Image;

   function Get_Default_Substitute (Encoding : Encoding_Id)
      return Ada.Streams.Stream_Element_Array
   is
      Result : Ada.Streams.Stream_Element_Array (
         0 .. -- from 0 for a result value
         Max_Substitute_Length - 1);
      Last : Ada.Streams.Stream_Element_Offset;
      Is_Overflow : Boolean;
   begin
      Default_Substitute (Encoding, Result, Last, Is_Overflow);
      pragma Assert (not Is_Overflow);
      return Result (Result'First .. Last);
   end Get_Default_Substitute;

   function Get_Min_Size_In_Stream_Elements (Encoding : Encoding_Id)
      return Ada.Streams.Stream_Element_Offset is
   begin
      case Encoding is
         when UTF_16 =>
            return 2;
         when UTF_32 =>
            return 4;
         when others =>
            return 1;
      end case;
   end Get_Min_Size_In_Stream_Elements;

   function Get_Current_Encoding return Encoding_Id is
   begin
      return Encoding_Id (C.winnls.GetACP);
   end Get_Current_Encoding;

   procedure Open (Object : out Converter; From, To : Encoding_Id) is
   begin
      Object.From := From;
      Object.To := To;
      Object.Substitute_Length := -1;
   end Open;

   function Is_Open (Object : Converter) return Boolean is
   begin
      return Object.From /= Invalid_Encoding_Id;
   end Is_Open;

   function Min_Size_In_From_Stream_Elements_No_Check (Object : Converter)
      return Ada.Streams.Stream_Element_Offset is
   begin
      return Get_Min_Size_In_Stream_Elements (Object.From);
   end Min_Size_In_From_Stream_Elements_No_Check;

   function Substitute_No_Check (Object : Converter)
      return Ada.Streams.Stream_Element_Array is
   begin
      if Object.Substitute_Length < 0 then
         return Get_Default_Substitute (Object.To);
      else
         return Object.Substitute (1 .. Object.Substitute_Length);
      end if;
   end Substitute_No_Check;

   procedure Set_Substitute_No_Check (
      Object : in out Converter;
      Substitute : Ada.Streams.Stream_Element_Array) is
   begin
      if Substitute'Length > Object.Substitute'Length - 1 then
         raise Constraint_Error;
      end if;
      Object.Substitute_Length := Substitute'Length;
      Object.Substitute (1 .. Object.Substitute_Length) := Substitute;
      Object.Substitute (Object.Substitute_Length + 1) := 0; -- zero terminator
   end Set_Substitute_No_Check;

   procedure Convert_No_Check (
      Object : Converter;
      Item : Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset;
      Out_Item : out Ada.Streams.Stream_Element_Array;
      Out_Last : out Ada.Streams.Stream_Element_Offset;
      Finish : Boolean;
      Status : out Subsequence_Status_Type)
   is
      Buffer : aliased Wide_String (1 .. 2);
      Buffer_As_C : aliased C.winnt.WCHAR_array (0 .. 1);
      for Buffer_As_C'Address use Buffer'Address;
      Buffer_As_SEA : aliased Ada.Streams.Stream_Element_Array (1 .. 4);
      for Buffer_As_SEA'Address use Buffer'Address;
      Buffer_Length : C.signed_int;
   begin
      pragma Check (Trace, Ada.Debug.Put ("enter"));
      if Item'First > Item'Last then
         Last := Item'First - 1;
         Out_Last := Out_Item'First - 1;
      else
         case Object.From is
            when UTF_8 =>
               declare
                  Length : C.signed_int;
                  From_Status : UTF_Conversions.From_Status_Type;
               begin
                  declare
                     Item_As_String : aliased String (1 .. Item'Length);
                     for Item_As_String'Address use Item'Address;
                     Dummy_Code : UTF_Conversions.UCS_4;
                  begin
                     UTF_Conversions.From_UTF_8 (
                        Item_As_String,
                        Integer (Length),
                        Dummy_Code,
                        From_Status);
                  end;
                  case From_Status is
                     when UTF_Conversions.Success =>
                        null;
                     when UTF_Conversions.Illegal_Sequence
                        | UTF_Conversions.Non_Shortest =>
                        Last := Item'First - 1;
                        Out_Last := Out_Item'First - 1;
                        Status := Illegal_Sequence;
                        pragma Check (Trace,
                           Check => Ada.Debug.Put ("illegal sequence"));
                        return; -- error
                     when UTF_Conversions.Truncated =>
                        Last := Item'First - 1;
                        Out_Last := Out_Item'First - 1;
                        Status := Truncated;
                        pragma Check (Trace, Ada.Debug.Put ("truncated"));
                        return; -- error
                  end case;
                  declare
                     Item_As_C : aliased C.char_array (0 .. 0); -- at least
                     for Item_As_C'Address use Item'Address;
                  begin
                     Buffer_Length := C.winnls.MultiByteToWideChar (
                        C.windef.UINT (Object.From),
                        C.winnls.MB_ERR_INVALID_CHARS,
                        Item_As_C (0)'Access,
                        Length,
                        Buffer_As_C (0)'Access,
                        Buffer'Length);
                  end;
                  if Buffer_Length = 0 then
                     Last := Item'First - 1;
                     Out_Last := Out_Item'First - 1;
                     Status := Illegal_Sequence;
                     pragma Check (Trace, Ada.Debug.Put ("illegal sequence"));
                     return; -- error
                  end if;
                  Last :=
                     Item'First
                     + (Ada.Streams.Stream_Element_Offset (Length) - 1);
               end;
            when UTF_16 =>
               if Item'First + 1 > Item'Last then
                  Last := Item'First - 1;
                  Out_Last := Out_Item'First - 1;
                  Status := Truncated;
                  pragma Check (Trace, Ada.Debug.Put ("truncated"));
                  return; -- error
               end if;
               if Item'First + 3 > Item'Last then
                  Last := Item'First + 1;
               else
                  Last := Item'First + 3;
               end if;
               Buffer_As_SEA (1 .. Last - Item'First + 1) :=
                  Item (Item'First .. Last);
               declare
                  Dummy_Code : UTF_Conversions.UCS_4;
                  From_Status : UTF_Conversions.From_Status_Type;
               begin
                  UTF_Conversions.From_UTF_16 (
                     Buffer,
                     Integer (Buffer_Length),
                     Dummy_Code,
                     From_Status);
                  case From_Status is
                     when UTF_Conversions.Success =>
                        null;
                     when UTF_Conversions.Illegal_Sequence
                        | UTF_Conversions.Non_Shortest =>
                           --  Non_Shortest do not returned in UTF_16
                        Last := Item'First - 1;
                        Out_Last := Out_Item'First - 1;
                        Status := Illegal_Sequence;
                        pragma Check (Trace,
                           Check => Ada.Debug.Put ("illegal sequence"));
                        return; -- error
                     when UTF_Conversions.Truncated =>
                        Last := Item'First - 1;
                        Out_Last := Out_Item'First - 1;
                        Status := Truncated;
                        pragma Check (Trace, Ada.Debug.Put ("truncated"));
                        return; -- error
                  end case;
               end;
               Last := 2 * Ada.Streams.Stream_Element_Offset (Buffer_Length);
            when UTF_32 =>
               if Item'First + 3 > Item'Last then
                  Last := Item'First - 1;
                  Out_Last := Out_Item'First - 1;
                  Status := Truncated;
                  pragma Check (Trace, Ada.Debug.Put ("truncated"));
                  return; -- error
               end if;
               declare
                  Code : aliased UTF_Conversions.UCS_4;
                  Code_As_SEA : Ada.Streams.Stream_Element_Array (1 .. 4);
                  for Code_As_SEA'Address use Code'Address;
                  To_Status : UTF_Conversions.To_Status_Type;
               begin
                  Last := Item'First + 3;
                  Code_As_SEA := Item (Item'First .. Last);
                  UTF_Conversions.To_UTF_16 (
                     Code,
                     Buffer,
                     Integer (Buffer_Length),
                     To_Status);
                  if To_Status /= UTF_Conversions.Success then
                     Last := Item'First - 1;
                     Out_Last := Out_Item'First - 1;
                     pragma Assert (To_Status = UTF_Conversions.Unmappable);
                     Status := Illegal_Sequence;
                     pragma Check (Trace, Ada.Debug.Put ("illegal sequence"));
                     return; -- error
                  end if;
               end;
            when others =>
               declare
                  Item_As_C : aliased C.char_array (0 .. 0); -- at least
                  for Item_As_C'Address use Item'Address;
                  Length : C.signed_int;
               begin
                  if C.winnls.IsDBCSLeadByteEx (
                        C.windef.UINT (Object.From),
                        C.char'Pos (Item_As_C (0))) /=
                     C.windef.FALSE
                  then
                     if Item'First + 1 > Item'Last then
                        Last := Item'First - 1;
                        Out_Last := Out_Item'First - 1;
                        Status := Truncated;
                        pragma Check (Trace, Ada.Debug.Put ("truncated"));
                        return; -- error
                     end if;
                     Length := 2;
                  else
                     Length := 1;
                  end if;
                  Last :=
                     Item'First
                     + (Ada.Streams.Stream_Element_Offset (Length) - 1);
                  Buffer_Length := C.winnls.MultiByteToWideChar (
                     C.windef.UINT (Object.From),
                     C.winnls.MB_ERR_INVALID_CHARS,
                     Item_As_C (0)'Access,
                     Length,
                     Buffer_As_C (0)'Access,
                     Buffer'Length);
                  if Buffer_Length = 0 then
                     Out_Last := Out_Item'First - 1;
                     Status := Illegal_Sequence;
                     pragma Check (Trace, Ada.Debug.Put ("illegal sequence"));
                     return; -- error
                  end if;
               end;
         end case;
         case Object.To is
            when UTF_16 =>
               declare
                  Buffer_SEA_Length : constant
                        Ada.Streams.Stream_Element_Offset :=
                     2 * Ada.Streams.Stream_Element_Offset (Buffer_Length);
               begin
                  if Out_Item'First + (Buffer_SEA_Length - 1) >
                     Out_Item'Last
                  then
                     Last := Item'First - 1;
                     Out_Last := Out_Item'First - 1;
                     Status := Overflow;
                     pragma Check (Trace, Ada.Debug.Put ("overflow"));
                     return; -- error
                  end if;
                  Out_Last := Out_Item'First + (Buffer_SEA_Length - 1);
                  Out_Item (Out_Item'First .. Out_Last) :=
                     Buffer_As_SEA (1 .. Buffer_SEA_Length);
               end;
            when UTF_32 =>
               if Out_Item'First + 3 > Out_Item'Last then
                  Last := Item'First - 1;
                  Out_Last := Out_Item'First - 1;
                  Status := Overflow;
                  pragma Check (Trace, Ada.Debug.Put ("overflow"));
                  return; -- error
               end if;
               declare
                  Out_Code : aliased UTF_Conversions.UCS_4;
                  Out_Code_As_SEA : Ada.Streams.Stream_Element_Array (1 .. 4);
                  for Out_Code_As_SEA'Address use Out_Code'Address;
                  Buffer_Used : Natural;
                  From_Status : UTF_Conversions.From_Status_Type;
               begin
                  UTF_Conversions.From_UTF_16 (
                     Buffer (1 .. Integer (Buffer_Length)),
                     Buffer_Used,
                     Out_Code,
                     From_Status);
                  if From_Status /= UTF_Conversions.Success
                     or else Buffer_Used /= Integer (Buffer_Length)
                  then
                     Last := Item'First - 1;
                     Out_Last := Out_Item'First - 1;
                     Status := Illegal_Sequence;
                     pragma Check (Trace, Ada.Debug.Put ("illegal sequence"));
                     return; -- error
                  end if;
                  Out_Last := Out_Item'First + 3;
                  Out_Item (Out_Item'First .. Out_Last) := Out_Code_As_SEA;
               end;
            when others => -- including UTF_8
               declare
                  Out_Item_As_C : aliased C.char_array (0 .. 0); -- at least
                  for Out_Item_As_C'Address use Out_Item'Address;
                  Out_Length : C.signed_int;
               begin
                  Out_Length := C.winnls.WideCharToMultiByte (
                     C.windef.UINT (Object.To),
                     0, -- MB_ERR_INVALID_CHARS ?
                     Buffer_As_C (0)'Access,
                     Buffer_Length,
                     Out_Item_As_C (0)'Access,
                     Out_Item'Length,
                     null,
                     null);
                  if Out_Length = 0 then
                     Last := Item'First - 1;
                     Out_Last := Out_Item'First - 1;
                     case C.winbase.GetLastError is
                        when C.winerror.ERROR_INSUFFICIENT_BUFFER =>
                           Status := Overflow;
                           pragma Check (Trace, Ada.Debug.Put ("overflow"));
                        when others =>
                           Status := Illegal_Sequence;
                           pragma Check (Trace,
                              Check => Ada.Debug.Put ("illegal sequence"));
                     end case;
                     return; -- error
                  end if;
                  Out_Last :=
                     Out_Item'First
                     + (Ada.Streams.Stream_Element_Offset (Out_Length) - 1);
               end;
         end case;
      end if;
      if Finish and then Last = Item'Last then
         Status := Finished;
         pragma Check (Trace, Ada.Debug.Put ("finished"));
      else
         Status := Success;
         pragma Check (Trace, Ada.Debug.Put ("success"));
      end if;
      pragma Check (Trace, Ada.Debug.Put ("leave"));
   end Convert_No_Check;

   procedure Convert_No_Check (
      Object : Converter;
      Item : Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset;
      Out_Item : out Ada.Streams.Stream_Element_Array;
      Out_Last : out Ada.Streams.Stream_Element_Offset;
      Status : out Continuing_Status_Type)
   is
      Subsequence_Status : Subsequence_Status_Type;
   begin
      Convert_No_Check (
         Object,
         Item,
         Last,
         Out_Item,
         Out_Last,
         False,
         Subsequence_Status);
      pragma Assert (
         Subsequence_Status in
            Subsequence_Status_Type (Continuing_Status_Type'First) ..
            Subsequence_Status_Type (Continuing_Status_Type'Last));
      Status := Continuing_Status_Type (Subsequence_Status);
   end Convert_No_Check;

   procedure Convert_No_Check (
      Object : Converter;
      Out_Item : out Ada.Streams.Stream_Element_Array;
      Out_Last : out Ada.Streams.Stream_Element_Offset;
      Finish : True_Only;
      Status : out Finishing_Status_Type)
   is
      pragma Unreferenced (Object);
      pragma Unmodified (Out_Item);
      pragma Unreferenced (Finish);
   begin
      Out_Last := Out_Item'First - 1;
      Status := Finished;
   end Convert_No_Check;

   procedure Convert_No_Check (
      Object : Converter;
      Item : Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset;
      Out_Item : out Ada.Streams.Stream_Element_Array;
      Out_Last : out Ada.Streams.Stream_Element_Offset;
      Finish : True_Only;
      Status : out Substituting_Status_Type)
   is
      pragma Unreferenced (Finish);
      Item_Length : constant Ada.Streams.Stream_Element_Offset := Item'Length;
      Buffer : aliased Wide_String (1 .. Natural (Item_Length));
      Buffer_As_C : aliased C.winnt.WCHAR_array (0 .. 0); -- at least
      for Buffer_As_C'Address use Buffer'Address;
      Buffer_As_SEA : aliased
         Ada.Streams.Stream_Element_Array (1 .. 2 * Item_Length);
      for Buffer_As_SEA'Address use Buffer'Address;
      Buffer_Length : C.signed_int;
   begin
      Last := Item'Last;
      case Object.From is
         when UTF_16 =>
            Buffer_As_SEA (1 .. Item_Length) := Item;
            Buffer_Length := C.signed_int (Item_Length / 2);
            if Item_Length rem 2 /= 0 then
               Buffer_Length := Buffer_Length + 1;
               Buffer (Integer (Buffer_Length)) := '?'; -- use Substitute
            end if;
         when UTF_32 =>
            declare
               WW : aliased Wide_Wide_String (1 .. Natural (Item_Length / 4));
               WW_As_SEA : Ada.Streams.Stream_Element_Array (1 .. Item_Length);
               for WW_As_SEA'Address use WW'Address;
            begin
               WW_As_SEA := Item; -- alignment
               UTF_Conversions.From_32_To_16.Convert (
                  WW,
                  Buffer,
                  Natural (Buffer_Length),
                  Substitute => "?"); -- use Substitute
            end;
            if Item_Length rem 4 /= 0 then
               Buffer_Length := Buffer_Length + 1;
               Buffer (Integer (Buffer_Length)) := '?'; -- use Substitute
            end if;
         when others => -- including UTF_8
            declare
               Item_As_C : aliased C.char_array (0 .. 0); -- at least
               for Item_As_C'Address use Item'Address;
            begin
               Buffer_Length := C.winnls.MultiByteToWideChar (
                  C.windef.UINT (Object.From),
                  0, -- no C.winnls.MB_ERR_INVALID_CHARS for converting all
                  Item_As_C (0)'Access,
                  C.signed_int (Item_Length),
                  Buffer_As_C (0)'Access,
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
               Out_Last := Out_Item'First + (Buffer_Length_In_SEA - 1);
               if Out_Last > Out_Item'Last then
                  Last := Item'First - 1;
                  Out_Last := Out_Item'First - 1;
                  Status := Overflow;
                  pragma Check (Trace, Ada.Debug.Put ("overflow"));
                  return; -- error
               end if;
               Out_Item (Out_Item'First .. Out_Last) :=
                  Buffer_As_SEA (1 .. Buffer_Length_In_SEA);
            end;
         when UTF_32 =>
            declare
               Out_WW : aliased
                  Wide_Wide_String (1 .. Natural (Buffer_Length));
               Out_WW_As_SEA : Ada.Streams.Stream_Element_Array (
                  1 ..
                  4 * Ada.Streams.Stream_Element_Offset (Buffer_Length));
               for Out_WW_As_SEA'Address use Out_WW'Address;
               Out_WW_Length : Natural;
            begin
               UTF_Conversions.From_16_To_32.Convert (
                  Buffer,
                  Out_WW,
                  Out_WW_Length,
                  Substitute => "?"); -- use Substitute
               declare
                  Out_WW_Length_In_SEA : constant
                        Ada.Streams.Stream_Element_Offset :=
                     4 * Ada.Streams.Stream_Element_Offset (Out_WW_Length);
               begin
                  Out_Last := Out_Item'First + (Out_WW_Length_In_SEA - 1);
                  if Out_Last > Out_Item'Last then
                     Last := Item'First - 1;
                     Out_Last := Out_Item'First - 1;
                     Status := Overflow;
                     pragma Check (Trace, Ada.Debug.Put ("overflow"));
                     return; -- error
                  end if;
                  Out_Item (Out_Item'First .. Out_Last) :=
                     Out_WW_As_SEA (1 .. Out_WW_Length_In_SEA); -- un-alignment
               end;
            end;
         when others => -- including UTF_8
            declare
               Out_Item_As_C : aliased C.char_array (0 .. 0); -- at least
               for Out_Item_As_C'Address use Out_Item'Address;
               Out_Length : C.signed_int;
               Substitute_P : C.winnt.LPCSTR;
            begin
               if Object.Substitute_Length < 0 or else Object.To = UTF_8 then
                  Substitute_P := null;
               else
                  Substitute_P :=
                     LPCSTR_Conv.To_Pointer (Object.Substitute (1)'Address);
               end if;
               Out_Length := C.winnls.WideCharToMultiByte (
                  C.windef.UINT (Object.To),
                  0, -- no C.winnls.MB_ERR_INVALID_CHARS for converting all
                  Buffer_As_C (0)'Access,
                  Buffer_Length,
                  Out_Item_As_C (0)'Access,
                  Out_Item'Length,
                  Substitute_P,
                  null);
               if Out_Length = 0 and then Item'First <= Item'Last then
                  Last := Item'First - 1;
                  Out_Last := Out_Item'First - 1;
                  Status := Overflow;
                  pragma Check (Trace, Ada.Debug.Put ("overflow"));
                  return; -- error
               end if;
               Out_Last :=
                  Out_Item'First
                  + (Ada.Streams.Stream_Element_Offset (Out_Length) - 1);
            end;
      end case;
      Status := Finished;
   end Convert_No_Check;

   procedure Put_Substitute (
      Object : Converter;
      Out_Item : out Ada.Streams.Stream_Element_Array;
      Out_Last : out Ada.Streams.Stream_Element_Offset;
      Is_Overflow : out Boolean) is
   begin
      if Object.Substitute_Length < 0 then
         Default_Substitute (
            Object.To,
            Out_Item,
            Out_Last,
            Is_Overflow);
      else
         Is_Overflow := Out_Item'Length < Object.Substitute_Length;
         if Is_Overflow then
            Out_Last := Out_Item'First - 1;
         else
            Out_Last := Out_Item'First + (Object.Substitute_Length - 1);
            Out_Item (Out_Item'First .. Out_Last) :=
               Object.Substitute (1 .. Object.Substitute_Length);
         end if;
      end if;
   end Put_Substitute;

end System.Native_Environment_Encoding;
