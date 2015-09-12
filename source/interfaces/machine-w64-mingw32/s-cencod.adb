with System.UTF_Conversions;
with C.winnls;
with C.winnt;
package body System.C_Encoding is
   use type C.char_array;
   use type C.signed_int;
   use type C.size_t;

   --  implementation of Character (UTF-8) from/to char (MBCS)

   function To_char (
      Item : Character;
      Substitute : C.char)
      return C.char is
   begin
      if Character'Pos (Item) > 16#7f# then
         return Substitute;
      else
         return C.char (Item);
      end if;
   end To_char;

   function To_Character (
      Item : C.char;
      Substitute : Character)
      return Character is
   begin
      if C.char'Pos (Item) > 16#7f# then
         return Substitute;
      else
         return Character (Item);
      end if;
   end To_Character;

   procedure To_Non_Nul_Terminated (
      Item : String;
      Target : out C.char_array;
      Count : out C.size_t;
      Substitute : C.char_array) is
   begin
      if Item'Length = 0 then
         Count := 0;
      else
         declare
            function To_Pointer (Value : Address)
               return C.winnt.LPSTR
               with Import, Convention => Intrinsic;
            W_Item : C.winnt.WCHAR_array (
               0 ..
               C.size_t (Natural'(Item'Length) - 1));
            W_Length : C.signed_int;
            Sub : aliased constant C.char_array :=
               Substitute & C.char'Val (0);
            Target_Length : C.signed_int;
         begin
            W_Length := C.winnls.MultiByteToWideChar (
               C.winnls.CP_UTF8,
               0,
               To_Pointer (Item'Address),
               Item'Length,
               W_Item (0)'Access,
               W_Item'Length);
            if W_Length = 0 then
               raise Constraint_Error;
            end if;
            Target_Length := C.winnls.WideCharToMultiByte (
               C.winnls.CP_ACP,
               0,
               W_Item (0)'Access,
               W_Length,
               To_Pointer (Target'Address),
               Target'Length,
               Sub (Sub'First)'Access,
               null);
            if Target_Length = 0 then
               raise Constraint_Error;
            end if;
            Count := C.size_t (Target_Length);
         end;
      end if;
   end To_Non_Nul_Terminated;

   procedure From_Non_Nul_Terminated (
      Item : C.char_array;
      Target : out String;
      Count : out Natural;
      Substitute : String)
   is
      pragma Unreferenced (Substitute);
   begin
      if Item'Length = 0 then
         Count := 0;
      else
         declare
            function To_Pointer (Value : Address)
               return C.winnt.LPSTR
               with Import, Convention => Intrinsic;
            W_Item : C.winnt.WCHAR_array (
               0 ..
               C.size_t (Natural'(Item'Length) - 1));
            W_Length : C.signed_int;
            Target_Length : C.signed_int;
         begin
            W_Length := C.winnls.MultiByteToWideChar (
               C.winnls.CP_ACP,
               0,
               To_Pointer (Item'Address),
               Item'Length,
               W_Item (0)'Access,
               W_Item'Length);
            if W_Length = 0 then
               raise Constraint_Error;
            end if;
            Target_Length := C.winnls.WideCharToMultiByte (
               C.winnls.CP_UTF8,
               0,
               W_Item (0)'Access,
               W_Length,
               To_Pointer (Target'Address),
               Target'Length,
               null,
               null);
            if Target_Length = 0 then
               raise Constraint_Error;
            end if;
            Count := Natural (Target_Length);
         end;
      end if;
   end From_Non_Nul_Terminated;

   --  implementation of Wide_Character (UTF-16) from/to wchar_t (UTF-16)

   function To_wchar_t (
      Item : Wide_Character;
      Substitute : C.wchar_t)
      return C.wchar_t
   is
      pragma Unreferenced (Substitute);
   begin
      return Wide_Character'Pos (Item);
   end To_wchar_t;

   function To_Wide_Character (
      Item : C.wchar_t;
      Substitute : Wide_Character)
      return Wide_Character
   is
      pragma Unreferenced (Substitute);
   begin
      return Wide_Character'Val (Item);
   end To_Wide_Character;

   procedure To_Non_Nul_Terminated (
      Item : Wide_String;
      Target : out C.wchar_t_array;
      Count : out C.size_t;
      Substitute : C.wchar_t_array)
   is
      pragma Unreferenced (Substitute);
   begin
      Count := Item'Length;
      if Count > 0 then
         if Count > Target'Length then
            raise Constraint_Error;
         end if;
         declare
            pragma Suppress (Alignment_Check);
            C_Item : C.wchar_t_array (0 .. Count - 1);
            for C_Item'Address use Item'Address;
         begin
            Target (Target'First .. Target'First + Count - 1) := C_Item;
         end;
      end if;
   end To_Non_Nul_Terminated;

   procedure From_Non_Nul_Terminated (
      Item : C.wchar_t_array;
      Target : out Wide_String;
      Count : out Natural;
      Substitute : Wide_String)
   is
      pragma Unreferenced (Substitute);
   begin
      Count := Item'Length;
      if Count > Target'Length then
         raise Constraint_Error;
      end if;
      declare
         pragma Suppress (Alignment_Check);
         Ada_Item : Wide_String (1 .. Count);
         for Ada_Item'Address use Item'Address;
      begin
         Target (Target'First .. Target'First + Count - 1) := Ada_Item;
      end;
   end From_Non_Nul_Terminated;

   --  Wide_Wide_Character (UTF-32) from/to wchar_t (UTF-32)

   function To_wchar_t (
      Item : Wide_Wide_Character;
      Substitute : C.wchar_t)
      return C.wchar_t is
   begin
      if Wide_Wide_Character'Pos (Item) > 16#FFFF# then
         --  a check for detecting illegal sequence are omitted
         return Substitute;
      else
         return Wide_Wide_Character'Pos (Item);
      end if;
   end To_wchar_t;

   function To_Wide_Wide_Character (
      Item : C.wchar_t;
      Substitute : Wide_Wide_Character)
      return Wide_Wide_Character is
   begin
      if C.wchar_t'Pos (Item) in 16#D800# .. 16#DFFF# then
         return Substitute;
      else
         return Wide_Wide_Character'Val (C.wchar_t'Pos (Item));
      end if;
   end To_Wide_Wide_Character;

   procedure To_Non_Nul_Terminated (
      Item : Wide_Wide_String;
      Target : out C.wchar_t_array;
      Count : out C.size_t;
      Substitute : C.wchar_t_array)
   is
      pragma Suppress (Alignment_Check);
      Ada_Target : Wide_String (1 .. Target'Length);
      for Ada_Target'Address use Target'Address;
      Item_Index : Natural := Item'First;
      Target_Index : C.size_t := Target'First;
   begin
      while Item_Index <= Item'Last loop
         declare
            Code : UTF_Conversions.UCS_4;
            Item_Used : Natural;
            From_Status : UTF_Conversions.From_Status_Type;
            Ada_Target_Last : Natural;
            Target_Last : C.size_t;
            To_Status : UTF_Conversions.To_Status_Type;
         begin
            UTF_Conversions.From_UTF_32 (
               Item (Item_Index .. Item'Last),
               Item_Used,
               Code,
               From_Status);
            Item_Index := Item_Used + 1;
            case From_Status is
               when UTF_Conversions.Success =>
                  UTF_Conversions.To_UTF_16 (
                     Code,
                     Ada_Target (
                        Ada_Target'First
                           + Integer (Target_Index - Target'First) ..
                        Ada_Target'Last),
                     Ada_Target_Last,
                     To_Status);
                  Target_Last := Target'First
                     + C.size_t (Ada_Target_Last - Ada_Target'First);
                  case To_Status is
                     when UTF_Conversions.Success =>
                        null;
                     when UTF_Conversions.Overflow
                        | UTF_Conversions.Unmappable =>
                        --  all values of UTF-16 are mappable to UTF-32
                        raise Constraint_Error;
                  end case;
               when UTF_Conversions.Illegal_Sequence
                  | UTF_Conversions.Truncated =>
                  Target_Last := Target_Index + Substitute'Length - 1;
                  if Target_Last > Target'Last then
                     raise Constraint_Error; -- overflow
                  end if;
                  Target (Target_Index .. Target_Last) := Substitute;
            end case;
            Target_Index := Target_Last + 1;
         end;
      end loop;
      Count := Target_Index - Target'First;
   end To_Non_Nul_Terminated;

   procedure From_Non_Nul_Terminated (
      Item : C.wchar_t_array;
      Target : out Wide_Wide_String;
      Count : out Natural;
      Substitute : Wide_Wide_String)
   is
      pragma Suppress (Alignment_Check);
      Ada_Item : Wide_String (1 .. Item'Length);
      for Ada_Item'Address use Item'Address;
      Item_Index : C.size_t := Item'First;
      Target_Index : Natural := Target'First;
   begin
      while Item_Index <= Item'Last loop
         declare
            Code : UTF_Conversions.UCS_4;
            Ada_Item_Used : Natural;
            Item_Used : C.size_t;
            From_Status : UTF_Conversions.From_Status_Type;
            Target_Last : Natural;
            To_Status : UTF_Conversions.To_Status_Type;
            Put_Substitute : Boolean;
         begin
            UTF_Conversions.From_UTF_16 (
               Ada_Item (
                  Ada_Item'First + Integer (Item_Index - Item'First) ..
                  Ada_Item'Last),
               Ada_Item_Used,
               Code,
               From_Status);
            Item_Used := Item'First
               + C.size_t (Ada_Item_Used - Ada_Item'First);
            Item_Index := Item_Used + 1;
            case From_Status is
               when UTF_Conversions.Success =>
                  UTF_Conversions.To_UTF_32 (
                     Code,
                     Target (Target_Index .. Target'Last),
                     Target_Last,
                     To_Status);
                  case To_Status is
                     when UTF_Conversions.Success =>
                        Put_Substitute := False;
                     when UTF_Conversions.Overflow =>
                        raise Constraint_Error;
                     when UTF_Conversions.Unmappable =>
                        Put_Substitute := True;
                  end case;
               when UTF_Conversions.Illegal_Sequence
                  | UTF_Conversions.Truncated =>
                  --  Truncated does not returned in UTF-32
                  Put_Substitute := True;
            end case;
            if Put_Substitute then
               Target_Last := Target_Index + Substitute'Length - 1;
               if Target_Last > Target'Last then
                  raise Constraint_Error; -- overflow
               end if;
               Target (Target_Index .. Target_Last) := Substitute;
            end if;
            Target_Index := Target_Last + 1;
         end;
      end loop;
      Count := Target_Index - Target'First;
   end From_Non_Nul_Terminated;

end System.C_Encoding;
