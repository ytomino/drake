with System.UTF_Conversions;
with C.winnls;
pragma Warnings (Off, C.winnls); -- break "pure" rule
with C.winnt;
pragma Warnings (Off, C.winnt); -- break "pure" rule
package body Interfaces.C.Inside is
   pragma Suppress (All_Checks);
   use type Standard.C.size_t;

   --  implementation of Character (UTF-8) from/to char (MBCS)

   function To_char (
      Item : Character;
      Substitute : char)
      return char is
   begin
      if Character'Pos (Item) > 16#7f# then
         return Substitute;
      else
         return char (Item);
      end if;
   end To_char;

   function To_Character (
      Item : char;
      Substitute : Character)
      return Character is
   begin
      if char'Pos (Item) > 16#7f# then
         return Substitute;
      else
         return Character (Item);
      end if;
   end To_Character;

   procedure To_Non_Nul_Terminated (
      Item : String;
      Target : out char_array;
      Count : out size_t;
      Substitute : char_array) is
   begin
      if Item'Length = 0 then
         Count := 0;
      else
         declare
            function To_Pointer (Value : System.Address)
               return Standard.C.winnt.LPSTR
               with Import, Convention => Intrinsic;
            W_Item : Standard.C.winnt.WCHAR_array (
               0 ..
               Standard.C.size_t (Natural'(Item'Length) - 1));
            W_Length : size_t;
            Sub : aliased constant char_array :=
               Substitute & char'Val (0);
            C_Sub : Standard.C.char_array (0 .. Sub'Length - 1);
            for C_Sub'Address use Sub'Address;
         begin
            W_Length := size_t (Standard.C.winnls.MultiByteToWideChar (
               Standard.C.winnls.CP_UTF8,
               0,
               To_Pointer (Item'Address),
               Item'Length,
               W_Item (0)'Access,
               W_Item'Length));
            if W_Length = 0 then
               raise Constraint_Error;
            end if;
            Count := size_t (Standard.C.winnls.WideCharToMultiByte (
               Standard.C.winnls.CP_ACP,
               0,
               W_Item (0)'Access,
               Standard.C.signed_int (W_Length),
               To_Pointer (Target'Address),
               Target'Length,
               C_Sub (0)'Access,
               null));
            if Count = 0 then
               raise Constraint_Error;
            end if;
         end;
      end if;
   end To_Non_Nul_Terminated;

   procedure From_Non_Nul_Terminated (
      Item : char_array;
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
            function To_Pointer (Value : System.Address)
               return Standard.C.winnt.LPSTR
               with Import, Convention => Intrinsic;
            W_Item : Standard.C.winnt.WCHAR_array (
               0 ..
               Standard.C.size_t (Natural'(Item'Length) - 1));
            W_Length : size_t;
         begin
            W_Length := size_t (Standard.C.winnls.MultiByteToWideChar (
               Standard.C.winnls.CP_ACP,
               0,
               To_Pointer (Item'Address),
               Item'Length,
               W_Item (0)'Access,
               W_Item'Length));
            if W_Length = 0 then
               raise Constraint_Error;
            end if;
            Count := Natural (Standard.C.winnls.WideCharToMultiByte (
               Standard.C.winnls.CP_UTF8,
               0,
               W_Item (0)'Access,
               Standard.C.signed_int (W_Length),
               To_Pointer (Target'Address),
               Target'Length,
               null,
               null));
            if Count = 0 then
               raise Constraint_Error;
            end if;
         end;
      end if;
   end From_Non_Nul_Terminated;

   --  implementation of Wide_Character (UTF-16) from/to wchar_t (UTF-16)

   function To_wchar_t (
      Item : Wide_Character;
      Substitute : wchar_t)
      return wchar_t
   is
      pragma Unreferenced (Substitute);
   begin
      return Wide_Character'Pos (Item);
   end To_wchar_t;

   function To_Wide_Character (
      Item : wchar_t;
      Substitute : Wide_Character)
      return Wide_Character
   is
      pragma Unreferenced (Substitute);
   begin
      return Wide_Character'Val (Item);
   end To_Wide_Character;

   procedure To_Non_Nul_Terminated (
      Item : Wide_String;
      Target : out wchar_array;
      Count : out size_t;
      Substitute : wchar_array)
   is
      pragma Unreferenced (Substitute);
      C_Item : wchar_array (0 .. Item'Length - 1);
      for C_Item'Address use Item'Address;
   begin
      Count := C_Item'Length;
      if Count > Target'Length then
         raise Constraint_Error;
      end if;
      Target (Target'First .. Target'First + C_Item'Length - 1) := C_Item;
   end To_Non_Nul_Terminated;

   procedure From_Non_Nul_Terminated (
      Item : wchar_array;
      Target : out Wide_String;
      Count : out Natural;
      Substitute : Wide_String)
   is
      pragma Unreferenced (Substitute);
      Ada_Item : Wide_String (1 .. Item'Length);
      for Ada_Item'Address use Item'Address;
   begin
      Count := Item'Length;
      if Count > Target'Length then
         raise Constraint_Error;
      end if;
      Target (Target'First .. Target'First + Count - 1) :=
         Ada_Item (1 .. Count);
   end From_Non_Nul_Terminated;

   --  Wide_Wide_Character (UTF-32) from/to wchar_t (UTF-32)

   function To_wchar_t (
      Item : Wide_Wide_Character;
      Substitute : wchar_t)
      return wchar_t is
   begin
      if Wide_Wide_Character'Pos (Item) > 16#FFFF# then
         --  a check for detecting illegal sequence are omitted
         return Substitute;
      else
         return Wide_Wide_Character'Pos (Item);
      end if;
   end To_wchar_t;

   function To_Wide_Wide_Character (
      Item : wchar_t;
      Substitute : Wide_Wide_Character)
      return Wide_Wide_Character is
   begin
      if wchar_t'Pos (Item) in 16#D800# .. 16#DFFF# then
         return Substitute;
      else
         return Wide_Wide_Character'Val (wchar_t'Pos (Item));
      end if;
   end To_Wide_Wide_Character;

   procedure To_Non_Nul_Terminated (
      Item : Wide_Wide_String;
      Target : out wchar_array;
      Count : out size_t;
      Substitute : wchar_array)
   is
      Ada_Target : Wide_String (1 .. Target'Length);
      for Ada_Target'Address use Target'Address;
      Item_Index : Natural := Item'First;
      Target_Index : C.size_t := Target'First;
   begin
      while Item_Index <= Item'Last loop
         declare
            Code : System.UTF_Conversions.UCS_4;
            Item_Used : Natural;
            From_Status : System.UTF_Conversions.From_Status_Type;
            Ada_Target_Last : Natural;
            Target_Last : C.size_t;
            To_Status : System.UTF_Conversions.To_Status_Type;
         begin
            System.UTF_Conversions.From_UTF_32 (
               Item (Item_Index .. Item'Last),
               Item_Used,
               Code,
               From_Status);
            Item_Index := Item_Used + 1;
            case From_Status is
               when System.UTF_Conversions.Success =>
                  System.UTF_Conversions.To_UTF_16 (
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
                     when System.UTF_Conversions.Success =>
                        null;
                     when System.UTF_Conversions.Overflow
                        | System.UTF_Conversions.Unmappable =>
                        --  all values of UTF-16 are mappable to UTF-32
                        raise Constraint_Error;
                  end case;
               when System.UTF_Conversions.Illegal_Sequence
                  | System.UTF_Conversions.Truncated =>
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
      Item : wchar_array;
      Target : out Wide_Wide_String;
      Count : out Natural;
      Substitute : Wide_Wide_String)
   is
      Ada_Item : Wide_String (1 .. Item'Length);
      for Ada_Item'Address use Item'Address;
      Item_Index : C.size_t := Item'First;
      Target_Index : Natural := Target'First;
   begin
      while Item_Index <= Item'Last loop
         declare
            Code : System.UTF_Conversions.UCS_4;
            Ada_Item_Used : Natural;
            Item_Used : C.size_t;
            From_Status : System.UTF_Conversions.From_Status_Type;
            Target_Last : Natural;
            To_Status : System.UTF_Conversions.To_Status_Type;
            Put_Substitute : Boolean;
         begin
            System.UTF_Conversions.From_UTF_16 (
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
               when System.UTF_Conversions.Success =>
                  System.UTF_Conversions.To_UTF_32 (
                     Code,
                     Target (Target_Index .. Target'Last),
                     Target_Last,
                     To_Status);
                  case To_Status is
                     when System.UTF_Conversions.Success =>
                        Put_Substitute := False;
                     when System.UTF_Conversions.Overflow =>
                        raise Constraint_Error;
                     when System.UTF_Conversions.Unmappable =>
                        Put_Substitute := True;
                  end case;
               when System.UTF_Conversions.Illegal_Sequence
                  | System.UTF_Conversions.Truncated =>
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

end Interfaces.C.Inside;
