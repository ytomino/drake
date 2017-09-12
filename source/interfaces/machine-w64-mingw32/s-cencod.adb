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
            Item_Length : constant Natural := Item'Length;
            W_Item : C.winnt.WCHAR_array (0 .. C.size_t (Item_Length - 1));
            W_Length : C.signed_int;
            Substitute_Length : constant C.size_t := Substitute'Length;
            Z_Substitute : aliased C.char_array (0 .. Substitute_Length);
            Target_Length : C.signed_int;
         begin
            Z_Substitute (0 .. Substitute_Length - 1) := Substitute;
            Z_Substitute (Substitute_Length) := C.char'Val (0);
            W_Length := C.winnls.MultiByteToWideChar (
               C.winnls.CP_UTF8,
               0,
               To_Pointer (Item'Address),
               C.signed_int (Item_Length),
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
               Z_Substitute (0)'Access,
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
            Item_Length : constant C.size_t := Item'Length;
            W_Item : C.winnt.WCHAR_array (0 .. Item_Length - 1);
            W_Length : C.signed_int;
            Target_Length : C.signed_int;
         begin
            W_Length := C.winnls.MultiByteToWideChar (
               C.winnls.CP_ACP,
               0,
               To_Pointer (Item'Address),
               C.signed_int (Item_Length),
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
      return C.wchar_t'Val (Wide_Character'Pos (Item));
   end To_wchar_t;

   function To_Wide_Character (
      Item : C.wchar_t;
      Substitute : Wide_Character)
      return Wide_Character
   is
      pragma Unreferenced (Substitute);
   begin
      return Wide_Character'Val (C.wchar_t'Pos (Item));
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
            Item_As_C : C.wchar_t_array (0 .. Count - 1);
            for Item_As_C'Address use Item'Address;
         begin
            Target (Target'First .. Target'First + (Count - 1)) := Item_As_C;
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
         Item_As_Ada : Wide_String (1 .. Count);
         for Item_As_Ada'Address use Item'Address;
      begin
         Target (Target'First .. Target'First + (Count - 1)) := Item_As_Ada;
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
         return C.wchar_t'Val (Wide_Wide_Character'Pos (Item));
      end if;
   end To_wchar_t;

   function To_Wide_Wide_Character (
      Item : C.wchar_t;
      Substitute : Wide_Wide_Character)
      return Wide_Wide_Character is
   begin
      if Item in C.wchar_t'Val (16#D800#) .. C.wchar_t'Val (16#DFFF#) then
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
      Item_Index : Positive := Item'First;
   begin
      Count := 0;
      if Item_Index <= Item'Last then
         declare
            Target_As_Ada : Wide_String (1 .. Target'Length);
            for Target_As_Ada'Address use Target'Address;
            Target_Index : C.size_t := Target'First;
         begin
            loop
               declare
                  Code : UTF_Conversions.UCS_4;
                  Item_Used : Natural;
                  From_Status : UTF_Conversions.From_Status_Type;
                  Target_Ada_Last : Natural;
                  Target_Last : C.size_t;
                  To_Status : UTF_Conversions.To_Status_Type;
               begin
                  UTF_Conversions.From_UTF_32 (
                     Item (Item_Index .. Item'Last),
                     Item_Used,
                     Code,
                     From_Status);
                  case From_Status is
                     when UTF_Conversions.Success =>
                        UTF_Conversions.To_UTF_16 (
                           Code,
                           Target_As_Ada (
                              Target_As_Ada'First
                                 + Integer (Target_Index - Target'First) ..
                              Target_As_Ada'Last),
                           Target_Ada_Last,
                           To_Status);
                        Target_Last := Target'First
                           + C.size_t (Target_Ada_Last - Target_As_Ada'First);
                        case To_Status is
                           when UTF_Conversions.Success =>
                              null;
                           when UTF_Conversions.Overflow
                              | UTF_Conversions.Unmappable =>
                              --  all values of UTF-16 are mappable to UTF-32
                              raise Constraint_Error;
                        end case;
                     when UTF_Conversions.Illegal_Sequence
                        | UTF_Conversions.Non_Shortest
                        | UTF_Conversions.Truncated =>
                           --  Non_Shortest and Truncated do not returned in
                           --    UTF-32.
                        Target_Last := Target_Index + (Substitute'Length - 1);
                        if Target_Last > Target'Last then
                           raise Constraint_Error; -- overflow
                        end if;
                        Target (Target_Index .. Target_Last) := Substitute;
                  end case;
                  Count := Target_Last - Target'First + 1;
                  exit when Item_Used >= Item'Last;
                  Item_Index := Item_Used + 1;
                  Target_Index := Target_Last + 1;
               end;
            end loop;
         end;
      end if;
   end To_Non_Nul_Terminated;

   procedure From_Non_Nul_Terminated (
      Item : C.wchar_t_array;
      Target : out Wide_Wide_String;
      Count : out Natural;
      Substitute : Wide_Wide_String)
   is
      pragma Suppress (Alignment_Check);
      Item_Index : C.size_t := Item'First;
   begin
      Count := 0;
      if Item_Index <= Item'Last then
         declare
            Item_As_Ada : Wide_String (1 .. Item'Length);
            for Item_As_Ada'Address use Item'Address;
            Target_Index : Positive := Target'First;
         begin
            loop
               declare
                  Code : UTF_Conversions.UCS_4;
                  Item_Ada_Used : Natural;
                  Item_Used : C.size_t;
                  From_Status : UTF_Conversions.From_Status_Type;
                  Target_Last : Natural;
                  To_Status : UTF_Conversions.To_Status_Type;
                  Put_Substitute : Boolean;
               begin
                  UTF_Conversions.From_UTF_16 (
                     Item_As_Ada (
                        Item_As_Ada'First
                           + Integer (Item_Index - Item'First) ..
                        Item_As_Ada'Last),
                     Item_Ada_Used,
                     Code,
                     From_Status);
                  Item_Used :=
                     Item'First + C.size_t (Item_Ada_Used - Item_As_Ada'First);
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
                        | UTF_Conversions.Non_Shortest
                        | UTF_Conversions.Truncated =>
                           --  Non_Shortest does not returned in UTF-16.
                        Put_Substitute := True;
                  end case;
                  if Put_Substitute then
                     Target_Last := Target_Index + (Substitute'Length - 1);
                     if Target_Last > Target'Last then
                        raise Constraint_Error; -- overflow
                     end if;
                     Target (Target_Index .. Target_Last) := Substitute;
                  end if;
                  Count := Target_Last - Target'First + 1;
                  exit when Item_Used >= Item'Last;
                  Item_Index := Item_Used + 1;
                  Target_Index := Target_Last + 1;
               end;
            end loop;
         end;
      end if;
   end From_Non_Nul_Terminated;

end System.C_Encoding;
