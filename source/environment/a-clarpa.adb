with System.Address_To_Constant_Access_Conversions;
with System.Storage_Elements;
package body Ada.Command_Line.Argument_Parsing is
   use type System.Storage_Elements.Storage_Offset;

   function Match (
      Argument : String;
      Position : in out Cursor;
      Short_Name : Character;
      Option : Option_Character := ' ')
      return Boolean;
   function Match (
      Argument : String;
      Position : in out Cursor;
      Short_Name : Character;
      Option : Option_Character := ' ')
      return Boolean is
   begin
      if Argument (Position.Index) = Short_Name then
         case Option is
            when ' ' =>
               return True;
            when ':' | '?' =>
               if Position.Index = Argument'First + 1 then
                  Position.Option_Index := Position.Index + 1;
                  if Option = ':'
                     or else Position.Option_Index <= Argument'Last
                  then
                     Position.Has_Value := True;
                  end if;
                  return True;
               else
                  return False;
               end if;
         end case;
      else
         return False;
      end if;
   end Match;

   function Match (
      Argument : String;
      Position : in out Cursor;
      Long_Name : String;
      Option : Option_Character := ' ')
      return Boolean;
   function Match (
      Argument : String;
      Position : in out Cursor;
      Long_Name : String;
      Option : Option_Character := ' ')
      return Boolean is
   begin
      if Argument (Position.Index .. Position.Option_Index - 2) =
         Long_Name
      then
         case Option is
            when ' ' =>
               return Position.Option_Index - 2 = Argument'Last;
            when ':' | '?' =>
               if Option = ':'
                  or else Position.Option_Index - 1 <= Argument'Last
               then
                  Position.Has_Value := True;
               end if;
               return True;
         end case;
      else
         return False;
      end if;
   end Match;

   --  implementation

   function Has_Element (Position : Cursor) return Boolean is
   begin
      return Position.Index > 0;
   end Has_Element;

   function Iterate (Argument : String; Initial_State : State_Type)
      return Argument_Iterator
   is
      pragma Suppress (Accessibility_Check);
      First : Cursor;
   begin
      First.State := Initial_State;
      First.Has_Value := False;
      if not Initial_State.Not_Option
         and then Argument'Length >= 1
         and then Argument (Argument'First) = '-'
      then
         if Argument'Length >= 2
            and then Argument (Argument'First + 1) = '-'
         then
            if Argument'Length = 2 then
               First.State.Not_Option := True;
               First.Kind := Double_Hyphen;
               First.Index := 0; -- No_Element
               First.Option_Index := 0;
            else
               First.Kind := Long_Option;
               First.Index := Argument'First + 2;
               declare
                  type Character_Access is access constant Character;
                  for Character_Access'Storage_Size use 0;
                  package Conv is
                     new System.Address_To_Constant_Access_Conversions (
                        Character,
                        Character_Access);
                  function memchr (
                     s : not null Character_Access;
                     c : Integer;
                     n : System.Storage_Elements.Storage_Count)
                     return Character_Access;
                  pragma Import (Intrinsic, memchr, "__builtin_memchr");
                  S : constant not null Character_Access :=
                     Argument (First.Index)'Unrestricted_Access;
                  P : constant Character_Access := memchr (
                     S,
                     Character'Pos ('='),
                     System.Storage_Elements.Storage_Offset (
                        Argument'Last - First.Index + 1));
               begin
                  if P = null then
                     First.Option_Index := Argument'Last + 2;
                  else
                     First.Option_Index :=
                        First.Index
                        + Integer (Conv.To_Address (P) - Conv.To_Address (S)
                        + 1);
                  end if;
               end;
            end if;
         else
            First.Kind := Short_Option;
            First.Index := Argument'First + 1;
            First.Option_Index := Argument'Last + 1;
         end if;
      else
         if First.State.Posixly_Correct then
            First.State.Not_Option := True;
         end if;
         First.Kind := Not_Option;
         First.Index := Argument'First;
         First.Option_Index := Argument'Last + 1;
      end if;
      return Argument_Iterator'(First => First);
   end Iterate;

   function State (Iterator : Argument_Iterator) return State_Type is
   begin
      return Iterator.First.State;
   end State;

   function Is_Option (
      Argument : String;
      Position : in out Cursor;
      Short_Name : Character;
      Option : Option_Character := ' ')
      return Boolean is
   begin
      case Position.Kind is
         when Short_Option =>
            return Match (Argument, Position, Short_Name, Option);
         when others =>
            return False;
      end case;
   end Is_Option;

   function Is_Option (
      Argument : String;
      Position : in out Cursor;
      Long_Name : String;
      Option : Option_Character := ' ')
      return Boolean is
   begin
      case Position.Kind is
         when Long_Option =>
            return Match (Argument, Position, Long_Name, Option);
         when others =>
            return False;
      end case;
   end Is_Option;

   function Is_Option (
      Argument : String;
      Position : in out Cursor;
      Short_Name : Character;
      Long_Name : String;
      Option : Option_Character := ' ')
      return Boolean is
   begin
      case Position.Kind is
         when Short_Option =>
            return Match (Argument, Position, Short_Name, Option);
         when Long_Option =>
            return Match (Argument, Position, Long_Name, Option);
         when others =>
            return False;
      end case;
   end Is_Option;

   function Is_Unknown_Option (
      Argument : String;
      Position : in out Cursor)
      return Boolean is
   begin
      case Position.Kind is
         when Short_Option =>
            return True;
         when Long_Option =>
            if Position.Option_Index - 1 <= Argument'Last then
               Position.Has_Value := True;
            end if;
            return True;
         when others =>
            return False;
      end case;
   end Is_Unknown_Option;

   function Name (Argument : String; Position : Cursor) return String is
   begin
      case Position.Kind is
         when Short_Option =>
            return ('-', Argument (Position.Index));
         when Long_Option =>
            return Argument (Argument'First .. Position.Option_Index - 2);
         when others =>
            raise Constraint_Error; -- status error
      end case;
   end Name;

   function Short_Name (Argument : String; Position : Cursor)
      return Character is
   begin
      case Position.Kind is
         when Short_Option =>
            return Argument (Position.Index);
         when Long_Option =>
            return Character'Val (0);
         when others =>
            raise Constraint_Error; -- status error
      end case;
   end Short_Name;

   function Long_Name (Argument : String; Position : Cursor) return String is
   begin
      case Position.Kind is
         when Short_Option =>
            return "";
         when Long_Option =>
            return Argument (Position.Index .. Position.Option_Index - 2);
         when others =>
            raise Constraint_Error; -- status error
      end case;
   end Long_Name;

   function Has_Value (Argument : String; Position : Cursor)
      return Value_Location is
   begin
      case Position.Kind is
         when Short_Option =>
            if Position.Has_Value then
               if Position.Option_Index <= Argument'Last then
                  return Same;
               else
                  return Next;
               end if;
            else
               return None;
            end if;
         when Long_Option =>
            if Position.Has_Value then
               if Position.Option_Index - 1 <= Argument'Last then
                  return Same;
               else
                  return Next;
               end if;
            else
               return None;
            end if;
         when others =>
            raise Constraint_Error; -- status error
      end case;
   end Has_Value;

   function Value (Argument : String; Position : Cursor) return String is
   begin
      case Position.Kind is
         when Short_Option | Long_Option =>
            return Argument (Position.Option_Index .. Argument'Last);
         when others =>
            raise Constraint_Error; -- status error
      end case;
   end Value;

   function First (Object : Argument_Iterator) return Cursor is
   begin
      return Object.First;
   end First;

   function Next (Object : Argument_Iterator; Position : Cursor)
      return Cursor
   is
      pragma Unreferenced (Object);
   begin
      case Position.Kind is
         when Short_Option =>
            if Position.Index < Position.Option_Index - 1 then
               return Result : Cursor := Position do
                  Result.Index := Result.Index + 1;
               end return;
            else
               return No_Element;
            end if;
         when others =>
            return No_Element;
      end case;
   end Next;

end Ada.Command_Line.Argument_Parsing;
