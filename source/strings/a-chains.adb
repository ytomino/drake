with System.UTF_Conversions;
package body Ada.Characters.Inside is

   function To_Character (Item : Wide_Wide_Character)
      return Character is
   begin
      if Wide_Wide_Character'Pos (Item) >= 16#80# then
         raise Constraint_Error;
      else
         return Character'Val (Wide_Wide_Character'Pos (Item));
      end if;
   end To_Character;

   function To_Wide_Wide_Character (Item : Character)
      return Wide_Wide_Character is
   begin
      if Character'Pos (Item) >= 16#80# then
         raise Constraint_Error;
      else
         return Wide_Wide_Character'Val (Character'Pos (Item));
      end if;
   end To_Wide_Wide_Character;

   function To_Wide_Character (Item : Wide_Wide_Character)
      return Wide_Character is
   begin
      if Wide_Wide_Character'Pos (Item) >= 16#ffff# then
         raise Constraint_Error;
      else
         return Wide_Character'Val (Wide_Wide_Character'Pos (Item));
      end if;
   end To_Wide_Character;

   function To_Wide_Wide_Character (Item : Wide_Character)
      return Wide_Wide_Character is
   begin
      case Wide_Character'Pos (Item) is
         when 16#d800# .. 16#dfff# =>
            raise Constraint_Error;
         when others =>
            return Wide_Wide_Character'Val (Wide_Character'Pos (Item));
      end case;
   end To_Wide_Wide_Character;

   function To_String (Item : Wide_Wide_String)
      return String
   is
      Result : String (
         1 ..
         Item'Length * System.UTF_Conversions.UTF_8_Max_Length);
      Last : Natural;
      Error : Boolean;
   begin
      System.UTF_Conversions.UTF_32_To_UTF_8 (Item, Result, Last, Error);
      return Result (1 .. Last);
   end To_String;

   function To_Wide_Wide_String (Item : String) return Wide_Wide_String is
      Result : Wide_Wide_String (1 .. Item'Length);
      Last : Natural;
      Error : Boolean;
   begin
      System.UTF_Conversions.UTF_8_To_UTF_32 (Item, Result, Last, Error);
      return Result (1 .. Last);
   end To_Wide_Wide_String;

   function To_Wide_String (Item : Wide_Wide_String)
      return Wide_String
   is
      Result : Wide_String (
         1 ..
         Item'Length * System.UTF_Conversions.UTF_16_Max_Length);
      Last : Natural;
      J : Natural := Result'First;
   begin
      Last := J - 1;
      for I in Item'Range loop
         declare
            Code : System.UTF_Conversions.UCS_4;
            Next : Positive;
            Error : Boolean;
         begin
            System.UTF_Conversions.From_UTF_32 (
               Item (I .. Item'Last),
               Next,
               Code,
               Error);
            System.UTF_Conversions.To_UTF_16 (
               Code,
               Result (J .. Result'Last),
               Last,
               Error);
            J := Last + 1;
         end;
      end loop;
      return Result (1 .. Last);
   end To_Wide_String;

   function To_Wide_Wide_String (Item : Wide_String) return Wide_Wide_String is
      Result : Wide_Wide_String (1 .. Item'Length);
      Last : Natural;
      I : Natural := Item'First;
      J : Natural := Result'First;
   begin
      Last := J - 1;
      while I <= Item'Last loop
         declare
            Code : System.UTF_Conversions.UCS_4;
            Next : Natural;
            Error : Boolean;
         begin
            System.UTF_Conversions.From_UTF_16 (
               Item (I .. Item'Last),
               Next,
               Code,
               Error);
            I := Next + 1;
            Result (J) := Wide_Wide_Character'Val (Code);
            Last := J;
            J := Last + 1;
         end;
      end loop;
      return Result (1 .. Last);
   end To_Wide_Wide_String;

end Ada.Characters.Inside;
