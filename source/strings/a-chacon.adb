with System.UTF_Conversions;
package body Ada.Characters.Conversions is

   function Is_Character (Item : Wide_Character) return Boolean is
   begin
      return Wide_Character'Pos (Item) <= 16#7f#;
   end Is_Character;

   function Is_Character (Item : Wide_Wide_Character) return Boolean is
   begin
      return Wide_Wide_Character'Pos (Item) <= 16#7f#;
   end Is_Character;

   function Is_String (Item : Wide_String) return Boolean is
      pragma Unreferenced (Item);
   begin
      return True;
   end Is_String;

   function Is_String (Item : Wide_Wide_String) return Boolean is
      pragma Unreferenced (Item);
   begin
      return True;
   end Is_String;

   function Is_Wide_Character (Item : Wide_Wide_Character) return Boolean is
   begin
      return Wide_Wide_Character'Pos (Item) <= 16#ffff#;
   end Is_Wide_Character;

   function Is_Wide_String (Item : Wide_Wide_String) return Boolean is
      pragma Unreferenced (Item);
   begin
      return True;
   end Is_Wide_String;

   function To_Character (
      Item : Wide_Character;
      Substitute : Character := ' ')
      return Character is
   begin
      if Wide_Character'Pos (Item) <= 16#7f# then
         return Character'Val (Wide_Character'Pos (Item));
      else
         return Substitute;
      end if;
   end To_Character;

   function To_Character (
      Item : Wide_Wide_Character;
      Substitute : Character := ' ')
      return Character is
   begin
      if Wide_Wide_Character'Pos (Item) <= 16#7f# then
         return Character'Val (Wide_Wide_Character'Pos (Item));
      else
         return Substitute;
      end if;
   end To_Character;

   function To_String (
      Item : Wide_String;
      Substitute : Character := ' ')
      return String
   is
      pragma Unreferenced (Substitute);
      Result : String (
         1 ..
         Item'Length * System.UTF_Conversions.UTF_8_Max_Length);
      Last : Natural;
      Error : Boolean;
   begin
      System.UTF_Conversions.UTF_16_To_UTF_8 (Item, Result, Last, Error);
      return Result (1 .. Last);
   end To_String;

   function To_String (
      Item : Wide_Wide_String;
      Substitute : Character := ' ')
      return String
   is
      pragma Unreferenced (Substitute);
      Result : String (
         1 ..
         Item'Length * System.UTF_Conversions.UTF_8_Max_Length);
      Last : Natural;
      Error : Boolean;
   begin
      System.UTF_Conversions.UTF_32_To_UTF_8 (Item, Result, Last, Error);
      return Result (1 .. Last);
   end To_String;

   function To_Wide_Character (Item : Character) return Wide_Character is
   begin
      return Wide_Character'Val (Character'Pos (Item));
   end To_Wide_Character;

   function To_Wide_Character (
      Item : Wide_Wide_Character;
      Substitute : Wide_Character := ' ')
      return Wide_Character is
   begin
      if Wide_Wide_Character'Pos (Item) <= 16#ffff# then
         return Wide_Character'Val (Wide_Wide_Character'Pos (Item));
      else
         return Substitute;
      end if;
   end To_Wide_Character;

   function To_Wide_String (Item : String) return Wide_String is
      Result : Wide_String (1 .. Item'Length);
      Last : Natural;
      Error : Boolean;
   begin
      System.UTF_Conversions.UTF_8_To_UTF_16 (Item, Result, Last, Error);
      return Result (1 .. Last);
   end To_Wide_String;

   function To_Wide_String (
      Item : Wide_Wide_String;
      Substitute : Wide_Character := ' ')
      return Wide_String
   is
      pragma Unreferenced (Substitute);
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
            Error : Boolean;
         begin
            System.UTF_Conversions.From_UTF_32 (Item (I), Code, Error);
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

   function To_Wide_Wide_Character (Item : Character)
      return Wide_Wide_Character is
   begin
      return Wide_Wide_Character'Val (Character'Pos (Item));
   end To_Wide_Wide_Character;

   function To_Wide_Wide_Character (Item : Wide_Character)
      return Wide_Wide_Character is
   begin
      return Wide_Wide_Character'Val (Wide_Character'Pos (Item));
   end To_Wide_Wide_Character;

   function To_Wide_Wide_String (Item : String) return Wide_Wide_String is
      Result : Wide_Wide_String (1 .. Item'Length);
      Last : Natural;
      Error : Boolean;
   begin
      System.UTF_Conversions.UTF_8_To_UTF_32 (Item, Result, Last, Error);
      return Result (1 .. Last);
   end To_Wide_Wide_String;

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

end Ada.Characters.Conversions;
