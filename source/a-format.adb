with System.Formatting.Decimal;
with System.Formatting.Fixed;
with System.Formatting.Float;
with System.Long_Long_Integer_Types;
package body Ada.Formatting is
   pragma Suppress (All_Checks);
   use type System.Long_Long_Integer_Types.Word_Unsigned;
   use type System.Long_Long_Integer_Types.Long_Long_Unsigned;

   subtype Word_Unsigned is System.Long_Long_Integer_Types.Word_Unsigned;
   subtype Long_Long_Unsigned is
      System.Long_Long_Integer_Types.Long_Long_Unsigned;

   pragma Compile_Time_Error (
      No_Sign /= System.Formatting.No_Sign,
      "No_Sign mismatch");

   function Integer_Image (Item : T) return String is
      Longest_Abs_Item : Long_Long_Unsigned;
      Word_Abs_Item : Word_Unsigned;
      Result : String (
         1 .. 4 + Long_Long_Integer'Width + Digits_Width); -- "16##"
      Last : Natural := Result'First - 1;
      Error : Boolean;
   begin
      declare
         Sign : Character;
      begin
         if Item < 0 then
            if T'Size > Standard'Word_Size then
               Longest_Abs_Item := -Long_Long_Unsigned'Mod (Item);
            else
               Word_Abs_Item := -Word_Unsigned'Mod (Item);
            end if;
            Sign := Signs (-1);
         else
            if T'Size > Standard'Word_Size then
               Longest_Abs_Item := Long_Long_Unsigned (Item);
            else
               Word_Abs_Item := Word_Unsigned (Item);
            end if;
            if Item > 0 then
               Sign := Signs (1);
            else
               Sign := Signs (0);
            end if;
         end if;
         if Sign /= No_Sign then
            Last := Last + 1;
            Result (Last) := Sign;
         end if;
      end;
      if Form = Ada and then Base /= 10 then
         System.Formatting.Image (
            Word_Unsigned (Base),
            Result (Last + 1 .. Result'Last),
            Last,
            Error => Error);
         Last := Last + 1;
         Result (Last) := '#';
      end if;
      if T'Size > Standard'Word_Size then
         System.Formatting.Image (
            Longest_Abs_Item,
            Result (Last + 1 .. Result'Last),
            Last,
            Base => Base,
            Set =>
               System.Formatting.Type_Set'Enum_Val (Type_Set'Enum_Rep (Set)),
            Width => Digits_Width,
            Fill => Digits_Fill,
            Error => Error);
      else
         System.Formatting.Image (
            Word_Abs_Item,
            Result (Last + 1 .. Result'Last),
            Last,
            Base => Base,
            Set =>
               System.Formatting.Type_Set'Enum_Val (Type_Set'Enum_Rep (Set)),
            Width => Digits_Width,
            Fill => Digits_Fill,
            Error => Error);
      end if;
      if Form = Ada and then Base /= 10 then
         Last := Last + 1;
         Result (Last) := '#';
      end if;
      return Result (1 .. Last);
   end Integer_Image;

   function Modular_Image (Item : T) return String is
      Result : String (
         1 .. 4 + Long_Long_Unsigned'Width + Digits_Width); -- "16##"
      Last : Natural := Result'First - 1;
      Error : Boolean;
   begin
      declare
         Sign : Character;
      begin
         if Item > 0 then
            Sign := Signs (1);
         else
            Sign := Signs (0);
         end if;
         if Sign /= No_Sign then
            Last := Last + 1;
            Result (Last) := Sign;
         end if;
      end;
      if Form = Ada and then Base /= 10 then
         System.Formatting.Image (
            Word_Unsigned (Base),
            Result (Last + 1 .. Result'Last),
            Last,
            Error => Error);
         Last := Last + 1;
         Result (Last) := '#';
      end if;
      if T'Size > Standard'Word_Size then
         System.Formatting.Image (
            Long_Long_Unsigned (Item),
            Result (Last + 1 .. Result'Last),
            Last,
            Base => Base,
            Set =>
               System.Formatting.Type_Set'Enum_Val (Type_Set'Enum_Rep (Set)),
            Width => Digits_Width,
            Fill => Digits_Fill,
            Error => Error);
      else
         System.Formatting.Image (
            Word_Unsigned (Item),
            Result (Last + 1 .. Result'Last),
            Last,
            Base => Base,
            Set =>
               System.Formatting.Type_Set'Enum_Val (Type_Set'Enum_Rep (Set)),
            Width => Digits_Width,
            Fill => Digits_Fill,
            Error => Error);
      end if;
      if Form = Ada and then Base /= 10 then
         Last := Last + 1;
         Result (Last) := '#';
      end if;
      return Result (1 .. Last);
   end Modular_Image;

   function Float_Image (Item : T) return String is
      Result : String (
         1 ..
         Fore_Digits_Width + Aft_Width + Exponent_Digits_Width
            + 13); -- 5(15bit exponent) + 8("-16#.#E-")
      Fore_Last, Last : Natural;
   begin
      System.Formatting.Float.Image (
         Long_Long_Float (Item),
         Result,
         Fore_Last,
         Last,
         Signs => (Signs (-1), Signs (0), Signs (1)),
         Base => Base,
         Base_Form => Form = Ada and then Base /= 10,
         Set => System.Formatting.Type_Set'Enum_Val (Type_Set'Enum_Rep (Set)),
         Fore_Digits_Width => Fore_Digits_Width,
         Fore_Digits_Fill => Fore_Digits_Fill,
         Aft_Width => Aft_Width,
         Exponent_Mark => Exponent_Mark,
         Exponent_Signs =>
            (Exponent_Signs (-1), Exponent_Signs (0), Exponent_Signs (1)),
         Exponent_Digits_Width => Exponent_Digits_Width,
         Exponent_Digits_Fill => Exponent_Digits_Fill,
         NaN => NaN,
         Infinity => Infinity);
      return Result (1 .. Last);
   end Float_Image;

   function Fixed_Image (Item : T) return String is
      Result : String (
         1 ..
         Integer'Max (
               System.Formatting.Float.Fore_Digits_Width (
                  Long_Long_Float (T'First),
                  Long_Long_Float (T'Last),
                  Base => Base),
               Fore_Digits_Width)
            + Aft_Width + Exponent_Digits_Width
            + 13); -- 5(15bit exponent) + 8("-16#.#E-")
      Fore_Last, Last : Natural;
   begin
      if Exponent then
         System.Formatting.Float.Image (
            Long_Long_Float (Item),
            Result,
            Fore_Last,
            Last,
            Signs => (Signs (-1), Signs (0), Signs (1)),
            Base => Base,
            Base_Form => Form = Ada and then Base /= 10,
            Set =>
               System.Formatting.Type_Set'Enum_Val (Type_Set'Enum_Rep (Set)),
            Fore_Digits_Width => Fore_Digits_Width,
            Fore_Digits_Fill => Fore_Digits_Fill,
            Aft_Width => Aft_Width,
            Exponent_Mark => Exponent_Mark,
            Exponent_Signs =>
               (Exponent_Signs (-1), Exponent_Signs (0), Exponent_Signs (1)),
            Exponent_Digits_Width => Exponent_Digits_Width,
            Exponent_Digits_Fill => Exponent_Digits_Fill);
      else
         System.Formatting.Fixed.Image (
            Long_Long_Float (Item),
            Result,
            Fore_Last,
            Last,
            Signs => (Signs (-1), Signs (0), Signs (1)),
            Base => Base,
            Base_Form => Form = Ada and then Base /= 10,
            Set =>
               System.Formatting.Type_Set'Enum_Val (Type_Set'Enum_Rep (Set)),
            Fore_Digits_Width => Fore_Digits_Width,
            Fore_Digits_Fill => Fore_Digits_Fill,
            Aft_Width => Aft_Width);
      end if;
      return Result (1 .. Last);
   end Fixed_Image;

   function Decimal_Image (Item : T) return String is
      Result : String (
         1 ..
         Integer'Max (
               System.Formatting.Float.Fore_Digits_Width (
                  Long_Long_Float (T'First),
                  Long_Long_Float (T'Last)),
               Fore_Digits_Width)
            + Aft_Width + Exponent_Digits_Width
            + 13); -- 5(15bit exponent) + 8("-16#.#E-")
      Fore_Last, Last : Natural;
   begin
      if Exponent then
         System.Formatting.Float.Image (
            Long_Long_Float (Item),
            Result,
            Fore_Last,
            Last,
            Signs => (Signs (-1), Signs (0), Signs (1)),
            Fore_Digits_Width => Fore_Digits_Width,
            Fore_Digits_Fill => Fore_Digits_Fill,
            Aft_Width => Aft_Width,
            Exponent_Mark => Exponent_Mark,
            Exponent_Signs =>
               (Exponent_Signs (-1), Exponent_Signs (0), Exponent_Signs (1)),
            Exponent_Digits_Width => Exponent_Digits_Width,
            Exponent_Digits_Fill => Exponent_Digits_Fill);
      else
         System.Formatting.Decimal.Image (
            Long_Long_Integer'Integer_Value (Item),
            Result,
            Fore_Last,
            Last,
            T'Scale,
            Signs => (Signs (-1), Signs (0), Signs (1)),
            Fore_Digits_Width => Fore_Digits_Width,
            Fore_Digits_Fill => Fore_Digits_Fill,
            Aft_Width => Aft_Width);
      end if;
      return Result (1 .. Last);
   end Decimal_Image;

end Ada.Formatting;
