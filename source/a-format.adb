with System.Formatting.Decimal;
with System.Formatting.Fixed;
with System.Formatting.Float;
package body Ada.Formatting is
   pragma Suppress (All_Checks);

   pragma Compile_Time_Error (No_Sign /= System.Formatting.No_Sign,
      "No_Sign mismatch");

   function Integer_Image (Item : T) return String is
      Abs_Item : T := Item;
      Result : String (
         1 ..
         4 + Long_Long_Integer'Width + Width); -- "16##"
      Last : Natural := Result'First - 1;
      Error : Boolean;
   begin
      if Item < 0 then
         Abs_Item := -Item;
         if Signs (-1) /= No_Sign then
            Last := Last + 1;
            Result (Last) := Signs (-1);
         end if;
      elsif Item > 0 then
         if Signs (1) /= No_Sign then
            Last := Last + 1;
            Result (Last) := Signs (1);
         end if;
      else
         if Signs (0) /= No_Sign then
            Last := Last + 1;
            Result (Last) := Signs (0);
         end if;
      end if;
      if Form = Ada and then Base /= 10 then
         System.Formatting.Image (
            System.Formatting.Unsigned (Base),
            Result (Last + 1 .. Result'Last),
            Last,
            Error => Error);
         Last := Last + 1;
         Result (Last) := '#';
      end if;
      if T'Size > System.Formatting.Unsigned'Size then
         System.Formatting.Image (
            System.Formatting.Longest_Unsigned (Abs_Item),
            Result (Last + 1 .. Result'Last),
            Last,
            Base => Base,
            Set => System.Formatting.Type_Set'Enum_Val (
               Type_Set'Enum_Rep (Set)),
            Width => Width,
            Padding => Padding,
            Error => Error);
      else
         System.Formatting.Image (
            System.Formatting.Unsigned (Abs_Item),
            Result (Last + 1 .. Result'Last),
            Last,
            Base => Base,
            Set => System.Formatting.Type_Set'Enum_Val (
               Type_Set'Enum_Rep (Set)),
            Width => Width,
            Padding => Padding,
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
         1 ..
         4 + System.Formatting.Longest_Unsigned'Width + Width); -- "16##"
      Last : Natural := Result'First - 1;
      Error : Boolean;
   begin
      if Item > 0 then
         if Signs (1) /= No_Sign then
            Last := Last + 1;
            Result (Last) := Signs (1);
         end if;
      else
         if Signs (0) /= No_Sign then
            Last := Last + 1;
            Result (Last) := Signs (0);
         end if;
      end if;
      if Form = Ada and then Base /= 10 then
         System.Formatting.Image (
            System.Formatting.Unsigned (Base),
            Result (Last + 1 .. Result'Last),
            Last,
            Error => Error);
         Last := Last + 1;
         Result (Last) := '#';
      end if;
      if T'Size > System.Formatting.Unsigned'Size then
         System.Formatting.Image (
            System.Formatting.Longest_Unsigned (Item),
            Result (Last + 1 .. Result'Last),
            Last,
            Base => Base,
            Set => System.Formatting.Type_Set'Enum_Val (
               Type_Set'Enum_Rep (Set)),
            Width => Width,
            Padding => Padding,
            Error => Error);
      else
         System.Formatting.Image (
            System.Formatting.Unsigned (Item),
            Result (Last + 1 .. Result'Last),
            Last,
            Base => Base,
            Set => System.Formatting.Type_Set'Enum_Val (
               Type_Set'Enum_Rep (Set)),
            Width => Width,
            Padding => Padding,
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
         Fore_Width + Aft_Width + Exponent_Width + 8); -- (-16#.#E-)
      Last : Natural;
   begin
      System.Formatting.Float.Image (
         System.Formatting.Float.Longest_Float (Item),
         Result,
         Last,
         Minus_Sign => Signs (-1),
         Zero_Sign => Signs (0),
         Plus_Sign => Signs (1),
         Base => Base,
         Base_Form => Form = Ada and then Base /= 10,
         Set => System.Formatting.Type_Set'Enum_Val (
            Type_Set'Enum_Rep (Set)),
         Fore_Width => Fore_Width,
         Fore_Padding => Fore_Padding,
         Aft_Width => Aft_Width,
         Exponent_Mark => Exponent_Mark,
         Exponent_Minus_Sign => Exponent_Signs (-1),
         Exponent_Zero_Sign => Exponent_Signs (0),
         Exponent_Plus_Sign => Exponent_Signs (1),
         Exponent_Width => Exponent_Width,
         Exponent_Padding => Exponent_Padding,
         NaN => NaN,
         Infinity => Infinity);
      return Result (1 .. Last);
   end Float_Image;

   function Fixed_Image (Item : T) return String is
      Result : String (
         1 ..
         Integer'Max (
            System.Formatting.Float.Fore_Width (
               Long_Long_Float (T'First),
               Long_Long_Float (T'Last),
               Base => Base),
            Fore_Width + 1)
            + Aft_Width + Exponent_Width + 7); -- (16#.#/16#.#E-)
      Last : Natural;
   begin
      if Exponent then
         System.Formatting.Float.Image (
            System.Formatting.Float.Longest_Float (Item),
            Result,
            Last,
            Minus_Sign => Signs (-1),
            Zero_Sign => Signs (0),
            Plus_Sign => Signs (1),
            Base => Base,
            Base_Form => Form = Ada and then Base /= 10,
            Set => System.Formatting.Type_Set'Enum_Val (
               Type_Set'Enum_Rep (Set)),
            Fore_Width => Fore_Width,
            Fore_Padding => Fore_Padding,
            Aft_Width => Aft_Width,
            Exponent_Mark => Exponent_Mark,
            Exponent_Minus_Sign => Exponent_Signs (-1),
            Exponent_Zero_Sign => Exponent_Signs (0),
            Exponent_Plus_Sign => Exponent_Signs (1),
            Exponent_Width => Exponent_Width,
            Exponent_Padding => Exponent_Padding);
      else
         System.Formatting.Fixed.Image (
            Long_Long_Float (Item),
            Result,
            Last,
            Minus_Sign => Signs (-1),
            Zero_Sign => Signs (0),
            Plus_Sign => Signs (1),
            Base => Base,
            Base_Form => Form = Ada and then Base /= 10,
            Set => System.Formatting.Type_Set'Enum_Val (
               Type_Set'Enum_Rep (Set)),
            Fore_Width => Fore_Width,
            Fore_Padding => Fore_Padding,
            Aft_Width => Aft_Width);
      end if;
      return Result (1 .. Last);
   end Fixed_Image;

   function Decimal_Image (Item : T) return String is
      Result : String (
         1 ..
         Integer'Max (
            System.Formatting.Float.Fore_Width (
               Long_Long_Float (T'First),
               Long_Long_Float (T'Last)),
            Fore_Width + 1)
            + Aft_Width + Exponent_Width + 7); -- (./16#.#E+)
      Last : Natural;
   begin
      if Exponent then
         System.Formatting.Float.Image (
            System.Formatting.Float.Longest_Float (Item),
            Result,
            Last,
            Minus_Sign => Signs (-1),
            Zero_Sign => Signs (0),
            Plus_Sign => Signs (1),
            Fore_Width => Fore_Width,
            Fore_Padding => Fore_Padding,
            Aft_Width => Aft_Width,
            Exponent_Mark => Exponent_Mark,
            Exponent_Minus_Sign => Exponent_Signs (-1),
            Exponent_Zero_Sign => Exponent_Signs (0),
            Exponent_Plus_Sign => Exponent_Signs (1),
            Exponent_Width => Exponent_Width,
            Exponent_Padding => Exponent_Padding);
      else
         System.Formatting.Decimal.Image (
            Long_Long_Integer'Integer_Value (Item),
            Result,
            Last,
            T'Scale,
            Minus_Sign => Signs (-1),
            Zero_Sign => Signs (0),
            Plus_Sign => Signs (1),
            Fore_Width => Fore_Width,
            Fore_Padding => Fore_Padding,
            Aft_Width => Aft_Width);
      end if;
      return Result (1 .. Last);
   end Decimal_Image;

end Ada.Formatting;
