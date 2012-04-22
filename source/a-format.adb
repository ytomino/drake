with System.Formatting.Decimal_Image;
with System.Formatting.Float_Image;
with System.Formatting.Fixed_Image;
package body Ada.Formatting is
   pragma Suppress (All_Checks);

   pragma Compile_Time_Error (No_Sign /= System.Formatting.No_Sign,
      "No_Sign mismatch");

   function Integer_Image (Item : T) return String is
      Result : String (
         1 ..
         4 + Long_Long_Integer'Width + Width); -- "16##"
      Last : Natural := Result'First - 1;
      Error : Boolean;
   begin
      if Item < 0 then
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
            System.Formatting.Longest_Unsigned (abs Item),
            Result (Last + 1 .. Result'Last),
            Last,
            Base => Base,
            Casing => System.Formatting.Casing_Type'Enum_Val (
               Casing_Type'Enum_Rep (Casing)),
            Width => Width,
            Padding => Padding,
            Error => Error);
      else
         System.Formatting.Image (
            System.Formatting.Unsigned (abs Item),
            Result (Last + 1 .. Result'Last),
            Last,
            Base => Base,
            Casing => System.Formatting.Casing_Type'Enum_Val (
               Casing_Type'Enum_Rep (Casing)),
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
            Casing => System.Formatting.Casing_Type'Enum_Val (
               Casing_Type'Enum_Rep (Casing)),
            Width => Width,
            Padding => Padding,
            Error => Error);
      else
         System.Formatting.Image (
            System.Formatting.Unsigned (Item),
            Result (Last + 1 .. Result'Last),
            Last,
            Base => Base,
            Casing => System.Formatting.Casing_Type'Enum_Val (
               Casing_Type'Enum_Rep (Casing)),
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
      System.Formatting.Float_Image (
         Result,
         Last,
         Long_Long_Float (Item),
         Minus_Sign => Signs (-1),
         Zero_Sign => Signs (0),
         Plus_Sign => Signs (1),
         Base => Base,
         Base_Form => Form = Ada and then Base /= 10,
         Casing => System.Formatting.Casing_Type'Enum_Val (
            Casing_Type'Enum_Rep (Casing)),
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
         Integer'Max (T'Fore, Fore_Width + 1) +
            Aft_Width + Exponent_Width + 7); -- (16#.#/16#.#E-)
      Last : Natural;
   begin
      if Exponent then
         System.Formatting.Float_Image (
            Result,
            Last,
            Long_Long_Float (Item),
            Minus_Sign => Signs (-1),
            Zero_Sign => Signs (0),
            Plus_Sign => Signs (1),
            Base => Base,
            Base_Form => Form = Ada and then Base /= 10,
            Casing => System.Formatting.Casing_Type'Enum_Val (
               Casing_Type'Enum_Rep (Casing)),
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
         System.Formatting.Fixed_Image (
            Result,
            Last,
            Long_Long_Float (Item),
            Minus_Sign => Signs (-1),
            Zero_Sign => Signs (0),
            Plus_Sign => Signs (1),
            Base => Base,
            Base_Form => Form = Ada and then Base /= 10,
            Casing => System.Formatting.Casing_Type'Enum_Val (
               Casing_Type'Enum_Rep (Casing)),
            Fore_Width => Fore_Width,
            Fore_Padding => Fore_Padding,
            Aft_Width => Aft_Width);
      end if;
      return Result (1 .. Last);
   end Fixed_Image;

   function Decimal_Image (Item : T) return String is
      Result : String (
         1 ..
         Integer'Max (T'Fore, Fore_Width + 1) +
            Aft_Width + Exponent_Width + 7); -- (./16#.#E+)
      Last : Natural;
   begin
      if Exponent then
         System.Formatting.Float_Image (
            Result,
            Last,
            Long_Long_Float (Item),
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
         System.Formatting.Decimal_Image (
            Result,
            Last,
            Long_Long_Integer'Integer_Value (Item),
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
