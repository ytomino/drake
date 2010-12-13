with Ada.Formatting.Inside;
with System.Formatting.Decimal_Image;
with System.Formatting.Float_Image;
with System.Formatting.Fixed_Image;
package body Ada.Formatting is
   pragma Suppress (All_Checks);

   function Integer_Image (Item : T) return String is
      Result : String (1 .. Inside.Integer_Width + Width);
      Last : Natural;
   begin
      case Form is
         when Simple =>
            Last := Result'First - 1;
            declare
               Error : Boolean;
            begin
               if Item < 0 then
                  if Minus_Sign /= None then
                     Last := Last + 1;
                     Result (Last) := Minus_Sign;
                  end if;
               elsif Item > 0 then
                  if Plus_Sign /= None then
                     Last := Last + 1;
                     Result (Last) := Plus_Sign;
                  end if;
               else
                  if Zero_Sign /= None then
                     Last := Last + 1;
                     Result (Last) := Zero_Sign;
                  end if;
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
            end;
         when Ada =>
            if T'Size > Integer'Size then
               Inside.Integer_Image (
                  Result,
                  Last,
                  Long_Long_Integer (Item),
                  Minus_Sign => Minus_Sign,
                  Zero_Sign => Zero_Sign,
                  Plus_Sign => Plus_Sign,
                  Base => Base,
                  Casing => Casing,
                  Width => Width,
                  Padding => Padding);
            else
               Inside.Integer_Image (
                  Result,
                  Last,
                  Integer (Item),
                  Minus_Sign => Minus_Sign,
                  Zero_Sign => Zero_Sign,
                  Plus_Sign => Plus_Sign,
                  Base => Base,
                  Casing => Casing,
                  Width => Width,
                  Padding => Padding);
            end if;
      end case;
      return Result (1 .. Last);
   end Integer_Image;

   function Modular_Image (Item : T) return String is
      Result : String (1 .. Inside.Integer_Width + Width);
      Last : Natural;
   begin
      case Form is
         when Simple =>
            Last := Result'First - 1;
            declare
               Error : Boolean;
            begin
               if Item > 0 then
                  if Plus_Sign /= None then
                     Last := Last + 1;
                     Result (Last) := Plus_Sign;
                  end if;
               else
                  if Zero_Sign /= None then
                     Last := Last + 1;
                     Result (Last) := Zero_Sign;
                  end if;
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
            end;
         when Ada =>
            if T'Size > System.Formatting.Unsigned'Size then
               Inside.Modular_Image (
                  Result,
                  Last,
                  System.Formatting.Longest_Unsigned (Item),
                  Zero_Sign => Zero_Sign,
                  Plus_Sign => Plus_Sign,
                  Base => Base,
                  Casing => Casing,
                  Width => Width,
                  Padding => Padding);
            else
               Inside.Modular_Image (
                  Result,
                  Last,
                  System.Formatting.Unsigned (Item),
                  Zero_Sign => Zero_Sign,
                  Plus_Sign => Plus_Sign,
                  Base => Base,
                  Casing => Casing,
                  Width => Width,
                  Padding => Padding);
            end if;
      end case;
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
         Minus_Sign => Minus_Sign,
         Zero_Sign => Zero_Sign,
         Plus_Sign => Plus_Sign,
         Base => Base,
         Base_Form => Form = Ada and then Base /= 10,
         Casing => System.Formatting.Casing_Type'Enum_Val (
            Casing_Type'Enum_Rep (Casing)),
         Fore_Width => Fore_Width,
         Fore_Padding => Fore_Padding,
         Aft_Width => Aft_Width,
         Exponent_Mark => Exponent_Mark,
         Exponent_Minus_Sign => Exponent_Minus_Sign,
         Exponent_Zero_Sign => Exponent_Zero_Sign,
         Exponent_Plus_Sign => Exponent_Plus_Sign,
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
            Minus_Sign => Minus_Sign,
            Zero_Sign => Zero_Sign,
            Plus_Sign => Plus_Sign,
            Base => Base,
            Base_Form => Form = Ada and then Base /= 10,
            Casing => System.Formatting.Casing_Type'Enum_Val (
               Casing_Type'Enum_Rep (Casing)),
            Fore_Width => Fore_Width,
            Fore_Padding => Fore_Padding,
            Aft_Width => Aft_Width,
            Exponent_Mark => Exponent_Mark,
            Exponent_Minus_Sign => Exponent_Minus_Sign,
            Exponent_Zero_Sign => Exponent_Zero_Sign,
            Exponent_Plus_Sign => Exponent_Plus_Sign,
            Exponent_Width => Exponent_Width,
            Exponent_Padding => Exponent_Padding);
      else
         System.Formatting.Fixed_Image (
            Result,
            Last,
            Long_Long_Float (Item),
            Minus_Sign => Minus_Sign,
            Zero_Sign => Zero_Sign,
            Plus_Sign => Plus_Sign,
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
            Minus_Sign => Minus_Sign,
            Zero_Sign => Zero_Sign,
            Plus_Sign => Plus_Sign,
            Fore_Width => Fore_Width,
            Fore_Padding => Fore_Padding,
            Aft_Width => Aft_Width,
            Exponent_Mark => Exponent_Mark,
            Exponent_Minus_Sign => Exponent_Minus_Sign,
            Exponent_Zero_Sign => Exponent_Zero_Sign,
            Exponent_Plus_Sign => Exponent_Plus_Sign,
            Exponent_Width => Exponent_Width,
            Exponent_Padding => Exponent_Padding);
      else
         System.Formatting.Decimal_Image (
            Result,
            Last,
            Long_Long_Integer'Integer_Value (Item),
            T'Scale,
            Minus_Sign => Minus_Sign,
            Zero_Sign => Zero_Sign,
            Plus_Sign => Plus_Sign,
            Fore_Width => Fore_Width,
            Fore_Padding => Fore_Padding,
            Aft_Width => Aft_Width);
      end if;
      return Result (1 .. Last);
   end Decimal_Image;

end Ada.Formatting;
