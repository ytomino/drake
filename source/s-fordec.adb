package body System.Formatting.Decimal is
   pragma Suppress (All_Checks);
   use type Long_Long_Integer_Types.Long_Long_Unsigned;

   subtype Word_Unsigned is Long_Long_Integer_Types.Word_Unsigned;
   subtype Long_Long_Unsigned is Long_Long_Integer_Types.Long_Long_Unsigned;

   --  implementation

   procedure Image (
      Value : Long_Long_Integer;
      Item : out String;
      Fore_Last, Last : out Natural;
      Scale : Integer;
      Signs : Sign_Marks := ('-', ' ', ' ');
      Fore_Digits_Width : Positive := 1;
      Fore_Digits_Fill : Character := '0';
      Aft_Width : Natural)
   is
      Abs_Value : Long_Long_Unsigned;
      Error : Boolean;
   begin
      Last := Item'First - 1;
      declare
         Sign : Character;
      begin
         if Value < 0 then
            Abs_Value := -Long_Long_Unsigned'Mod (Value);
            Sign := Signs.Minus;
         else
            Abs_Value := Long_Long_Unsigned (Value);
            if Value > 0 then
               Sign := Signs.Plus;
            else
               Sign := Signs.Zero;
            end if;
         end if;
         if Sign /= No_Sign then
            Last := Last + 1;
            pragma Assert (Last <= Item'Last);
            Item (Last) := Sign;
         end if;
      end;
      if Scale > 0 then
         declare
            Rounded_Item : Long_Long_Unsigned := Abs_Value;
            Sp : constant Long_Long_Unsigned := 10 ** Scale;
            Q : Long_Long_Unsigned;
            Aft : Long_Long_Unsigned;
            Error : Boolean;
         begin
            if Aft_Width < Scale then
               Rounded_Item := Rounded_Item + (10 ** (Scale - Aft_Width)) / 2;
            end if;
            Long_Long_Integer_Types.Divide (Rounded_Item, Sp, Q, Aft);
            if Long_Long_Integer'Size <= Word_Unsigned'Size then
               Formatting.Image (
                  Word_Unsigned (Q),
                  Item (Last + 1 .. Item'Last),
                  Last,
                  Width => Fore_Digits_Width,
                  Fill => Fore_Digits_Fill,
                  Error => Error);
            else
               Formatting.Image (
                  Q,
                  Item (Last + 1 .. Item'Last),
                  Last,
                  Width => Fore_Digits_Width,
                  Fill => Fore_Digits_Fill,
                  Error => Error);
            end if;
            pragma Assert (not Error);
            Fore_Last := Last;
            if Aft_Width > 0 then
               Last := Last + 1;
               pragma Assert (Last <= Item'Last);
               Item (Last) := '.';
               if Aft_Width > Scale then
                  Aft := Aft * 10 ** (Aft_Width - Scale);
               elsif Aft_Width < Scale then
                  Aft := Aft / 10 ** (Scale - Aft_Width);
               end if;
               if Long_Long_Integer'Size <= Word_Unsigned'Size then
                  Formatting.Image (
                     Word_Unsigned (Aft),
                     Item (Last + 1 .. Item'Last),
                     Last,
                     Width => Aft_Width,
                     Error => Error);
               else
                  Formatting.Image (
                     Aft,
                     Item (Last + 1 .. Item'Last),
                     Last,
                     Width => Aft_Width,
                     Error => Error);
               end if;
               pragma Assert (not Error);
            end if;
         end;
      else
         if Abs_Value /= 0 then
            if Long_Long_Integer'Size <= Standard'Word_Size then
               Formatting.Image (
                  Word_Unsigned (Abs_Value),
                  Item (Last + 1 .. Item'Last),
                  Last,
                  Width => Fore_Digits_Width,
                  Fill => Fore_Digits_Fill,
                  Error => Error);
            else
               Formatting.Image (
                  Abs_Value,
                  Item (Last + 1 .. Item'Last),
                  Last,
                  Width => Fore_Digits_Width,
                  Fill => Fore_Digits_Fill,
                  Error => Error);
            end if;
            pragma Assert (not Error);
            pragma Assert (Last - Scale <= Item'Last);
            Fill_Padding (Item (Last + 1 .. Last - Scale), '0');
            Last := Last - Scale;
         else
            pragma Assert (Last + Fore_Digits_Width <= Item'Last);
            Fill_Padding (
               Item (Last + 1 .. Last + Fore_Digits_Width - 1),
               Fore_Digits_Fill);
            Last := Last + Fore_Digits_Width; -- including '0'
            Item (Last) := '0';
         end if;
         Fore_Last := Last;
         if Aft_Width > 0 then
            Last := Last + 1;
            pragma Assert (Last <= Item'Last);
            Item (Last) := '.';
            pragma Assert (Last + Aft_Width <= Item'Last);
            Fill_Padding (Item (Last + 1 .. Last + Aft_Width), '0');
            Last := Last + Aft_Width;
         end if;
      end if;
   end Image;

end System.Formatting.Decimal;
