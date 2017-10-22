with System.Long_Long_Integer_Types;
package body System.Formatting.Decimal is
   pragma Suppress (All_Checks);

   procedure Image (
      Value : Long_Long_Integer;
      Item : out String;
      Last : out Natural;
      Scale : Integer;
      Minus_Sign : Character := '-';
      Zero_Sign : Character := ' ';
      Plus_Sign : Character := ' ';
      Fore_Width : Positive := 1;
      Fore_Padding : Character := '0';
      Aft_Width : Natural)
   is
      Error : Boolean;
   begin
      Last := Item'First - 1;
      if Value < 0 then
         if Minus_Sign /= No_Sign then
            Last := Last + 1;
            pragma Assert (Last <= Item'Last);
            Item (Last) := Minus_Sign;
         end if;
      elsif Value > 0 then
         if Plus_Sign /= No_Sign then
            Last := Last + 1;
            pragma Assert (Last <= Item'Last);
            Item (Last) := Plus_Sign;
         end if;
      else
         if Zero_Sign /= No_Sign then
            Last := Last + 1;
            pragma Assert (Last <= Item'Last);
            Item (Last) := Zero_Sign;
         end if;
      end if;
      if Scale > 0 then
         declare
            Rounded_Item : Formatting.Longest_Unsigned :=
               Formatting.Longest_Unsigned'Mod (abs Value);
            Sp : constant Formatting.Longest_Unsigned := 10 ** Scale;
            Q : Formatting.Longest_Unsigned;
            Aft : Formatting.Longest_Unsigned;
            Error : Boolean;
         begin
            if Aft_Width < Scale then
               Rounded_Item := Rounded_Item + (10 ** (Scale - Aft_Width)) / 2;
            end if;
            Long_Long_Integer_Types.Divide (
               Long_Long_Integer_Types.Longest_Unsigned (Rounded_Item),
               Long_Long_Integer_Types.Longest_Unsigned (Sp),
               Long_Long_Integer_Types.Longest_Unsigned (Q),
               Long_Long_Integer_Types.Longest_Unsigned (Aft));
            Formatting.Image (
               Q,
               Item (Last + 1 .. Item'Last),
               Last,
               Width => Fore_Width,
               Padding => Fore_Padding,
               Error => Error);
            pragma Assert (not Error);
            if Aft_Width > 0 then
               Last := Last + 1;
               pragma Assert (Last <= Item'Last);
               Item (Last) := '.';
               if Aft_Width > Scale then
                  Aft := Aft * 10 ** (Aft_Width - Scale);
               elsif Aft_Width < Scale then
                  Aft := Aft / 10 ** (Scale - Aft_Width);
               end if;
               Formatting.Image (
                  Aft,
                  Item (Last + 1 .. Item'Last),
                  Last,
                  Width => Aft_Width,
                  Error => Error);
               pragma Assert (not Error);
            end if;
         end;
      else
         if Value /= 0 then
            Formatting.Image (
               Formatting.Longest_Unsigned (abs Value),
               Item (Last + 1 .. Item'Last),
               Last,
               Width => Fore_Width,
               Padding => Fore_Padding,
               Error => Error);
            pragma Assert (not Error);
            pragma Assert (Last - Scale <= Item'Last);
            Fill_Padding (Item (Last + 1 .. Last - Scale), '0');
            Last := Last - Scale;
         else
            pragma Assert (Last + Fore_Width <= Item'Last);
            Fill_Padding (
               Item (Last + 1 .. Last + Fore_Width - 1),
               Fore_Padding);
            Last := Last + Fore_Width; -- including '0'
            Item (Last) := '0';
         end if;
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
