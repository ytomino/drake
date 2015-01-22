with System.Long_Long_Integer_Divide;
package body System.Formatting.Decimal is

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
      pragma Suppress (All_Checks);
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
            Rounded_Item : Long_Long_Integer := abs Value;
            Sp : constant Long_Long_Integer := 10 ** Scale;
            Q : Long_Long_Integer;
            R : Long_Long_Integer;
            Aft : Formatting.Longest_Unsigned;
            Error : Boolean;
         begin
            if Aft_Width < Scale then
               Rounded_Item := Rounded_Item + (10 ** (Scale - Aft_Width)) / 2;
            end if;
            Long_Long_Integer_Divide (Rounded_Item, Sp, Q, R);
            Aft := Formatting.Longest_Unsigned (R);
            Formatting.Image (
               Formatting.Longest_Unsigned (Q),
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
            for I in Scale .. -1 loop
               pragma Assert (Last + 1 <= Item'Last);
               Last := Last + 1;
               Item (Last) := '0';
            end loop;
         else
            for I in 2 .. Fore_Width loop
               Last := Last + 1;
               pragma Assert (Last <= Item'Last);
               Item (Last) := Fore_Padding;
            end loop;
            Last := Last + 1;
            pragma Assert (Last <= Item'Last);
            Item (Last) := '0';
         end if;
         if Aft_Width > 0 then
            Last := Last + 1;
            pragma Assert (Last <= Item'Last);
            Item (Last) := '.';
            for I in Last + 1 .. Last + Aft_Width loop
               Last := Last + 1;
               pragma Assert (Last <= Item'Last);
               Item (Last) := '0';
            end loop;
         end if;
      end if;
   end Image;

end System.Formatting.Decimal;
