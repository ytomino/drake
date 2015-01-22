with System.Formatting.Float;
with System.Long_Long_Float_Divide;
package body System.Formatting.Fixed is

   procedure Image (
      Value : Long_Long_Float;
      Item : out String;
      Last : out Natural;
      Minus_Sign : Character := '-';
      Zero_Sign : Character := ' ';
      Plus_Sign : Character := ' ';
      Base : Number_Base := 10;
      Base_Form : Boolean := False;
      Set : Type_Set := Upper_Case;
      Fore_Width : Positive := 1;
      Fore_Padding : Character := '0';
      Aft_Width : Positive)
   is
      pragma Suppress (All_Checks);
      function signbit (X : Long_Long_Float) return Integer;
      pragma Import (Intrinsic, signbit, "__builtin_signbitl");
      function modfl (value : Long_Long_Float; iptr : access Long_Long_Float)
         return Long_Long_Float;
      pragma Import (Intrinsic, modfl, "__builtin_modfl");
      Item_Fore : aliased Long_Long_Float;
      Aft : Long_Long_Float;
      Scaled_Aft : Long_Long_Float;
      Round_Up : Boolean;
      Required_Fore_Width : Positive;
      Error : Boolean;
   begin
      Last := Item'First - 1;
      if signbit (Value) /= 0 then
         if Minus_Sign /= No_Sign then
            Last := Last + 1;
            pragma Assert (Last <= Item'Last);
            Item (Last) := Minus_Sign;
         end if;
      elsif Value > 0.0 then
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
      --  opening '#'
      if Base_Form then
         Image (
            Unsigned (Base),
            Item (Last + 1 .. Item'Last),
            Last,
            Error => Error);
         pragma Assert (not Error);
         Last := Last + 1;
         pragma Assert (Last <= Item'Last);
         Item (Last) := '#';
      end if;
      --  split
      Aft := modfl (abs Value, Item_Fore'Access);
      Float.Aft_Scale (
         Aft,
         Scaled_Aft,
         0,
         Round_Up,
         Base => Base,
         Width => Aft_Width);
      if Round_Up then
         Item_Fore := Item_Fore + 1.0;
         Scaled_Aft := 0.0;
      end if;
      --  integer part
      Required_Fore_Width := Float.Fore_Width (Item_Fore, Base => Base);
      for I in Required_Fore_Width + 1 .. Fore_Width loop
         Last := Last + 1;
         pragma Assert (Last <= Item'Last);
         Item (Last) := Fore_Padding;
      end loop;
      pragma Assert (Last + Required_Fore_Width <= Item'Last);
      for I in reverse Last + 1 .. Last + Required_Fore_Width loop
         declare
            Q : Long_Long_Float;
            R : Long_Long_Float;
         begin
            Long_Long_Float_Divide (Item_Fore, Long_Long_Float (Base), Q, R);
            Image (
               Digit (R),
               Item (I),
               Set => Set);
            Item_Fore := Q;
         end;
      end loop;
      Last := Last + Required_Fore_Width;
      --  '.' and decimal part
      pragma Assert (Last + Aft_Width <= Item'Last);
      Float.Aft_Image (
         Scaled_Aft,
         Item (Last + 1 .. Item'Last),
         Last,
         Base => Base,
         Set => Set,
         Width => Aft_Width);
      --  closing '#'
      if Base_Form then
         Last := Last + 1;
         pragma Assert (Last <= Item'Last);
         Item (Last) := '#';
      end if;
   end Image;

end System.Formatting.Fixed;
