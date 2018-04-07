with System.Formatting.Float;
with System.Long_Long_Float_Types;
package body System.Formatting.Fixed is
   pragma Suppress (All_Checks);

   subtype Word_Unsigned is Long_Long_Integer_Types.Word_Unsigned;

   function modfl (value : Long_Long_Float; iptr : access Long_Long_Float)
      return Long_Long_Float
      with Import, Convention => Intrinsic, External_Name => "__builtin_modfl";

   --  implementation

   procedure Image (
      Value : Long_Long_Float;
      Item : out String;
      Fore_Last, Last : out Natural;
      Signs : Sign_Marks := ('-', ' ', ' ');
      Base : Number_Base := 10;
      Base_Form : Boolean := False;
      Set : Type_Set := Upper_Case;
      Fore_Digits_Width : Positive := 1;
      Fore_Digits_Fill : Character := '0';
      Aft_Width : Positive)
   is
      Item_Fore : aliased Long_Long_Float;
      Aft : Long_Long_Float;
      Scaled_Aft : Long_Long_Float;
      Round_Up : Boolean;
      Required_Fore_Width : Positive;
      Error : Boolean;
   begin
      Last := Item'First - 1;
      declare
         Sign : constant Character := Float.Sign_Mark (Value, Signs);
      begin
         if Sign /= No_Sign then
            Last := Last + 1;
            pragma Assert (Last <= Item'Last);
            Item (Last) := Sign;
         end if;
      end;
      --  opening '#'
      if Base_Form then
         Image (
            Word_Unsigned (Base),
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
         Aft_Width => Aft_Width);
      if Round_Up then
         Item_Fore := Item_Fore + 1.0;
         Scaled_Aft := 0.0;
      end if;
      --  integer part
      Required_Fore_Width := Float.Fore_Digits_Width (Item_Fore, Base => Base);
      if Fore_Digits_Width > Required_Fore_Width then
         pragma Assert (
            Last + Fore_Digits_Width - Required_Fore_Width <= Item'Last);
         Fill_Padding (
            Item (Last + 1 .. Last + Fore_Digits_Width - Required_Fore_Width),
            Fore_Digits_Fill);
         Last := Last + Fore_Digits_Width - Required_Fore_Width;
      end if;
      pragma Assert (Last + Required_Fore_Width <= Item'Last);
      for I in reverse Last + 1 .. Last + Required_Fore_Width loop
         declare
            Q : Long_Long_Float;
            R : Long_Long_Float;
         begin
            Long_Long_Float_Types.Divide (
               Item_Fore,
               Long_Long_Float (Base),
               Q,
               R);
            Image (Digit (R), Item (I), Set => Set);
            Item_Fore := Q;
         end;
      end loop;
      Last := Last + Required_Fore_Width;
      Fore_Last := Last;
      --  '.' and decimal part
      pragma Assert (Last + Aft_Width <= Item'Last);
      Float.Aft_Image (
         Scaled_Aft,
         Item (Last + 1 .. Item'Last),
         Last,
         Base => Base,
         Set => Set,
         Aft_Width => Aft_Width);
      --  closing '#'
      if Base_Form then
         Last := Last + 1;
         pragma Assert (Last <= Item'Last);
         Item (Last) := '#';
      end if;
   end Image;

end System.Formatting.Fixed;
