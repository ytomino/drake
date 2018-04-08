with System.Formatting.Float;
package body System.Formatting.Fixed is
   pragma Suppress (All_Checks);

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
      Aft_Width : Positive) is
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
      Float.Image_No_Sign_Nor_Exponent (
         Value,
         Item (Last + 1 .. Item'Last),
         Fore_Last,
         Last,
         Base => Base,
         Base_Form => Base_Form,
         Set => Set,
         Fore_Digits_Width => Fore_Digits_Width,
         Fore_Digits_Fill => Fore_Digits_Fill,
         Aft_Width => Aft_Width);
   end Image;

end System.Formatting.Fixed;
