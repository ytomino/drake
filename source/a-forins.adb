package body Ada.Formatting.Inside is
   pragma Suppress (All_Checks);
   use type System.Formatting.Unsigned;
   use type System.Formatting.Longest_Unsigned;

   procedure Integer_Image (
      To : out String;
      Last : out Natural;
      Item : Integer;
      Minus_Sign : Character := '-';
      Zero_Sign : Character := ' ';
      Plus_Sign : Character := ' ';
      Base : Number_Base := 10;
      Casing : Casing_Type := Upper;
      Width : Positive := 1;
      Padding : Character := '0')
   is
      pragma Assert (To'Length >= Integer_Width);
      Error : Boolean;
   begin
      Last := To'First - 1;
      if Item < 0 then
         if Minus_Sign /= None then
            Last := Last + 1;
            To (Last) := Minus_Sign;
         end if;
      elsif Item > 0 then
         if Plus_Sign /= None then
            Last := Last + 1;
            To (Last) := Plus_Sign;
         end if;
      else
         if Zero_Sign /= None then
            Last := Last + 1;
            To (Last) := Zero_Sign;
         end if;
      end if;
      if Base /= 10 then
         System.Formatting.Image (
            System.Formatting.Unsigned (Base),
            To (Last + 1 .. To'Last),
            Last,
            Error => Error);
         Last := Last + 1;
         To (Last) := '#';
      end if;
      System.Formatting.Image (
         System.Formatting.Unsigned (abs Item),
         To (Last + 1 .. To'Last),
         Last,
         Base => Base,
         Casing => System.Formatting.Casing_Type'Enum_Val (
            Casing_Type'Enum_Rep (Casing)),
         Width => Width,
         Padding => Padding,
         Error => Error);
      if Base /= 10 then
         Last := Last + 1;
         To (Last) := '#';
      end if;
   end Integer_Image;

   procedure Integer_Image (
      To : out String;
      Last : out Natural;
      Item : Long_Long_Integer;
      Minus_Sign : Character := '-';
      Zero_Sign : Character := ' ';
      Plus_Sign : Character := ' ';
      Base : Number_Base := 10;
      Casing : Casing_Type := Upper;
      Width : Positive := 1;
      Padding : Character := '0')
   is
      Error : Boolean;
   begin
      Last := To'First - 1;
      if Item < 0 then
         if Minus_Sign /= None then
            Last := Last + 1;
            pragma Assert (Last <= To'Last);
            To (Last) := Minus_Sign;
         end if;
      elsif Item > 0 then
         if Plus_Sign /= None then
            Last := Last + 1;
            pragma Assert (Last <= To'Last);
            To (Last) := Plus_Sign;
         end if;
      else
         if Zero_Sign /= None then
            Last := Last + 1;
            pragma Assert (Last <= To'Last);
            To (Last) := Zero_Sign;
         end if;
      end if;
      if Base /= 10 then
         System.Formatting.Image (
            System.Formatting.Unsigned (Base),
            To (Last + 1 .. To'Last),
            Last,
            Error => Error);
         pragma Assert (not Error);
         Last := Last + 1;
         pragma Assert (Last <= To'Last);
         To (Last) := '#';
      end if;
      System.Formatting.Image (
         System.Formatting.Longest_Unsigned (abs Item),
         To (Last + 1 .. To'Last),
         Last,
         Base => Base,
         Casing => System.Formatting.Casing_Type'Enum_Val (
            Casing_Type'Enum_Rep (Casing)),
         Width => Width,
         Padding => Padding,
         Error => Error);
      pragma Assert (not Error);
      if Base /= 10 then
         Last := Last + 1;
         pragma Assert (Last <= To'Last);
         To (Last) := '#';
      end if;
   end Integer_Image;

   procedure Modular_Image (
      To : out String;
      Last : out Natural;
      Item : System.Formatting.Unsigned;
      Zero_Sign : Character := ' ';
      Plus_Sign : Character := ' ';
      Base : Number_Base := 10;
      Casing : Casing_Type := Upper;
      Width : Positive := 1;
      Padding : Character := '0')
   is
      pragma Assert (To'Length >= Modular_Width);
      Error : Boolean;
   begin
      Last := To'First - 1;
      if Item > 0 then
         if Plus_Sign /= None then
            Last := Last + 1;
            To (Last) := Plus_Sign;
         end if;
      else
         if Zero_Sign /= None then
            Last := Last + 1;
            To (Last) := Zero_Sign;
         end if;
      end if;
      if Base /= 10 then
         System.Formatting.Image (
            System.Formatting.Unsigned (Base),
            To (Last + 1 .. To'Last),
            Last,
            Error => Error);
         Last := Last + 1;
         To (Last) := '#';
      end if;
      System.Formatting.Image (
         Item,
         To (Last + 1 .. To'Last),
         Last,
         Base => Base,
         Casing => System.Formatting.Casing_Type'Enum_Val (
            Casing_Type'Enum_Rep (Casing)),
         Width => Width,
         Padding => Padding,
         Error => Error);
      if Base /= 10 then
         Last := Last + 1;
         To (Last) := '#';
      end if;
   end Modular_Image;

   procedure Modular_Image (
      To : out String;
      Last : out Natural;
      Item : System.Formatting.Longest_Unsigned;
      Zero_Sign : Character := ' ';
      Plus_Sign : Character := ' ';
      Base : Number_Base := 10;
      Casing : Casing_Type := Upper;
      Width : Positive := 1;
      Padding : Character := '0')
   is
      pragma Assert (To'Length >= Modular_Width);
      Error : Boolean;
   begin
      Last := To'First - 1;
      if Item > 0 then
         if Plus_Sign /= None then
            Last := Last + 1;
            To (Last) := Plus_Sign;
         end if;
      else
         if Zero_Sign /= None then
            Last := Last + 1;
            To (Last) := Zero_Sign;
         end if;
      end if;
      if Base /= 10 then
         System.Formatting.Image (
            System.Formatting.Unsigned (Base),
            To (Last + 1 .. To'Last),
            Last,
            Error => Error);
         Last := Last + 1;
         To (Last) := '#';
      end if;
      System.Formatting.Image (
         Item,
         To (Last + 1 .. To'Last),
         Last,
         Base => Base,
         Casing => System.Formatting.Casing_Type'Enum_Val (
            Casing_Type'Enum_Rep (Casing)),
         Width => Width,
         Padding => Padding,
         Error => Error);
      if Base /= 10 then
         Last := Last + 1;
         To (Last) := '#';
      end if;
   end Modular_Image;

end Ada.Formatting.Inside;
