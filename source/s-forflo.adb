package body System.Formatting.Float is
   pragma Suppress (All_Checks);

   procedure Split (
      X : Longest_Unsigned_Float;
      Fore : out Unsigned;
      Aft : out Longest_Unsigned_Float;
      Exponent : out Integer;
      Base : Number_Base := 10) is
   begin
      if X > 0.0 then
         if X >= Longest_Unsigned_Float (Base) then
            declare
               B : Longest_Unsigned_Float := Longest_Unsigned_Float (Base);
            begin
               Exponent := 1;
               loop
                  declare
                     Next_B : constant Longest_Unsigned_Float :=
                        B * Longest_Unsigned_Float (Base);
                  begin
                     exit when Next_B > X;
                     B := Next_B;
                  end;
                  Exponent := Exponent + 1;
               end loop;
               Aft := X / B;
            end;
         else
            Aft := X;
            Exponent := 0;
            while Aft < 1.0 loop
               Aft := Aft * Longest_Unsigned_Float (Base);
               Exponent := Exponent - 1;
            end loop;
         end if;
         declare
            Fore_Float : constant Longest_Unsigned_Float :=
               Longest_Unsigned_Float'Truncation (Aft);
         begin
            Aft := Aft - Fore_Float;
            Fore := Unsigned (Fore_Float);
         end;
      else
         Fore := 0;
         Aft := 0.0;
         Exponent := 0;
      end if;
   end Split;

   procedure Aft_Image (
      Value : Longest_Unsigned_Float;
      Item : out String;
      Last : out Natural;
      Base : Number_Base := 10;
      Casing : Casing_Type := Upper;
      Width : Positive := Standard.Float'Digits - 1)
   is
      X : Long_Long_Float := Value;
      Int_Part : Long_Long_Float;
   begin
      Last := Item'First;
      Item (Last) := '.';
      for I in 2 .. Width loop --  drop last one
         X := X * Longest_Unsigned_Float (Base);
         Int_Part := Longest_Unsigned_Float'Truncation (X);
         X := X - Int_Part;
         Last := Last + 1;
         Image (
            Unsigned (Int_Part),
            Item (Last),
            Casing);
      end loop;
      X := X * Longest_Unsigned_Float (Base);
      Last := Last + 1;
      Image (
         Unsigned (Longest_Unsigned_Float'Rounding (X)), -- last one
         Item (Last),
         Casing);
   end Aft_Image;

end System.Formatting.Float;
