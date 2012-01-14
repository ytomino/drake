with System.Long_Long_Float_Divide;
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
               declare
                  Scaled : constant Longest_Unsigned_Float := X / B;
                  Fore_Float : constant Longest_Unsigned_Float :=
                     Longest_Unsigned_Float'Truncation (Scaled);
               begin
                  Fore := Unsigned (Fore_Float);
                  Aft := X - Fore_Float * B;
               end;
            end;
         else
            declare
               Scaled : Longest_Unsigned_Float := X;
               B : Longest_Unsigned_Float := 1.0;
            begin
               Exponent := 0;
               while Scaled < 1.0 loop
                  Scaled := Scaled * Longest_Unsigned_Float (Base);
                  B := B * Longest_Unsigned_Float (Base);
                  Exponent := Exponent - 1;
               end loop;
               declare
                  Fore_Float : constant Longest_Unsigned_Float :=
                     Longest_Unsigned_Float'Truncation (Scaled);
               begin
                  Fore := Unsigned (Fore_Float);
                  Aft := X - Fore_Float / B;
               end;
            end;
         end if;
      else
         Fore := 0;
         Aft := 0.0;
         Exponent := 0;
      end if;
   end Split;

   procedure Aft_Image (
      Value : Longest_Unsigned_Float;
      Exponent : Integer;
      Item : out String;
      Last : out Natural;
      Base : Number_Base := 10;
      Casing : Casing_Type := Upper;
      Width : Positive := Standard.Float'Digits - 1)
   is
      X : Longest_Unsigned_Float;
   begin
      Last := Item'First + Width;
      Item (Item'First) := '.';
      X := Longest_Unsigned_Float'Rounding (
         Value * Longest_Unsigned_Float (Base) ** (Width - Exponent));
      for I in reverse Item'First + 1 .. Last loop
         declare
            Q : Long_Long_Float;
            R : Long_Long_Float;
         begin
            Long_Long_Float_Divide (X, Long_Long_Float (Base), Q, R);
            Image (
               Digit (R),
               Item (I),
               Casing => Casing);
            X := Q;
         end;
      end loop;
   end Aft_Image;

   function Fore_Width (Value : Longest_Float; Base : Number_Base := 10)
      return Positive
   is
      V : Long_Long_Float := Value;
      Result : Positive := 1;
   begin
      while V >= Long_Long_Float (Base) loop
         V := V / Long_Long_Float (Base);
         Result := Result + 1;
      end loop;
      return Result;
   end Fore_Width;

end System.Formatting.Float;
