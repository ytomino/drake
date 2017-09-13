with System.Unwind.Raising;
pragma Warnings (Off, System.Unwind.Raising); -- break "pure" rule
package body System.Exponentiations is
   pragma Suppress (All_Checks);

   function Generic_Exp_Integer (Left : Integer_Type; Right : Natural)
      return Integer_Type is
   begin
      if Left = 2 then
         if Right >= Integer_Type'Size then
            Unwind.Raising.Overflow;
         else
            declare
               function Shift_Left (Value : Integer_Type; Amount : Natural)
                  return Integer_Type
                  with Import, Convention => Intrinsic;
            begin
               return Shift_Left (1, Right);
            end;
         end if;
      elsif Left = 0 then
         if Right > 0 then
            return 0;
         else -- Right = 0
            return 1;
         end if;
      else
         declare
            function mul_overflow (
               a, b : Integer;
               res : not null access Integer)
               return Boolean
               with Import,
                  Convention => Intrinsic,
                  External_Name => "__builtin_smul_overflow";
            function mul_overflow (
               a, b : Long_Long_Integer;
               res : not null access Long_Long_Integer)
               return Boolean
               with Import,
                  Convention => Intrinsic,
                  External_Name => "__builtin_smulll_overflow";
            Result : Integer_Type := 1;
            Factor : Integer_Type := Left;
            Exponent : Natural := Right;
         begin
            if Factor < 0 then
               Factor := -Factor;
               if Exponent rem 2 /= 0 then
                  Result := -1;
               end if;
            end if;
            loop
               if Exponent rem 2 /= 0 then
                  declare
                     Overflow : Boolean;
                  begin
                     if Integer_Type'Size > Integer'Size then
                        declare
                           R : aliased Long_Long_Integer;
                        begin
                           Overflow := mul_overflow (
                              Long_Long_Integer (Result),
                              Long_Long_Integer (Factor),
                              R'Access);
                           Result := Integer_Type (R);
                        end;
                     else
                        declare
                           R : aliased Integer;
                        begin
                           Overflow := mul_overflow (
                              Integer (Result),
                              Integer (Factor),
                              R'Access);
                           Result := Integer_Type (R);
                        end;
                     end if;
                     if Overflow then
                        Unwind.Raising.Overflow;
                     end if;
                  end;
               end if;
               Exponent := Exponent / 2;
               exit when Exponent = 0;
               if Exponent rem 2 /= 0 then
                  declare
                     Overflow : Boolean;
                  begin
                     if Integer_Type'Size > Integer'Size then
                        declare
                           Factor_Squared : aliased Long_Long_Integer;
                        begin
                           Overflow := mul_overflow (
                              Long_Long_Integer (Factor),
                              Long_Long_Integer (Factor),
                              Factor_Squared'Access);
                           Factor := Integer_Type (Factor_Squared);
                        end;
                     else
                        declare
                           Factor_Squared : aliased Integer;
                        begin
                           Overflow := mul_overflow (
                              Integer (Factor),
                              Integer (Factor),
                              Factor_Squared'Access);
                           Factor := Integer_Type (Factor_Squared);
                        end;
                     end if;
                     if Overflow then
                        Unwind.Raising.Overflow;
                     end if;
                  end;
               end if;
            end loop;
            return Result;
         end;
      end if;
   end Generic_Exp_Integer;

   function Generic_Exp_Integer_No_Check (Left : Integer_Type; Right : Natural)
      return Integer_Type is
   begin
      if Left = 2 then
         if Right >= Integer_Type'Size then
            return 0;
         else
            declare
               function Shift_Left (Value : Integer_Type; Amount : Natural)
                  return Integer_Type
                  with Import, Convention => Intrinsic;
            begin
               return Shift_Left (1, Right);
            end;
         end if;
      else
         declare
            Result : Integer_Type := 1;
            Factor : Integer_Type := Left;
            Exponent : Natural := Right;
         begin
            loop
               if Exponent rem 2 /= 0 then
                  Result := Result * Factor;
               end if;
               Exponent := Exponent / 2;
               exit when Exponent = 0;
               Factor := Factor * Factor;
            end loop;
            return Result;
         end;
      end if;
   end Generic_Exp_Integer_No_Check;

   function Generic_Exp_Unsigned (Left : Unsigned_Type; Right : Natural)
      return Unsigned_Type is
   begin
      if Left = 2 then
         if Right >= Unsigned_Type'Size then
            return 0;
         else
            return Shift_Left (1, Right);
         end if;
      else
         declare
            Result : Unsigned_Type := 1;
            Factor : Unsigned_Type := Left;
            Exponent : Natural := Right;
         begin
            loop
               if Exponent rem 2 /= 0 then
                  Result := Result * Factor;
               end if;
               Exponent := Exponent / 2;
               exit when Exponent = 0;
               Factor := Factor * Factor;
            end loop;
            return Result;
         end;
      end if;
   end Generic_Exp_Unsigned;

   function Generic_Exp_Modular (
      Left : Unsigned_Type;
      Modulus : Unsigned_Type;
      Right : Natural)
      return Unsigned_Type
   is
      pragma Suppress (Division_Check); -- Modulus > 0
   begin
      if Left = 2 and then Right < Unsigned_Type'Size then
         return Shift_Left (1, Right) mod Modulus;
      else
         declare
            type Long_Long_Unsigned is mod 2 ** Long_Long_Integer'Size;
            Result : Unsigned_Type := 1;
            Factor : Unsigned_Type := Left;
            Exponent : Natural := Right;
         begin
            loop
               if Exponent rem 2 /= 0 then
                  Result := Unsigned_Type'Mod (
                     Long_Long_Unsigned'Mod (Result)
                        * Long_Long_Unsigned'Mod (Factor)
                        mod Long_Long_Unsigned'Mod (Modulus));
               end if;
               Exponent := Exponent / 2;
               exit when Exponent = 0;
               Factor := Unsigned_Type'Mod (
                  Long_Long_Unsigned'Mod (Factor)
                     * Long_Long_Unsigned'Mod (Factor)
                     mod Long_Long_Unsigned'Mod (Modulus));
            end loop;
            return Result;
         end;
      end if;
   end Generic_Exp_Modular;

end System.Exponentiations;
