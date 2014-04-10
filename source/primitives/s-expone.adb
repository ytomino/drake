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
            return Shift_Left (1, Right);
         end if;
      else
         declare
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
                  if Result < 0 then
                     if Integer_Type'First / Factor > Result then
                        Unwind.Raising.Overflow;
                     end if;
                  else
                     if Integer_Type'Last / Factor < Result then
                        Unwind.Raising.Overflow;
                     end if;
                  end if;
                  Result := Result * Factor;
               end if;
               Exponent := Exponent / 2;
               exit when Exponent = 0;
               if Integer_Type'Last / Factor < Factor then
                  Unwind.Raising.Overflow;
               end if;
               Factor := Factor * Factor;
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
            return Shift_Left (1, Right);
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
      Left : Integer_Type;
      Modulus : Unsigned_Type;
      Right : Natural)
      return Integer_Type
   is
      pragma Compile_Time_Error (Integer_Type'Size /= Unsigned_Type'Size,
         "size mismatch");
      pragma Suppress (Range_Check);
   begin
      if Left = 2 and then Right < Unsigned_Type'Size then
         return Integer_Type (Shift_Left (1, Right) mod Modulus);
      else
         declare
            type Long_Long_Unsigned is mod 2 ** Long_Long_Integer'Size;
            Result : Unsigned_Type := 1;
            Factor : Unsigned_Type := Unsigned_Type'Mod (Left);
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
            return Integer_Type (Result);
         end;
      end if;
   end Generic_Exp_Modular;

end System.Exponentiations;
