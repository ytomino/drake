package body Ada.Numerics.Generic_Elementary_Functions is
   pragma Suppress (All_Checks);

   --  implementation

   function Arccos (X, Cycle : Float_Type'Base) return Float_Type'Base is
   begin
      if Standard'Fast_Math then
         return Arccos (X) * Cycle / (2.0 * Pi);
      else
         if Cycle <= 0.0 then
            raise Argument_Error; -- CXA5A06
         elsif X = -1.0 then
            return Cycle / 2.0; -- CXG2015
         else
            return Arccos (X) * Cycle / (2.0 * Pi);
         end if;
      end if;
   end Arccos;

   function Arccot (X : Float_Type'Base; Y : Float_Type'Base := 1.0)
      return Float_Type'Base is
   begin
      return Arctan (Y, X);
   end Arccot;

   function Arccot (
      X : Float_Type'Base;
      Y : Float_Type'Base := 1.0;
      Cycle : Float_Type'Base)
      return Float_Type'Base is
   begin
      if not Standard'Fast_Math and then Cycle <= 0.0 then
         raise Argument_Error; -- CXA5A08
      else
         return Arccot (X, Y) * Cycle / (2.0 * Pi);
      end if;
   end Arccot;

   function Arccoth (X : Float_Type'Base) return Float_Type'Base is
   begin
      if not Standard'Fast_Math and then abs X < 1.0 then
         raise Argument_Error; -- CXA5A04
      else
         return Log ((X + 1.0) / (X - 1.0)) * 0.5;
      end if;
   end Arccoth;

   function Arcsin (X, Cycle : Float_Type'Base) return Float_Type'Base is
   begin
      if Standard'Fast_Math then
         return Arcsin (X) * Cycle / (2.0 * Pi);
      else
         if Cycle <= 0.0 then
            raise Argument_Error; -- CXA5A05
         elsif abs X = 1.0 then
            return Float_Type'Base'Copy_Sign (Cycle / 4.0, X); -- CXG2015
         else
            return Arcsin (X) * Cycle / (2.0 * Pi);
         end if;
      end if;
   end Arcsin;

   function Arctan (
      Y : Float_Type'Base;
      X : Float_Type'Base := 1.0;
      Cycle : Float_Type'Base)
      return Float_Type'Base is
   begin
      if not Standard'Fast_Math and then Cycle <= 0.0 then
         raise Argument_Error; -- CXA5A07
      elsif not Standard'Fast_Math and then Y = 0.0 then
         --  CXG2016 requires
         if X < 0.0 then
            return Cycle / 2.0 * Float_Type'Copy_Sign (1.0, Y);
         else
            return 0.0;
         end if;
      else
         return Arctan (Y, X) * Cycle / (2.0 * Pi);
      end if;
   end Arctan;

   function Cos (X, Cycle : Float_Type'Base) return Float_Type'Base is
   begin
      if Standard'Fast_Math then
         return Cos (2.0 * Pi * X / Cycle);
      else
         --  CXA5A02 requires just result that is 0.0, 1.0 or -1.0
         --  CXG2004 requires just result that is 0.5
         if Cycle <= 0.0 then
            raise Argument_Error;
         else
            declare
               R : constant Float_Type'Base :=
                  Float_Type'Base'Remainder (X / Cycle, 1.0);
            begin
               if R = 2.0 / 12.0 then
                  return 0.5;
               elsif R = 0.25 then
                  return 0.0;
               elsif R = 4.0 / 12.0 then
                  return -0.5;
               elsif R = 0.5 then
                  return -1.0;
               elsif R = 8.0 / 12.0 then
                  return -0.5;
               elsif R = 0.75 then
                  return 0.0;
               elsif R = 10.0 / 12.0 then
                  return 0.5;
               else
                  return Cos (2.0 * Pi * R);
               end if;
            end;
         end if;
      end if;
   end Cos;

   function Cot (X : Float_Type'Base) return Float_Type'Base is
   begin
      return 1.0 / Tan (X);
   end Cot;

   function Cot (X, Cycle : Float_Type'Base) return Float_Type'Base is
   begin
      if Standard'Fast_Math then
         return Cot (2.0 * Pi * X / Cycle);
      else
         if Cycle <= 0.0 then
            raise Argument_Error; -- CXA5A04
         else
            --  CXG2013 requires just result that is 0.0
            declare
               R : constant Float_Type'Base :=
                  Float_Type'Base'Remainder (X / Cycle, 1.0);
            begin
               if R = 0.25 then
                  return 0.0;
               elsif R = 0.75 then
                  return 0.0;
               else
                  return Cot (2.0 * Pi * R);
               end if;
            end;
         end if;
      end if;
   end Cot;

   function Coth (X : Float_Type'Base) return Float_Type'Base is
   begin
      return 1.0 / Tanh (X);
   end Coth;

   function Log (X, Base : Float_Type'Base) return Float_Type'Base is
   begin
      if not Standard'Fast_Math and then (Base <= 0.0 or else Base = 1.0) then
         raise Argument_Error; -- CXA5A09
      else
         return Log (X) / Log (Base);
      end if;
   end Log;

   function Sin (X, Cycle : Float_Type'Base) return Float_Type'Base is
   begin
      if Standard'Fast_Math then
         return Sin (2.0 * Pi * X / Cycle);
      else
         --  CXA5A01 requires just result that is 0.0, 1.0 or -1.0
         --  CXG2004 requires just result that is 0.5
         if Cycle <= 0.0 then
            raise Argument_Error;
         else
            declare
               R : constant Float_Type'Base :=
                  Float_Type'Base'Remainder (X / Cycle, 1.0);
            begin
               if R = 1.0 / 12.0 then
                  return 0.5;
               elsif R = 0.25 then
                  return 1.0;
               elsif R = 5.0 / 12.0 then
                  return 0.5;
               elsif R = 0.5 then
                  return 0.0;
               elsif R = 7.0 / 12.0 then
                  return -0.5;
               elsif R = 0.75 then
                  return -1.0;
               elsif R = 11.0 / 12.0 then
                  return -0.5;
               else
                  return Sin (2.0 * Pi * R);
               end if;
            end;
         end if;
      end if;
   end Sin;

   function Tan (X, Cycle : Float_Type'Base) return Float_Type'Base is
   begin
      if Standard'Fast_Math then
         return Tan (2.0 * Pi * X / Cycle);
      else
         if Cycle <= 0.0 then
            raise Argument_Error; -- CXA5A01
         else
            --  CXG2013 requires just result that is 0.0
            declare
               R : constant Float_Type'Base :=
                  Float_Type'Base'Remainder (X / Cycle, 1.0);
            begin
               if R = 0.5 then
                  return 0.0;
               else
                  return Tan (2.0 * Pi * R);
               end if;
            end;
         end if;
      end if;
   end Tan;

end Ada.Numerics.Generic_Elementary_Functions;
