pragma Check_Policy (Trace => Ignore, Validate => Ignore);
with Ada.Float;
package body Ada.Colors is
   pragma Suppress (All_Checks);

   subtype Float is Standard.Float; -- hiding "Float" package

   function sinf (X : Float) return Float
      with Import, Convention => Intrinsic, External_Name => "__builtin_sinf";
   function cosf (X : Float) return Float
      with Import, Convention => Intrinsic, External_Name => "__builtin_cosf";

   function To_Hue (Color : RGB; Diff, Max : Brightness'Base)
      return Hue'Base;
   function To_Hue (Color : RGB; Diff, Max : Brightness'Base)
      return Hue'Base
   is
      Result : Hue'Base;
   begin
      pragma Check (Validate, Diff > 0.0);
      if Color.Blue = Max then
         Result := 4.0 * Diff + (Color.Red - Color.Green);
      elsif Color.Green = Max then
         Result := 2.0 * Diff + (Color.Blue - Color.Red);
      else -- Red
         Result := (Color.Green - Color.Blue);
         if Result < 0.0 then
            Result := Result + 6.0 * Diff;
         end if;
      end if;
      Result := 2.0 * Numerics.Pi / 6.0 * Result / Diff;
      return Result;
   end To_Hue;

   --  implementation

   function To_RGB (Color : HSV) return RGB is
      pragma Check (Trace, Debug.Put (
         "H =" & Hue'Image (Color.Hue)
         & ", S =" & Brightness'Image (Color.Saturation)
         & ", V =" & Brightness'Image (Color.Value)));
      procedure Divide_By_1 is
         new Ada.Float.Divide_By_1 (
            Brightness'Base,
            Hue'Base,
            Brightness'Base);
      Q : Hue'Base;
      Diff : Brightness'Base;
      N1, N2, N3 : Brightness'Base;
      Red, Green, Blue : Brightness'Base;
   begin
      Divide_By_1 (
         Dividend => 6.0 / (2.0 * Numerics.Pi) * Color.Hue,
         Quotient => Q,
         Remainder => Diff);
      N1 := Color.Value * (1.0 - Color.Saturation);
      N2 := Color.Value * (1.0 - Color.Saturation * Diff);
      N3 := Color.Value * (1.0 - Color.Saturation * (1.0 - Diff));
      if Q < 1.0 then
         Red := Color.Value;
         Green := N3;
         Blue := N1;
      elsif Q < 2.0 then
         Red := N2;
         Green := Color.Value;
         Blue := N1;
      elsif Q < 3.0 then
         Red := N1;
         Green := Color.Value;
         Blue := N3;
      elsif Q < 4.0 then
         Red := N1;
         Green := N2;
         Blue := Color.Value;
      elsif Q < 5.0 then
         Red := N3;
         Green := N1;
         Blue := Color.Value;
      else
         Red := Color.Value;
         Green := N1;
         Blue := N2;
      end if;
      pragma Check (Validate, Color.Saturation > 0.0
         or else (Red = Green and then Green = Blue));
      pragma Check (Trace, Debug.Put (
         "R =" & Brightness'Image (Red)
         & ", G =" & Brightness'Image (Green)
         & ", B =" & Brightness'Image (Blue)));
      return (Red => Red, Green => Green, Blue => Blue);
   end To_RGB;

   function To_RGB (Color : HSL) return RGB is
      pragma Check (Trace, Debug.Put (
         "H =" & Hue'Image (Color.Hue)
         & ", S =" & Brightness'Image (Color.Saturation)
         & ", L =" & Brightness'Image (Color.Lightness)));
      procedure Divide_By_1 is
         new Ada.Float.Divide_By_1 (
            Brightness'Base,
            Hue'Base,
            Brightness'Base);
      H, Q : Hue'Base;
      Diff : Brightness'Base;
      X, C, Max, Min : Brightness'Base;
      Red, Green, Blue : Brightness'Base;
   begin
      H := 3.0 / (2.0 * Numerics.Pi) * Color.Hue;
      Divide_By_1 (Dividend => H, Quotient => Q, Remainder => Diff);
      C := (1.0 - abs (2.0 * Color.Lightness - 1.0)) * Color.Saturation;
      Min := Color.Lightness - C / 2.0;
      Max := C + Min;
      X := C * (1.0 - abs (2.0 * Diff - 1.0)) + Min;
      if H < 0.5 then
         Red := Max;
         Green := X;
         Blue := Min;
      elsif H < 1.0 then
         Red := X;
         Green := Max;
         Blue := Min;
      elsif H < 1.5 then
         Red := Min;
         Green := Max;
         Blue := X;
      elsif H < 2.0 then
         Red := Min;
         Green := X;
         Blue := Max;
      elsif H < 2.5 then
         Red := X;
         Green := Min;
         Blue := Max;
      else
         Red := Max;
         Green := Min;
         Blue := X;
      end if;
      pragma Check (Trace, Debug.Put (
         "R =" & Brightness'Image (Red)
         & ", G =" & Brightness'Image (Green)
         & ", B =" & Brightness'Image (Blue)));
      return (Red => Red, Green => Green, Blue => Blue);
   end To_RGB;

   function To_HSV (Color : RGB) return HSV is
      pragma Check (Trace, Debug.Put (
         "R =" & Brightness'Image (Color.Red)
         & ", G =" & Brightness'Image (Color.Green)
         & ", B =" & Brightness'Image (Color.Blue)));
      Max : constant Brightness'Base :=
         Brightness'Base'Max (
            Color.Red,
            Brightness'Base'Max (
               Color.Green,
               Color.Blue));
      Min : constant Brightness'Base :=
         Brightness'Base'Min (
            Color.Red,
            Brightness'Base'Min (
               Color.Green,
               Color.Blue));
      Diff : constant Brightness'Base := Max - Min;
      Hue : Colors.Hue'Base;
      Saturation : Brightness'Base;
      Value : Brightness'Base;
   begin
      Value := Max;
      if Diff > 0.0 then
         Saturation := Diff / Max;
         Hue := To_Hue (Color, Diff => Diff, Max => Max);
      else
         Saturation := 0.0;
         Hue := 0.0;
      end if;
      pragma Check (Trace, Debug.Put (
         "H =" & Colors.Hue'Image (Hue)
         & ", S =" & Brightness'Image (Saturation)
         & ", V =" & Brightness'Image (Value)));
      return (Hue => Hue, Saturation => Saturation, Value => Value);
   end To_HSV;

   function To_HSV (Color : HSL) return HSV is
      pragma Check (Trace, Debug.Put (
         "H =" & Hue'Image (Color.Hue)
         & ", S =" & Brightness'Image (Color.Saturation)
         & ", L =" & Brightness'Image (Color.Lightness)));
      Hue : constant Colors.Hue'Base := Color.Hue;
      Saturation : Brightness'Base;
      Value : constant Brightness'Base :=
         Color.Lightness
         + Color.Saturation * (1.0 - abs (2.0 * Color.Lightness - 1.0)) / 2.0;
   begin
      if Value > 0.0 then
         Saturation := 2.0 * (Value - Color.Lightness) / Value;
      else
         Saturation := 0.0;
      end if;
      pragma Check (Trace, Debug.Put (
         "H =" & Colors.Hue'Image (Hue)
         & ", S =" & Brightness'Image (Saturation)
         & ", V =" & Brightness'Image (Value)));
      return (Hue => Hue, Saturation => Saturation, Value => Value);
   end To_HSV;

   function To_HSL (Color : RGB) return HSL is
      pragma Check (Trace, Debug.Put (
         "R =" & Brightness'Image (Color.Red)
         & ", G =" & Brightness'Image (Color.Green)
         & ", B =" & Brightness'Image (Color.Blue)));
      Max : constant Brightness'Base :=
         Brightness'Base'Max (
            Color.Red,
            Brightness'Base'Max (
               Color.Green,
               Color.Blue));
      Min : constant Brightness'Base :=
         Brightness'Base'Min (
            Color.Red,
            Brightness'Base'Min (
               Color.Green,
               Color.Blue));
      Diff : constant Brightness'Base := Max - Min;
      Hue : Colors.Hue'Base;
      Saturation : Brightness'Base;
      Lightness : Brightness'Base;
   begin
      Lightness := (Max + Min) / 2.0;
      if Diff > 0.0 then
         Saturation := Diff / (1.0 - abs (2.0 * Lightness - 1.0));
         Hue := To_Hue (Color, Diff => Diff, Max => Max);
      else
         Saturation := 0.0;
         Hue := 0.0;
      end if;
      pragma Check (Trace, Debug.Put (
         "H =" & Colors.Hue'Image (Hue)
         & ", S =" & Brightness'Image (Saturation)
         & ", L =" & Brightness'Image (Lightness)));
      return (Hue => Hue, Saturation => Saturation, Lightness => Lightness);
   end To_HSL;

   function To_HSL (Color : HSV) return HSL is
      pragma Check (Trace, Debug.Put (
         "H =" & Hue'Image (Color.Hue)
         & ", S =" & Brightness'Image (Color.Saturation)
         & ", V =" & Brightness'Image (Color.Value)));
      Hue : constant Colors.Hue'Base := Color.Hue;
      Saturation : Brightness'Base;
      Lightness : constant Brightness'Base :=
         Color.Value * (2.0 - Color.Saturation) / 2.0;
   begin
      if Lightness > 0.0 and then Lightness < 1.0 then
         Saturation := Color.Value * Color.Saturation
            / (1.0 - abs (2.0 * Lightness - 1.0));
      else
         Saturation := 0.0;
      end if;
      pragma Check (Trace, Debug.Put (
         "H =" & Colors.Hue'Image (Hue)
         & ", S =" & Brightness'Image (Saturation)
         & ", L =" & Brightness'Image (Lightness)));
      return (Hue => Hue, Saturation => Saturation, Lightness => Lightness);
   end To_HSL;

   function Luminance (Color : RGB) return Brightness is
   begin
      return Brightness'Base'Min (
         0.30 * Color.Red + 0.59 * Color.Green + 0.11 * Color.Blue,
         Brightness'Last);
   end Luminance;

   function Luminance (Color : HSV) return Brightness is
   begin
      return Luminance (To_RGB (Color));
   end Luminance;

   function Luminance (Color : HSL) return Brightness is
   begin
      return Luminance (To_RGB (Color));
   end Luminance;

   function RGB_Distance (Left, Right : RGB) return Float is
   begin
      --  sum of squares
      return (Left.Red - Right.Red) ** 2
         + (Left.Green - Right.Green) ** 2
         + (Left.Blue - Right.Blue) ** 2;
   end RGB_Distance;

   function HSV_Distance (Left, Right : HSV) return Float is
      pragma Check (Trace,
         Check => Debug.Put (
            "Left = (H =" & Hue'Image (Left.Hue)
            & ", S =" & Brightness'Image (Left.Saturation)
            & ", V =" & Brightness'Image (Left.Value)
            & ")"));
      pragma Check (Trace,
         Check => Debug.Put (
            "Right = (H =" & Hue'Image (Right.Hue)
            & ", S =" & Brightness'Image (Right.Saturation)
            & ", V =" & Brightness'Image (Right.Value)
            & ")"));
      --  cone model
      LR : constant Float := Left.Saturation * Left.Value / 2.0;
      LX : constant Float := cosf (Left.Hue) * LR;
      LY : constant Float := sinf (Left.Hue) * LR;
      LZ : constant Float := Left.Value;
      RR : constant Float := Right.Saturation * Right.Value / 2.0;
      RX : constant Float := cosf (Right.Hue) * RR;
      RY : constant Float := sinf (Right.Hue) * RR;
      RZ : constant Float := Right.Value;
      pragma Check (Trace,
         Check => Debug.Put (
            "Left = (X =" & Float'Image (LX)
            & ", Y =" & Float'Image (LY)
            & ", Z =" & Float'Image (LZ)
            & ")"));
      pragma Check (Trace,
         Check => Debug.Put (
            "Right = (X =" & Float'Image (RX)
            & ", Y =" & Float'Image (RY)
            & ", Z =" & Float'Image (RZ)
            & ")"));
   begin
      return (LX - RX) ** 2 + (LY - RY) ** 2 + (LZ - RZ) ** 2;
   end HSV_Distance;

   function HSL_Distance (Left, Right : HSL) return Float is
      pragma Check (Trace,
         Check => Debug.Put (
            "Left = (H =" & Hue'Image (Left.Hue)
            & ", S =" & Brightness'Image (Left.Saturation)
            & ", L =" & Brightness'Image (Left.Lightness)
            & ")"));
      pragma Check (Trace,
         Check => Debug.Put (
            "Right = (H =" & Hue'Image (Right.Hue)
            & ", S =" & Brightness'Image (Right.Saturation)
            & ", L =" & Brightness'Image (Right.Lightness)
            & ")"));
      --  double cone model
      LR : constant Float :=
         Left.Saturation * (0.5 - abs (Left.Lightness - 0.5));
      LX : constant Float := cosf (Left.Hue) * LR;
      LY : constant Float := sinf (Left.Hue) * LR;
      LZ : constant Float := Left.Lightness;
      RR : constant Float :=
         Right.Saturation * (0.5 - abs (Right.Lightness - 0.5));
      RX : constant Float := cosf (Right.Hue) * RR;
      RY : constant Float := sinf (Right.Hue) * RR;
      RZ : constant Float := Right.Lightness;
      pragma Check (Trace,
         Check => Debug.Put (
            "Left = (X =" & Float'Image (LX)
            & ", Y =" & Float'Image (LY)
            & ", Z =" & Float'Image (LZ)
            & ")"));
      pragma Check (Trace,
         Check => Debug.Put (
            "Right = (X =" & Float'Image (RX)
            & ", Y =" & Float'Image (RY)
            & ", Z =" & Float'Image (RZ)
            & ")"));
   begin
      return (LX - RX) ** 2 + (LY - RY) ** 2 + (LZ - RZ) ** 2;
   end HSL_Distance;

end Ada.Colors;
