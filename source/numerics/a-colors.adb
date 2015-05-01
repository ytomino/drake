pragma Check_Policy (Trace, Off);
pragma Check_Policy (Validate, Off);
with Ada.Float;
package body Ada.Colors is
   pragma Suppress (All_Checks);

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
         if Color.Blue = Max then
            Hue := 4.0 * Diff + (Color.Red - Color.Green);
         elsif Color.Green = Max then
            Hue := 2.0 * Diff + (Color.Blue - Color.Red);
         else -- Red
            Hue := (Color.Green - Color.Blue);
            if Hue < 0.0 then
               Hue := Hue + 6.0 * Diff;
            end if;
         end if;
         Hue := 2.0 * Numerics.Pi / 6.0 * Hue / Diff;
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

   function To_RGB (Color : HSV) return RGB is
      pragma Check (Trace, Debug.Put (
         "H =" & Hue'Image (Color.Hue)
         & ", S =" & Brightness'Image (Color.Saturation)
         & ", V =" & Brightness'Image (Color.Value)));
      procedure Divide_By_1 is
         new Float.Divide_By_1 (
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

   function RGB_Distance (Left, Right : RGB) return Standard.Float is
   begin
      return (Left.Red - Right.Red) ** 2
         + (Left.Green - Right.Green) ** 2
         + (Left.Blue - Right.Blue) ** 2;
   end RGB_Distance;

   function HSV_Distance (Left, Right : HSV) return Standard.Float is
   begin
      return ((Left.Hue - Right.Hue) / (2.0 * Numerics.Pi)) ** 2
         + (Left.Saturation - Right.Saturation) ** 2
         + (Left.Value - Right.Value) ** 2;
   end HSV_Distance;

end Ada.Colors;
