pragma License (Unrestricted);
--  extended unit
with Ada.Numerics;
package Ada.Colors is
   --  Color spaces.
   pragma Pure;

   subtype Brightness is Float range 0.0 .. 1.0;

   subtype Hue is Float range 0.0 .. Float'Pred (2.0 * Numerics.Pi);

   type RGB is record
      Red : Brightness;
      Green : Brightness;
      Blue : Brightness;
   end record;

   type HSV is record
      Hue : Colors.Hue;
      Saturation : Brightness;
      Value : Brightness;
   end record;

   type HSL is record
      Hue : Colors.Hue;
      Saturation : Brightness;
      Lightness : Brightness;
   end record;

   function To_RGB (Color : HSV) return RGB;
   function To_RGB (Color : HSL) return RGB;
   function To_HSV (Color : RGB) return HSV;
   function To_HSV (Color : HSL) return HSV;
   function To_HSL (Color : RGB) return HSL;
   function To_HSL (Color : HSV) return HSL;

   --  NTSC luminance
   function Luminance (Color : RGB) return Brightness;
   function Luminance (Color : HSV) return Brightness;
   function Luminance (Color : HSL) return Brightness;

   --  distances in the color spaces
   function RGB_Distance (Left, Right : RGB) return Float;
   function HSV_Distance (Left, Right : HSV) return Float;
   function HSL_Distance (Left, Right : HSL) return Float;

end Ada.Colors;
