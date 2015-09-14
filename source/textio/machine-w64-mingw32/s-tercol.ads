pragma License (Unrestricted);
--  implementation unit specialized for Windows
with Ada.Colors;
with System.Native_IO;
package System.Terminal_Colors is
   pragma Preelaborate;

   type Color is mod 16;

   --  Note: Color represents
   --    a combination of BLUE(1), GREEN(2), RED(4) or INTENSITY(8).

   function RGB_To_Color (Item : Ada.Colors.RGB) return Color;

   procedure Set (
      Handle : Native_IO.Handle_Type;
      Reset : Boolean;
      Bold_Changing : Boolean;
      Bold : Boolean;
      Underline_Changing : Boolean;
      Underline : Boolean;
      Blink_Changing : Boolean;
      Blink : Boolean;
      Reversed_Changing : Boolean;
      Reversed : Boolean;
      Foreground_Changing : Boolean;
      Foreground : Color;
      Background_Changing : Boolean;
      Background : Color);
   procedure Reset (
      Handle : Native_IO.Handle_Type);

end System.Terminal_Colors;
