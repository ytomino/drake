pragma License (Unrestricted);
--  implementation unit specialized for POSIX (Darwin, FreeBSD, or Linux)
with Ada.Colors;
with System.Native_IO;
package System.Terminal_Colors is
   pragma Preelaborate;

   type Color is mod 256;
   --    0 ..   7 => Normal System Colors (R, G, B => 0 or 16#C0#)
   --  if $TERM = xterm-256color,
   --    8 ..  16 => Bright System Colors (R, G, B => 16#80# or 16#FF#)
   --   16 .. 231 => (B, G, R => 0, 16#5F#, 16#87#, 16#AF#, 16#D7# or 16#FF#)
   --  232 .. 255 => Grayscale (16#08# + 10 * (Index - 232))

   function RGB_To_Color (Item : Ada.Colors.RGB) return Color;

   procedure Set (
      Handle : Native_IO.Handle_Type;
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
