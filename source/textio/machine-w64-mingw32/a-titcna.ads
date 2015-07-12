pragma License (Unrestricted);
--  extended unit specialized for Windows
private with C.wincon;
package Ada.Text_IO.Terminal.Colors.Names is
   --  Constants for platform-depended system colors.

   Black : constant Color; -- (R => 0.0, G => 0.0, B => 0.0)
   Dark_Blue : constant Color; -- (R => 0.0, G => 0.0, B => 0.5)
   Dark_Green : constant Color; -- (R => 0.0, G => 0.5, B => 0.0)
   Dark_Cyan : constant Color; -- (R => 0.0, G => 0.5, B => 0.5)
   Dark_Red : constant Color; -- (R => 0.5, G => 0.0, B => 0.0)
   Dark_Magenta : constant Color; -- (R => 0.5, G => 0.0, B => 0.5)
   Dark_Yellow : constant Color; -- (R => 0.5, G => 0.5, B => 0.0)
   Gray : constant Color; -- (R => 0.75, G => 0.75, B => 0.75)
   Dark_Gray : constant Color; -- (R => 0.5, G => 0.5, B => 0.5)
   Blue : constant Color; -- (R => 0.0, G => 0.0, B => 1.0)
   Green : constant Color; -- (R => 0.0, G => 1.0, B => 0.0)
   Cyan : constant Color; -- (R => 0.0, G => 1.0, B => 1.0)
   Red : constant Color; -- (R => 1.0, G => 0.0, B => 0.0)
   Magenta : constant Color; -- (R => 1.0, G => 0.0, B => 1.0)
   Yellow : constant Color; -- (R => 1.0, G => 1.0, B => 0.0)
   White : constant Color; -- (R => 1.0, G => 1.0, B => 1.0)

private

   Black : constant Color := 0;
   Dark_Blue : constant Color := C.wincon.FOREGROUND_BLUE; -- 1
   Dark_Green : constant Color := C.wincon.FOREGROUND_GREEN; -- 2
   Dark_Cyan : constant Color := Dark_Blue or Dark_Green;
   Dark_Red : constant Color := C.wincon.FOREGROUND_RED; -- 4
   Dark_Magenta : constant Color := Dark_Blue or Dark_Red;
   Dark_Yellow : constant Color := Dark_Green or Dark_Red;
   Gray : constant Color := Dark_Blue or Dark_Green or Dark_Red;
   Dark_Gray : constant Color := C.wincon.FOREGROUND_INTENSITY; -- 8
   Blue : constant Color := Dark_Blue or C.wincon.FOREGROUND_INTENSITY;
   Green : constant Color := Dark_Green or C.wincon.FOREGROUND_INTENSITY;
   Cyan : constant Color := Dark_Cyan or C.wincon.FOREGROUND_INTENSITY;
   Red : constant Color := Dark_Red or C.wincon.FOREGROUND_INTENSITY;
   Magenta : constant Color := Dark_Magenta or C.wincon.FOREGROUND_INTENSITY;
   Yellow : constant Color := Dark_Yellow or C.wincon.FOREGROUND_INTENSITY;
   White : constant Color := Gray or C.wincon.FOREGROUND_INTENSITY;

end Ada.Text_IO.Terminal.Colors.Names;
