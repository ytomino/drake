pragma License (Unrestricted);
--  extended unit specialized for POSIX (Darwin, FreeBSD, or Linux)
package Ada.Text_IO.Terminal.Colors.Names is
   --  Constants for platform-depended system colors.

   Black : constant Color; -- (R => 0.0, G => 0.0, B => 0.0)
   Dark_Red : constant Color; -- (R => 0.75, G => 0.0, B => 0.0)
   Dark_Green : constant Color; -- (R => 0.0, G => 0.75, B => 0.0)
   Dark_Yellow : constant Color; -- (R => 0.75, G => 0.75, B => 0.0)
   Dark_Blue : constant Color; -- (R => 0.0, G => 0.0, B => 0.75)
   Dark_Magenta : constant Color; -- (R => 0.75, G => 0.0, B => 0.75)
   Dark_Cyan : constant Color; -- (R => 0.0, G => 0.75, B => 0.75)
   Gray : constant Color; -- (R => 0.75, G => 0.75, B => 0.75)
   Dark_Gray : constant Color; -- (R => 0.5, G => 0.5, B => 0.5)
   Red : constant Color; -- (R => 1.0, G => 0.5, B => 0.5)
   Green : constant Color; -- (R => 0.5, G => 1.0, B => 0.5)
   Yellow : constant Color; -- (R => 1.0, G => 1.0, B => 0.5)
   Blue : constant Color; -- (R => 0.5, G => 0.5, B => 1.0)
   Magenta : constant Color; -- (R => 1.0, G => 0.5, B => 1.0)
   Cyan : constant Color; -- (R => 0.5, G => 1.0, B => 1.0)
   White : constant Color; -- (R => 1.0, G => 1.0, B => 1.0)

private

   Black : constant Color := 0;
   Dark_Red : constant Color := 1;
   Dark_Green : constant Color := 2;
   Dark_Yellow : constant Color := 3;
   Dark_Blue : constant Color := 4;
   Dark_Magenta : constant Color := 5;
   Dark_Cyan : constant Color := 6;
   Gray : constant Color := 7;
   Dark_Gray : constant Color := 8;
   Red : constant Color := 9;
   Green : constant Color := 10;
   Yellow : constant Color := 11;
   Blue : constant Color := 12;
   Magenta : constant Color := 13;
   Cyan : constant Color := 14;
   White : constant Color := 15;

end Ada.Text_IO.Terminal.Colors.Names;
