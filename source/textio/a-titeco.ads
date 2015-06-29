pragma License (Unrestricted);
--  extended unit
with Ada.Colors;
private with System.Terminal_Colors;
package Ada.Text_IO.Terminal.Colors is
   --  Additional terminal-color handling subprograms.

   type Color is private;

   function To_Color (Item : Ada.Colors.RGB) return Color;

   pragma Inline (To_Color); -- renamed

   type Boolean_Parameter (Changing : Boolean := False) is record
      case Changing is
         when False =>
            null;
         when True =>
            Item : Boolean;
      end case;
   end record;

   type Color_Parameter (Changing : Boolean := False) is record
      case Changing is
         when False =>
            null;
         when True =>
            Item : Color;
      end case;
   end record;

   --  for shorthand
   function "+" (Item : Boolean) return Boolean_Parameter;
   function "+" (Item : Color) return Color_Parameter;

   pragma Inline ("+");

   procedure Set_Color (
      File : File_Type; -- Output_File_Type
      Reset : Boolean := False;
      Bold : Boolean_Parameter := (Changing => False); -- only POSIX
      Underline : Boolean_Parameter := (Changing => False);
      Blink : Boolean_Parameter := (Changing => False); -- only POSIX
      Reversed : Boolean_Parameter := (Changing => False);
      Foreground : Color_Parameter := (Changing => False);
      Background : Color_Parameter := (Changing => False));
   procedure Reset_Color (
      File : File_Type); -- Output_File_Type

private

   type Color is new System.Terminal_Colors.Color;

   function To_Color (Item : Ada.Colors.RGB) return Color
      renames RGB_To_Color;

end Ada.Text_IO.Terminal.Colors;
