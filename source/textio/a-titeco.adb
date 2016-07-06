package body Ada.Text_IO.Terminal.Colors is

   function "+" (Item : Boolean) return Boolean_Parameter is
   begin
      return (Changing => True, Item => Item);
   end "+";

   function "+" (Item : Color) return Color_Parameter is
   begin
      return (Changing => True, Item => Item);
   end "+";

   procedure Set_Color (
      File : File_Type; -- Output_File_Type
      Reset : Boolean := False;
      Bold : Boolean_Parameter := (Changing => False);
      Underline : Boolean_Parameter := (Changing => False);
      Blink : Boolean_Parameter := (Changing => False);
      Reversed : Boolean_Parameter := (Changing => False);
      Foreground : Color_Parameter := (Changing => False);
      Background : Color_Parameter := (Changing => False))
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Open (File) or else raise Status_Error);
      pragma Check (Dynamic_Predicate,
         Check => Mode (File) /= In_File or else raise Mode_Error);
      Bold_Item : Boolean;
      Underline_Item : Boolean;
      Blink_Item : Boolean;
      Reversed_Item : Boolean;
      Foreground_Item : System.Native_Text_IO.Terminal_Colors.Color;
      Background_Item : System.Native_Text_IO.Terminal_Colors.Color;
   begin
      if Bold.Changing then
         Bold_Item := Bold.Item;
      end if;
      if Underline.Changing then
         Underline_Item := Underline.Item;
      end if;
      if Blink.Changing then
         Blink_Item := Blink.Item;
      end if;
      if Reversed.Changing then
         Reversed_Item := Reversed.Item;
      end if;
      if Foreground.Changing then
         Foreground_Item :=
            System.Native_Text_IO.Terminal_Colors.Color (Foreground.Item);
      end if;
      if Background.Changing then
         Background_Item :=
            System.Native_Text_IO.Terminal_Colors.Color (Background.Item);
      end if;
      declare
         NC_File : Naked_Text_IO.Non_Controlled_File_Type
            renames Controlled.Reference (File).all;
      begin
         System.Native_Text_IO.Terminal_Colors.Set (
            Naked_Text_IO.Terminal_Handle (NC_File),
            Reset,
            Bold.Changing,
            Bold_Item,
            Underline.Changing,
            Underline_Item,
            Blink.Changing,
            Blink_Item,
            Reversed.Changing,
            Reversed_Item,
            Foreground.Changing,
            Foreground_Item,
            Background.Changing,
            Background_Item);
      end;
   end Set_Color;

   procedure Reset_Color (
      File : File_Type)
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Open (File) or else raise Status_Error);
      pragma Check (Dynamic_Predicate,
         Check => Mode (File) /= In_File or else raise Mode_Error);
      NC_File : Naked_Text_IO.Non_Controlled_File_Type
         renames Controlled.Reference (File).all;
   begin
      System.Native_Text_IO.Terminal_Colors.Reset (
         Naked_Text_IO.Terminal_Handle (NC_File));
   end Reset_Color;

end Ada.Text_IO.Terminal.Colors;
