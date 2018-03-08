with C.wincon;
with C.windef;
package body System.Native_Text_IO.Terminal_Colors is
   use type C.windef.WORD;

   Initial_Attributes_Assigned : Boolean := False;
   Initial_Attributes : C.windef.WORD;

   --  implementation

   function RGB_To_Color (Item : Ada.Colors.RGB) return Color is
      subtype B is Ada.Colors.Brightness'Base;
      Result : Color;
   begin
      if Item.Red in 0.625 .. B'Pred (0.875)
         and then Item.Green in 0.625 .. B'Pred (0.875)
         and then Item.Blue in 0.625 .. B'Pred (0.875)
      then -- Gray = (16#C0#, 16#C0#, 16#C0#)
         Result := C.wincon.FOREGROUND_RED
            or C.wincon.FOREGROUND_GREEN
            or C.wincon.FOREGROUND_BLUE;
      elsif Item.Red >= 0.75
         or else Item.Green >= 0.75
         or else Item.Blue >= 0.75
      then -- bright colors
         Result := C.wincon.FOREGROUND_INTENSITY;
         if Item.Red >= 0.75 then
            Result := Result or C.wincon.FOREGROUND_RED;
         end if;
         if Item.Green >= 0.75 then
            Result := Result or C.wincon.FOREGROUND_GREEN;
         end if;
         if Item.Blue >= 0.75 then
            Result := Result or C.wincon.FOREGROUND_BLUE;
         end if;
      else -- dark colors
         Result := 0;
         if Item.Red >= 0.25 then
            Result := Result or C.wincon.FOREGROUND_RED;
         end if;
         if Item.Green >= 0.25 then
            Result := Result or C.wincon.FOREGROUND_GREEN;
         end if;
         if Item.Blue >= 0.25 then
            Result := Result or C.wincon.FOREGROUND_BLUE;
         end if;
         if Result = (
            C.wincon.FOREGROUND_RED
            or C.wincon.FOREGROUND_GREEN
            or C.wincon.FOREGROUND_BLUE)
         then -- Dark_Gray = (16#80#, 16#80#, 16#80#)
            Result := C.wincon.FOREGROUND_INTENSITY;
         end if;
      end if;
      return Result;
   end RGB_To_Color;

   procedure Set (
      Handle : Handle_Type;
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
      Background : Color)
   is
      pragma Unreferenced (Bold_Changing);
      pragma Unreferenced (Bold);
      pragma Unreferenced (Underline_Changing);
      pragma Unreferenced (Underline);
      pragma Unreferenced (Blink_Changing);
      pragma Unreferenced (Blink);
      pragma Unreferenced (Reversed_Changing);
      pragma Unreferenced (Reversed);
      State : Output_State;
      Attributes : C.windef.WORD;
   begin
      --  getting
      Save_State (Handle, State);
      if not Initial_Attributes_Assigned then
         Initial_Attributes_Assigned := True;
         Initial_Attributes := State.Attributes;
      end if;
      if Reset then
         Attributes := Initial_Attributes;
      else
         Attributes := State.Attributes;
      end if;
      --  changing
--    if Underline_Changing then
--       Attributes := (Attributes and not C.wincon.COMMON_LVB_UNDERSCORE)
--          or Boolean'Pos (Underline) * C.wincon.COMMON_LVB_UNDERSCORE;
--    end if;
--    if Reversed_Changing then
--       Attributes := (Attributes and not C.wincon.COMMON_LVB_REVERSE_VIDEO)
--          or Boolean'Pos (Reversed) * C.wincon.COMMON_LVB_REVERSE_VIDEO;
--    end if;
      if Foreground_Changing then
         Attributes :=
            (Attributes and not 16#0F#) or C.windef.WORD (Foreground);
      end if;
      if Background_Changing then
         Attributes :=
            (Attributes and not 16#F0#)
            or (C.windef.WORD (Background) * 16#10#);
      end if;
      --  setting
      if Attributes /= State.Attributes then
         Set_Terminal_Attributes (Handle, Attributes);
      end if;
   end Set;

   procedure Reset (
      Handle : Handle_Type) is
   begin
      if Initial_Attributes_Assigned then
         Set_Terminal_Attributes (Handle, Initial_Attributes);
      end if;
   end Reset;

end System.Native_Text_IO.Terminal_Colors;
