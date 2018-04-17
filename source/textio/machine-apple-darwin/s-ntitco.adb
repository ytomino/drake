--  reference:
--  http://www.mudpedia.org/mediawiki/index.php/Xterm_256_colors
with System.Address_To_Named_Access_Conversions;
with System.Formatting;
with System.Long_Long_Integer_Types;
with System.Once;
with C.stdlib;
package body System.Native_Text_IO.Terminal_Colors is
   use type C.char_array;
   use type C.char_ptr;
   use type C.signed_int;
   use type C.size_t;

   subtype Word_Unsigned is Long_Long_Integer_Types.Word_Unsigned;

   function strlen (s : not null access constant C.char) return C.size_t
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_strlen";

   package char_ptr_Conv is
      new Address_To_Named_Access_Conversions (C.char, C.char_ptr);

   TERM_Variable : constant C.char_array (0 .. 4) := "TERM" & C.char'Val (0);

   xterm_256color : constant String (1 .. 14) := "xterm-256color";

   Support_256_Color_Flag : aliased Once.Flag := 0;
   Support_256_Color : Boolean;

   procedure Support_256_Color_Init;
   procedure Support_256_Color_Init is
      TERM : C.char_ptr;
   begin
      TERM := C.stdlib.getenv (TERM_Variable (0)'Access);
      if TERM /= null
         and then strlen (TERM) = xterm_256color'Length
      then
         declare
            TERM_All : String (1 .. xterm_256color'Length);
            for TERM_All'Address use char_ptr_Conv.To_Address (TERM);
         begin
            Support_256_Color := TERM_All = xterm_256color;
         end;
      else
         Support_256_Color := False;
      end if;
   end Support_256_Color_Init;

   procedure Initialize;
   procedure Initialize is
   begin
      Once.Initialize (
         Support_256_Color_Flag'Access,
         Support_256_Color_Init'Access);
   end Initialize;

   function RGB_To_256_Color (Item : Ada.Colors.RGB) return Color;
   function RGB_To_256_Color (Item : Ada.Colors.RGB) return Color is
      subtype B is Ada.Colors.Brightness'Base;
      function Color_Scale (Item : B) return Color;
      function Color_Scale (Item : B) return Color is
      begin
         if Item < (0.0 + 16#5F.0#) / 2.0 / 255.0 then
            return 0;
         elsif Item < (16#5F.0# + 16#87.0#) / 2.0 / 255.0 then
            return 1;
         elsif Item < (16#87.0# + 16#AF.0#) / 2.0 / 255.0 then
            return 2;
         elsif Item < (16#AF.0# + 16#D7.0#) / 2.0 / 255.0 then
            return 3;
         elsif Item < (16#D7.0# + 16#FF.0#) / 2.0 / 255.0 then
            return 4;
         else
            return 5;
         end if;
      end Color_Scale;
   begin
      return 16
         + 36 * Color_Scale (Item.Red)
         + 6 * Color_Scale (Item.Green)
         + Color_Scale (Item.Blue);
   end RGB_To_256_Color;

   function Brightness_To_Grayscale_256_Color (Item : Ada.Colors.Brightness)
      return Color;
   function Brightness_To_Grayscale_256_Color (Item : Ada.Colors.Brightness)
      return Color
   is
      subtype B is Ada.Colors.Brightness'Base;
      Grayscale_Index : constant Integer :=
         (Integer (B'Floor (Item * B'Pred (250.0))) + 5) / 10 - 1 + 232;
   begin
      if Grayscale_Index < 232 then
         return 16; -- 16#00#
      elsif Grayscale_Index <= 255 then -- in 232 .. 255
         return Color (Grayscale_Index);
      else
         return 16 + 6#555#; -- 16#FF#
      end if;
   end Brightness_To_Grayscale_256_Color;

   function RGB_To_System_Color (Item : Ada.Colors.RGB) return Color;
   function RGB_To_System_Color (Item : Ada.Colors.RGB) return Color is
      subtype B is Ada.Colors.Brightness'Base;
      Result : Color;
   begin
      if Item.Red in 0.25 .. B'Pred (0.675)
         and then Item.Green in 0.25 .. B'Pred (0.675)
         and then Item.Blue in 0.25 .. B'Pred (0.675)
      then -- Dark_Gray = (16#80#, 16#80#, 16#80#)
         Result := 8;
      elsif Item.Red >= 0.875
         or else Item.Green >= 0.875
         or else Item.Blue >= 0.875
      then -- bright colors
         Result := 8;
         if Item.Red >= 0.875 then
            Result := Result or 1;
         end if;
         if Item.Green >= 0.875 then
            Result := Result or 2;
         end if;
         if Item.Blue >= 0.875 then
            Result := Result or 4;
         end if;
      else -- dark colors
         Result := 0;
         if Item.Red >= 0.375 then
            Result := Result or 1;
         end if;
         if Item.Green >= 0.375 then
            Result := Result or 2;
         end if;
         if Item.Blue >= 0.375 then
            Result := Result or 4;
         end if;
      end if;
      return Result;
   end RGB_To_System_Color;

   function Brightness_To_Grayscale_System_Color (Item : Ada.Colors.Brightness)
      return Color;
   function Brightness_To_Grayscale_System_Color (Item : Ada.Colors.Brightness)
      return Color is
   begin
      --  [0.000 .. 0.250) => 0
      --  [0.250 .. 0.625) => 16#80# = 8
      --  [0.625 .. 0.875) => 16#C0# = 7
      --  [0.875 .. 1.000] => 16#FF# = 15
      return RGB_To_System_Color ((Red => Item, Green => Item, Blue => Item));
   end Brightness_To_Grayscale_System_Color;

   --  implementation

   function RGB_To_Color (Item : Ada.Colors.RGB) return Color is
   begin
      Initialize;
      if Support_256_Color then
         return RGB_To_256_Color (Item);
      else
         return RGB_To_System_Color (Item);
      end if;
   end RGB_To_Color;

   function Brightness_To_Grayscale_Color (Item : Ada.Colors.Brightness)
      return Color is
   begin
      Initialize;
      if Support_256_Color then
         return Brightness_To_Grayscale_256_Color (Item);
      else
         return Brightness_To_Grayscale_System_Color (Item);
      end if;
   end Brightness_To_Grayscale_Color;

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
      Seq : String (1 .. 256);
      Last : Natural;
      Error : Boolean;
   begin
      Seq (1) := Character'Val (16#1B#);
      Seq (2) := '[';
      Last := 2;
      --  changing
      if Reset then
         Last := Last + 1;
         Seq (Last) := '0';
      end if;
      if Bold_Changing and then Bold then
         if Last > 2 then
            Last := Last + 1;
            Seq (Last) := ';';
         end if;
         Last := Last + 1;
         Seq (Last) := '1';
      end if;
      if Underline_Changing and then Underline then
         if Last > 2 then
            Last := Last + 1;
            Seq (Last) := ';';
         end if;
         Last := Last + 1;
         Seq (Last) := '4';
      end if;
      if Blink_Changing and then Blink then
         if Last > 2 then
            Last := Last + 1;
            Seq (Last) := ';';
         end if;
         Last := Last + 1;
         Seq (Last) := '5';
      end if;
      if Reversed_Changing and then Reversed then
         if Last > 2 then
            Last := Last + 1;
            Seq (Last) := ';';
         end if;
         Last := Last + 1;
         Seq (Last) := '7';
      end if;
      if Foreground_Changing then
         if Last > 2 then
            Last := Last + 1;
            Seq (Last) := ';';
         end if;
         declare
            Color_Index : Word_Unsigned := Word_Unsigned (Foreground);
         begin
            if Foreground < 16#10# then
               --  system color
               if (Foreground and 8) = 0 then
                  Last := Last + 1;
                  Seq (Last) := '3';
               else
                  Last := Last + 1;
                  Seq (Last) := '9';
                  Color_Index := Word_Unsigned (Foreground and 7);
               end if;
            else
               --  256 color
               Seq (Last + 1 .. Last + 5) := "38;5;";
               Last := Last + 5;
            end if;
            Formatting.Image (
               Color_Index,
               Seq (Last + 1 .. Seq'Last),
               Last,
               Error => Error);
         end;
      end if;
      if Background_Changing then
         if Last > 2 then
            Last := Last + 1;
            Seq (Last) := ';';
         end if;
         declare
            Color_Index : Word_Unsigned := Word_Unsigned (Background);
         begin
            if Background < 16#10# then
               --  system color
               if (Background and 8) = 0 then
                  Last := Last + 1;
                  Seq (Last) := '4';
               else
                  Last := Last + 1;
                  Seq (Last) := '1';
                  Last := Last + 1;
                  Seq (Last) := '0';
                  Color_Index := Word_Unsigned (Background and 7);
               end if;
            else
               --  256 color
               Seq (Last + 1 .. Last + 5) := "48;5;";
               Last := Last + 5;
            end if;
            Formatting.Image (
               Color_Index,
               Seq (Last + 1 .. Seq'Last),
               Last,
               Error => Error);
         end;
      end if;
      --  setting
      if Last > 2 then
         Last := Last + 1;
         Seq (Last) := 'm';
         Write_Just (Handle, Seq (1 .. Last));
      end if;
   end Set;

   procedure Reset (
      Handle : Handle_Type)
   is
      Seq : constant String (1 .. 4) :=
         (Character'Val (16#1b#), '[', '0', 'm');
   begin
      Write_Just (Handle, Seq);
   end Reset;

end System.Native_Text_IO.Terminal_Colors;
