with Ada.Exception_Identification.From_Here;
package body Ada.Wide_Text_IO.Editing is
   use Exception_Identification.From_Here;

   procedure Tail (Target : out Wide_String; Source : Wide_String);
   procedure Tail (Target : out Wide_String; Source : Wide_String) is
      Padding : constant Wide_Character := ' ';
   begin
      if Target'Length < Source'Length then
         Raise_Exception (Layout_Error'Identity);
      end if;
      for I in Target'First .. Target'Last - Source'Length loop
         Target (I) := Padding;
      end loop;
      Target (Target'Last - Source'Length + 1 .. Target'Last) := Source;
   end Tail;

   --  implementation

   package body Decimal_Output is

      package Instance is new Text_IO.Editing.Decimal_Output (Num);

      function Length (
         Pic : Picture;
         Currency : Wide_String := Default_Currency)
         return Natural is
      begin
         return Instance.Length (
            Text_IO.Editing.Picture (Pic),
            String'(1 .. Currency'Length => '$'));
      end Length;

      function Valid (
         Item : Num;
         Pic : Picture;
         Currency : Wide_String := Default_Currency)
         return Boolean is
      begin
         return Instance.Valid (
            Item,
            Text_IO.Editing.Picture (Pic),
            String'(1 .. Currency'Length => '$'));
      end Valid;

      function Image (
         Item : Num;
         Pic : Picture;
         Currency : Wide_String := Default_Currency;
         Fill : Wide_Character := Default_Fill;
         Separator : Wide_Character := Default_Separator;
         Radix_Mark : Wide_Character := Default_Radix_Mark)
         return Wide_String
      is
         Image : constant String := Instance.Image (
            Item,
            Text_IO.Editing.Picture (Pic),
            String'(1 .. Currency'Length => '$'));
         Result : Wide_String (Image'Range);
         I : Positive := Result'First;
      begin
         while I <= Result'Last loop
            case Image (I) is
               when '$' =>
                  Result (I .. I + Currency'Length - 1) := Currency;
                  I := I + Currency'Length;
               when '*' =>
                  Result (I) := Fill;
                  I := I + 1;
               when ',' =>
                  Result (I) := Separator;
                  I := I + 1;
               when '.' =>
                  Result (I) := Radix_Mark;
                  I := I + 1;
               when others =>
                  Result (I) := Wide_Character'Val (Character'Pos (Image (I)));
                  I := I + 1;
            end case;
         end loop;
         return Result;
      end Image;

      procedure Put (
         File : File_Type;
         Item : Num;
         Pic : Picture;
         Currency : Wide_String := Default_Currency;
         Fill : Wide_Character := Default_Fill;
         Separator : Wide_Character := Default_Separator;
         Radix_Mark : Wide_Character := Default_Radix_Mark) is
      begin
         Put (File, Image (Item, Pic, Currency, Fill, Separator, Radix_Mark));
      end Put;

      procedure Put (
         Item : Num;
         Pic : Picture;
         Currency : Wide_String := Default_Currency;
         Fill : Wide_Character := Default_Fill;
         Separator : Wide_Character := Default_Separator;
         Radix_Mark : Wide_Character := Default_Radix_Mark) is
      begin
         Put (
            Current_Output.all,
            Image (Item, Pic, Currency, Fill, Separator, Radix_Mark));
      end Put;

      procedure Put (
         To : out Wide_String;
         Item : Num;
         Pic : Picture;
         Currency : Wide_String := Default_Currency;
         Fill : Wide_Character := Default_Fill;
         Separator : Wide_Character := Default_Separator;
         Radix_Mark : Wide_Character := Default_Radix_Mark) is
      begin
         Tail (
            To,
            Image (Item, Pic, Currency, Fill, Separator, Radix_Mark));
      end Put;

   end Decimal_Output;

end Ada.Wide_Text_IO.Editing;
