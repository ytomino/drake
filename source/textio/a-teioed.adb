pragma Check_Policy (Trace, Off);
with Ada.Exceptions;
with Ada.Text_IO.Inside.Formatting;
with System.Formatting.Decimal_Image;
with System.Val_Uns;
package body Ada.Text_IO.Editing is
   use type System.Formatting.Unsigned;

   function Length (Pic : Picture; Currency : String := Default_Currency)
      return Natural;
   function Length (Pic : Picture; Currency : String := Default_Currency)
      return Natural
   is
      Result : Natural := Pic.Length;
   begin
      if Pic.Has_V then
         Result := Result - 1;
      end if;
      if Pic.Has_Dollar /= None then
         Result := Result + Currency'Length - 1;
      end if;
      return Result;
   end Length;

   function Image (
      Item : Long_Long_Integer;
      Scale : Integer;
      Fore : Integer;
      Pic : Picture;
      Currency : String := Default_Currency;
      Fill : Character := Default_Fill;
      Separator : Character := Default_Separator;
      Radix_Mark : Character := Default_Radix_Mark)
      return String;
   function Image (
      Item : Long_Long_Integer;
      Scale : Integer;
      Fore : Integer;
      Pic : Picture;
      Currency : String := Default_Currency;
      Fill : Character := Default_Fill;
      Separator : Character := Default_Separator;
      Radix_Mark : Character := Default_Radix_Mark)
      return String is
   begin
      return Result : String (1 .. Length (Pic, Currency)) do
         if Pic.Real_Blank_When_Zero and then Item = 0 then
            for I in Result'Range loop
               Result (I) := ' ';
            end loop;
         else
            declare
               Aft : constant Natural := Pic.Aft;
               Item_Image : String (1 .. Fore + Aft + 2); -- sign and '.'
               Item_Last : Natural;
               Radix_Position : Integer := Pic.Radix_Position;
            begin
               if Pic.Has_Dollar = Previous then
                  Radix_Position := Radix_Position + Currency'Length - 1;
               end if;
               System.Formatting.Decimal_Image (
                  Item,
                  Item_Image,
                  Item_Last,
                  Scale,
                  Fore_Width => Fore,
                  Fore_Padding => ' ',
                  Aft_Width => Aft);
               --  fix
               if Item_Image (Item_Image'First + Fore - 1) = ' '
                  and then Item_Image (Item_Image'First + Fore) = '0'
               then
                  Item_Image (Item_Image'First + Fore) := ' ';
               end if;
               declare
                  Result_Index : Natural := Radix_Position - 1;
                  Pic_Index : Natural := Pic.Radix_Position - 1;
                  Pic_Leading_Index : Natural := Pic_Index;
                  Sign_Filled : Boolean := False;
                  Paren_Filled : Boolean := False;
                  Currency_Filled : Boolean := False;
                  Dollar_Used : Boolean := False;
               begin
                  for I in reverse
                     Item_Image'First + 1 .. -- skip sign
                     Item_Image'First + Fore
                  loop
                     exit when Item_Image (I) = ' ';
                     if Result_Index < Result'First then
                        pragma Check (Trace, Debug.Put (
                           "width of"
                           & Long_Long_Integer'Image (Item)
                           & "* 10**"
                           & Integer'Image (-Scale)
                           & " is too longer for "
                           & Pic_String (Pic)));
                        Exceptions.Raise_Exception_From_Here (
                           Layout_Error'Identity);
                     end if;
                     loop
                        pragma Assert (Pic_Index >= Pic.Expanded'First);
                        if Pic.Expanded (Pic_Index) = '>' then
                           if Item < 0 then
                              Result (Result_Index) := ')';
                           else
                              Result (Result_Index) := ' ';
                           end if;
                           Result_Index := Result_Index - 1;
                           Pic_Index := Pic_Index - 1;
                        elsif Pic.Expanded (Pic_Index) = '_' then
                           Result (Result_Index) := Separator;
                           Result_Index := Result_Index - 1;
                           Pic_Index := Pic_Index - 1;
                        elsif Pic.Expanded (Pic_Index) = '-'
                           and then Pic_Index = Pic.First_Sign_Position
                        then
                           if Item < 0 then
                              Result (Result_Index) := '-';
                           else
                              Result (Result_Index) := '+';
                           end if;
                           Result_Index := Result_Index - 1;
                           Pic_Index := Pic_Index - 1;
                           Sign_Filled := True;
                        else
                           exit;
                        end if;
                     end loop;
                     Result (Result_Index) := Item_Image (I);
                     Result_Index := Result_Index - 1;
                     Pic_Index := Pic_Index - 1;
                  end loop;
                  while Result_Index >= Result'First loop
                     if Pic_Leading_Index > Pic_Index then
                        Pic_Leading_Index := Pic_Index;
                     end if;
                     pragma Assert (Pic_Leading_Index >= Pic.Expanded'First);
                     case Pic.Expanded (Pic_Leading_Index) is
                        when '$' =>
                           if Currency_Filled then
                              Result (Result_Index) := ' ';
                              Result_Index := Result_Index - 1;
                           else
                              Result (
                                 Result_Index - Currency'Length + 1 ..
                                 Result_Index) := Currency;
                              Result_Index := Result_Index - Currency'Length;
                              Currency_Filled := True;
                              Dollar_Used := True;
                           end if;
                           Pic_Index := Pic_Index - 1;
                        when '#' =>
                           if Currency_Filled then
                              Result (Result_Index) := ' ';
                              Result_Index := Result_Index - 1;
                              Pic_Index := Pic_Index - 1;
                           else
                              declare
                                 N : Natural := Currency'Length;
                              begin
                                 while N > 0 loop
                                    if Pic_Index <= Pic_Leading_Index
                                       and then (
                                          Pic_Index < Pic.Expanded'First
                                          or else (
                                             Pic.Expanded (Pic_Index) /= '#'
                                             and then
                                             Pic.Expanded (Pic_Index) /= '_'))
                                    then
                                       Exceptions.Raise_Exception_From_Here (
                                          Layout_Error'Identity);
                                    end if;
                                    Pic_Index := Pic_Index - 1;
                                    N := N - 1;
                                 end loop;
                              end;
                              Result (
                                 Result_Index - Currency'Length + 1 ..
                                 Result_Index) := Currency;
                              Result_Index := Result_Index - Currency'Length;
                              Currency_Filled := True;
                              Pic_Leading_Index := Pic_Index;
                           end if;
                        when '+' =>
                           if Sign_Filled then
                              Result (Result_Index) := ' ';
                           elsif Item < 0 then
                              Result (Result_Index) := '-';
                              Sign_Filled := True;
                           else
                              Result (Result_Index) := '+';
                              Sign_Filled := True;
                           end if;
                           Result_Index := Result_Index - 1;
                           Pic_Index := Pic_Index - 1;
                        when '-' =>
                           if Sign_Filled then
                              Result (Result_Index) := ' ';
                           elsif Item < 0 then
                              Result (Result_Index) := '-';
                              Sign_Filled := True;
                           else
                              Result (Result_Index) := ' ';
                           end if;
                           Result_Index := Result_Index - 1;
                           Pic_Index := Pic_Index - 1;
                        when '<' =>
                           if not Paren_Filled and then Item < 0 then
                              Result (Result_Index) := '(';
                              Paren_Filled := True;
                           else
                              Result (Result_Index) := ' ';
                           end if;
                           Result_Index := Result_Index - 1;
                           Pic_Index := Pic_Index - 1;
                        when '9' =>
                           Result (Result_Index) := '0';
                           Result_Index := Result_Index - 1;
                           Pic_Index := Pic_Index - 1;
                        when '*' =>
                           Result (Result_Index) := Fill;
                           Result_Index := Result_Index - 1;
                           Pic_Index := Pic_Index - 1;
                        when '_' =>
                           if Pic_Leading_Index > Pic.Expanded'First
                              and then Pic.Expanded (Pic_Leading_Index - 1) =
                                 Pic.Expanded (Pic_Leading_Index + 1)
                           then
                              Pic_Leading_Index := Pic_Leading_Index - 1;
                           else
                              Result (Result_Index) := ' ';
                              Result_Index := Result_Index - 1;
                              Pic_Index := Pic_Index - 1;
                           end if;
                        when others =>
                           Result (Result_Index) := ' ';
                           Result_Index := Result_Index - 1;
                           Pic_Index := Pic_Index - 1;
                     end case;
                  end loop;
                  if (Item < 0
                        and then not Sign_Filled
                        and then not Paren_Filled) -- minus is not presented
                     or else (Pic.Has_Dollar = Previous
                        and then not Dollar_Used) -- all $ used for item
                  then
                     Exceptions.Raise_Exception_From_Here (
                        Layout_Error'Identity);
                  end if;
               end;
               declare
                  Result_Index : Positive := Radix_Position;
                  Pic_Index : Natural := Pic.Radix_Position + 1;
                  Paren_Filled : Boolean := False;
                  Currency_Filled : Boolean := False;
               begin
                  if Pic.Radix_Position <= Pic.Length
                     and then Pic.Expanded (Pic.Radix_Position) = '.'
                  then
                     pragma Assert (Result_Index <= Result'Last);
                     Result (Result_Index) := Radix_Mark;
                     Result_Index := Result_Index + 1;
                  end if;
                  for I in
                     Item_Image'First + Fore + 2 ..
                     Item_Image'Last
                  loop
                     exit when Result_Index > Result'Last;
                     pragma Assert (Pic_Index <= Pic.Length);
                     if Pic.Expanded (Pic_Index) = '_' then
                        Result (Result_Index) := Separator;
                        Result_Index := Result_Index + 1;
                        Pic_Index := Pic_Index + 1;
                     end if;
                     Result (Result_Index) := Item_Image (I);
                     Result_Index := Result_Index + 1;
                     Pic_Index := Pic_Index + 1;
                  end loop;
                  while Result_Index <= Result'Last loop
                     pragma Assert (Pic_Index >= Pic.Expanded'First);
                     case Pic.Expanded (Pic_Index) is
                        when '#' =>
                           if Currency_Filled then
                              Result (Result_Index) := ' ';
                              Result_Index := Result_Index + 1;
                              Pic_Index := Pic_Index + 1;
                           else
                              declare
                                 N : Natural := Currency'Length;
                              begin
                                 while N > 0 loop
                                    if Pic_Index > Pic.Length
                                       or else Pic.Expanded (Pic_Index) /= '#'
                                    then
                                       Exceptions.Raise_Exception_From_Here (
                                          Layout_Error'Identity);
                                    end if;
                                    Pic_Index := Pic_Index + 1;
                                    N := N - 1;
                                 end loop;
                              end;
                              Result (
                                 Result_Index ..
                                 Result_Index + Currency'Length - 1) :=
                                 Currency;
                              Result_Index := Result_Index + Currency'Length;
                              Currency_Filled := True;
                           end if;
                        when '>' =>
                           if not Paren_Filled and then Item < 0 then
                              Result (Result_Index) := ')';
                              Paren_Filled := True;
                           else
                              Result (Result_Index) := ' ';
                           end if;
                           Result_Index := Result_Index + 1;
                           Pic_Index := Pic_Index + 1;
                        when '_' =>
                           Result (Result_Index) := Separator;
                           Result_Index := Result_Index + 1;
                           Pic_Index := Pic_Index + 1;
                        when others =>
                           Result (Result_Index) := ' ';
                           Result_Index := Result_Index + 1;
                           Pic_Index := Pic_Index + 1;
                     end case;
                  end loop;
               end;
            end;
         end if;
      end return;
   end Image;

   --  implementation

   function Valid (
      Pic_String : String;
      Blank_When_Zero : Boolean := False)
      return Boolean is
   begin
      declare
         Dummy : constant Picture := To_Picture (Pic_String, Blank_When_Zero);
         pragma Unreferenced (Dummy);
      begin
         return True;
      end;
   exception
      when Picture_Error =>
         return False;
   end Valid;

   function To_Picture (
      Pic_String : String;
      Blank_When_Zero : Boolean := False)
      return Picture
   is
      type Currency_State is (None, Dollar, Sharp);
      pragma Discard_Names (Currency_State);
      Currency : Currency_State := None;
      type Paren_State is (None, Opened, Closed);
      pragma Discard_Names (Paren_State);
      Paren : Paren_State := None;
      type Sign_State is (None, Plus, Minus);
      pragma Discard_Names (Sign_State);
      Sign : Sign_State := None;
      type Suppression_State is (None, Z, Asterisk);
      pragma Discard_Names (Suppression_State);
      Suppression : Suppression_State := None;
      type State_Type is (
         Start,
         Any_Sign,
         B_After_Sign,
         Any_Suppression,
         Nine,
         Radix,
         Fraction);
      pragma Discard_Names (State_Type);
      State : State_Type := Start;
      I : Positive := Pic_String'First;
   begin
      return Result : Picture do
         Result.Length := 0;
         Result.Has_V := False;
         Result.Has_Dollar := None;
         Result.Blank_When_Zero := Blank_When_Zero;
         Result.Real_Blank_When_Zero := True;
         Result.First_Sign_Position := 0;
         Result.Aft := 0;
         while I <= Pic_String'Last loop
            case Pic_String (I) is
               when '(' =>
                  declare
                     Count_First : Positive;
                     Count_Last : Natural;
                     Count : System.Formatting.Unsigned;
                  begin
                     Inside.Formatting.Get_Tail (
                        Pic_String (I + 1 .. Pic_String'Last),
                        First => Count_First);
                     declare
                        Error : Boolean;
                     begin
                        System.Val_Uns.Get_Unsigned_Literal (
                           Pic_String (Count_First .. Pic_String'Last),
                           Count_Last,
                           Count,
                           Error => Error);
                        if Error then
                           Exceptions.Raise_Exception_From_Here (
                              Picture_Error'Identity);
                        end if;
                     end;
                     if Count = 0
                        or else I = Pic_String'First
                        or else I + Integer (Count) - 1 > Pic_String'Last
                     then
                        Exceptions.Raise_Exception_From_Here (
                           Picture_Error'Identity);
                     end if;
                     for J in 2 .. Count loop
                        Result.Length := Result.Length + 1;
                        Result.Expanded (Result.Length) := Pic_String (I - 1);
                     end loop;
                     I := Count_Last + 1;
                  end;
               when ')' =>
                  Exceptions.Raise_Exception_From_Here (
                     Picture_Error'Identity);
               when others =>
                  Result.Length := Result.Length + 1;
                  if Result.Length > Result.Expanded'Last then
                     Exceptions.Raise_Exception_From_Here (
                        Picture_Error'Identity);
                  end if;
                  Result.Expanded (Result.Length) := Pic_String (I);
                  case Pic_String (I) is
                     when '$' =>
                        if Currency = Sharp then
                           Exceptions.Raise_Exception_From_Here (
                              Picture_Error'Identity);
                        end if;
                        Currency := Dollar;
                        if State >= Radix then
                           if Result.Has_Dollar = None then
                              Exceptions.Raise_Exception_From_Here (
                                 Layout_Error'Identity);
                           end if;
                           Result.Aft := Result.Aft + 1;
                        else
                           Result.Has_Dollar := Previous;
                        end if;
                     when '#' =>
                        if Currency = Dollar then
                           Exceptions.Raise_Exception_From_Here (
                              Picture_Error'Identity);
                        end if;
                        Currency := Sharp;
                     when '<' =>
                        if Paren = Closed then
                           Exceptions.Raise_Exception_From_Here (
                              Picture_Error'Identity);
                        end if;
                        Paren := Opened;
                        if Result.First_Sign_Position = 0 then
                           Result.First_Sign_Position := Result.Length;
                        end if;
                        if State >= Radix then
                           State := Fraction;
                           Result.Aft := Result.Aft + 1;
                        end if;
                     when '>' =>
                        if Paren /= Opened then
                           Exceptions.Raise_Exception_From_Here (
                              Picture_Error'Identity);
                        end if;
                        Paren := Closed;
                     when '+' =>
                        if Sign = Minus or else State >= Radix then
                           Exceptions.Raise_Exception_From_Here (
                              Picture_Error'Identity);
                        end if;
                        if Result.First_Sign_Position = 0 then
                           Result.First_Sign_Position := Result.Length;
                        end if;
                        Sign := Plus;
                        if State < Any_Sign then
                           State := Any_Sign;
                        end if;
                     when '-' =>
                        if Sign = Plus or else State >= Radix then
                           Exceptions.Raise_Exception_From_Here (
                              Picture_Error'Identity);
                        end if;
                        if Result.First_Sign_Position = 0 then
                           Result.First_Sign_Position := Result.Length;
                        end if;
                        Sign := Minus;
                        if State < Any_Sign then
                           State := Any_Sign;
                        end if;
                     when '*' =>
                        if Blank_When_Zero then
                           Exceptions.Raise_Exception_From_Here (
                              Picture_Error'Identity);
                        end if;
                        Result.Real_Blank_When_Zero := False;
                        if State >= Radix then
                           State := Fraction;
                           Result.Aft := Result.Aft + 1;
                        elsif Suppression = Z
                           or else State > Any_Suppression
                        then
                           Exceptions.Raise_Exception_From_Here (
                              Picture_Error'Identity);
                        else
                           Suppression := Asterisk;
                           State := Any_Suppression;
                        end if;
                     when 'Z' | 'z' =>
                        Result.Expanded (Result.Length) := 'Z';
                        if State >= Radix then
                           State := Fraction;
                           Result.Aft := Result.Aft + 1;
                        elsif Suppression = Asterisk
                           or else State > Any_Suppression
                        then
                           Exceptions.Raise_Exception_From_Here (
                              Picture_Error'Identity);
                        else
                           Suppression := Z;
                           State := Any_Suppression;
                        end if;
                     when '9' =>
                        Result.Real_Blank_When_Zero := False;
                        if State >= Radix then
                           State := Fraction;
                           Result.Aft := Result.Aft + 1;
                        elsif State > Nine then
                           Exceptions.Raise_Exception_From_Here (
                              Picture_Error'Identity);
                        else
                           State := Nine;
                        end if;
                     when '.' =>
                        if State > Radix
                           or else (
                              State <= Any_Sign
                              and then Currency = None
                              and then Paren = None)
                        then
                           Exceptions.Raise_Exception_From_Here (
                              Picture_Error'Identity);
                        end if;
                        State := Radix;
                        Result.Radix_Position := Result.Length;
                     when 'V' | 'v' =>
                        Result.Expanded (Result.Length) := 'V';
                        if State > Radix
                           or else (
                              State <= Any_Sign
                              and then Currency = None
                              and then Paren = None)
                        then
                           Exceptions.Raise_Exception_From_Here (
                              Picture_Error'Identity);
                        end if;
                        State := Radix;
                        Result.Radix_Position := Result.Length;
                        Result.Has_V := True;
                     when 'B' | 'b' =>
                        Result.Expanded (Result.Length) := 'B';
                        if State = Any_Sign then
                           State := B_After_Sign;
                        end if;
                     when '_' | '/' | '0' =>
                        null;
                     when others =>
                        Exceptions.Raise_Exception_From_Here (
                           Picture_Error'Identity);
                  end case;
                  I := I + 1;
            end case;
         end loop;
         if Paren = Opened
            or else (
               State < Any_Sign
               and then Currency = None
               and then Paren = None)
            or else State = B_After_Sign -- CXF3A01
         then
            Exceptions.Raise_Exception_From_Here (Picture_Error'Identity);
         end if;
         if State < Radix then
            Result.Radix_Position := Result.Length + 1;
         end if;
         Result.Real_Blank_When_Zero :=
            (Result.Real_Blank_When_Zero and then State > Any_Suppression)
            or else Blank_When_Zero;
      end return;
   end To_Picture;

   function Pic_String (Pic : Picture) return String is
   begin
      return Pic.Expanded (1 .. Pic.Length);
   end Pic_String;

   function Blank_When_Zero (Pic : Picture) return Boolean is
   begin
      return Pic.Blank_When_Zero;
   end Blank_When_Zero;

   package body Decimal_Output is

      function Length (Pic : Picture; Currency : String := Default_Currency)
         return Natural
         renames Editing.Length;

      function Valid (
         Item : Num;
         Pic : Picture;
         Currency : String := Default_Currency)
         return Boolean is
      begin
         declare
            Dummy : constant String := Image (Item, Pic, Currency);
            pragma Unreferenced (Dummy);
         begin
            return True;
         end;
      exception
         when Layout_Error =>
            return False;
      end Valid;

      function Image (
         Item : Num;
         Pic : Picture;
         Currency : String := Default_Currency;
         Fill : Character := Default_Fill;
         Separator : Character := Default_Separator;
         Radix_Mark : Character := Default_Radix_Mark)
         return String is
      begin
         return Editing.Image (
            Long_Long_Integer'Integer_Value (Item),
            Num'Scale,
            Num'Fore,
            Pic,
            Currency,
            Fill,
            Separator,
            Radix_Mark);
      end Image;

      procedure Put (
         File : File_Type;
         Item : Num;
         Pic : Picture;
         Currency : String := Default_Currency;
         Fill : Character := Default_Fill;
         Separator : Character := Default_Separator;
         Radix_Mark : Character := Default_Radix_Mark) is
      begin
         Put (File, Image (Item, Pic, Currency, Fill, Separator, Radix_Mark));
      end Put;

      procedure Put (
         Item : Num;
         Pic : Picture;
         Currency : String := Default_Currency;
         Fill : Character := Default_Fill;
         Separator : Character := Default_Separator;
         Radix_Mark : Character := Default_Radix_Mark) is
      begin
         Put (
            Current_Output.all,
            Image (Item, Pic, Currency, Fill, Separator, Radix_Mark));
      end Put;

      procedure Put (
         To : out String;
         Item : Num;
         Pic : Picture;
         Currency : String := Default_Currency;
         Fill : Character := Default_Fill;
         Separator : Character := Default_Separator;
         Radix_Mark : Character := Default_Radix_Mark) is
      begin
         Inside.Formatting.Tail (
            To,
            Image (Item, Pic, Currency, Fill, Separator, Radix_Mark));
      end Put;

   end Decimal_Output;

end Ada.Text_IO.Editing;
