package body System.UTF_Conversions is
   pragma Suppress (All_Checks);

   procedure To_UTF_8 (
      Code : UCS_4;
      Result : out String;
      Last : out Natural;
      Error : out Boolean)
   is
      I : constant Natural := Result'First;
      Length : Natural;
      Code_2 : UCS_4;
   begin
      if I > Result'Last then
         Last := Result'Last;
         Error := True; --  overflow
         return;
      end if;
      Error := False;
      case Code is
         when 0 .. 16#7f# =>
            Last := I;
            Result (I) := Character'Val (Code);
            return;
         when 16#80# .. 2 ** (5 + 6) - 1 =>
            Result (I) := Character'Val (
               2#11000000# or Code / (2 ** 6));
            Length := 2;
         when 2 ** (5 + 6) .. 2 ** (4 + 6 + 6) - 1 =>
            if Code in 16#d800# .. 16#dfff# then
               Error := True; --  range of surrogate pair
            end if;
            Result (I) := Character'Val (
               2#11100000# or Code / (2 ** 12));
            Length := 3;
         when 2 ** (4 + 6 + 6) .. 2 ** (3 + 6 + 6 + 6) - 1 =>
            Result (I) := Character'Val (
               2#11110000# or Code / (2 ** 18));
            Length := 4;
         when 2 ** (3 + 6 + 6 + 6) .. 2 ** (2 + 6 + 6 + 6 + 6) - 1 =>
            Result (I) := Character'Val (
               2#11111000# or Code / (2 ** 24));
            Length := 5;
         when 2 ** (2 + 6 + 6 + 6 + 6) .. 2 ** (1 + 6 + 6 + 6 + 6 + 6) - 1 =>
            Result (I) := Character'Val (
               2#11111100# or Code / (2 ** 30));
            Length := 6;
      end case;
      Code_2 := Code;
      Last := I + Length - 1;
      if Last > Result'Last then
         Last := Result'Last;
         Length := Result'Last + 1 - I;
         Error := True; --  overflow
      end if;
      for J in reverse 2 .. Length loop
         Result (I + J - 1) := Character'Val (
            2#10000000# or (Code_2 and (2 ** 6 - 1)));
         Code_2 := Code_2 / (2 ** 6);
      end loop;
   end To_UTF_8;

   procedure From_UTF_8 (
      Data : String;
      Last : out Natural;
      Result : out UCS_4;
      Error : out Boolean)
   is
      I : Natural := Data'First;
      First : constant Character := Data (I);
      Trail : Character;
      Length : Natural;
      Code : UCS_4;
   begin
      Error := False;
      --  leading byte
      case First is
         when Character'Val (2#00000000#) .. Character'Val (2#01111111#) =>
            Last := I;
            Result := Character'Pos (First);
            return;
         when Character'Val (2#11000000#) .. Character'Val (2#11011111#) =>
            Code := Character'Pos (First) and 2#00011111#;
            Length := 2;
         when Character'Val (2#11100000#) .. Character'Val (2#11101111#) =>
            Code := Character'Pos (First) and 2#00001111#;
            Length := 3;
         when Character'Val (2#11110000#) .. Character'Val (2#11110111#) =>
            Code := Character'Pos (First) and 2#00000111#;
            Length := 4;
         when Character'Val (2#11111000#) .. Character'Val (2#11111011#) =>
            Code := Character'Pos (First) and 2#00000011#;
            Length := 5;
         when Character'Val (2#11111100#) .. Character'Val (2#11111101#) =>
            Code := Character'Pos (First) and 2#00000001#;
            Length := 6;
         when others =>
            Last := I;
            Result := Character'Pos (First);
            Error := True; --  trailing byte or invalid code
            return;
      end case;
      --  trailing bytes
      for J in 2 .. Length loop
         if I >= Data'Last then
            Code := Shift_Left (Code, 6 * (Length - J + 1));
            Error := True; --  trailing byte is nothing
            exit;
         else
            I := I + 1;
            Trail := Data (I);
            if Trail not in
               Character'Val (2#10000000#) .. Character'Val (2#10111111#)
            then
               I := I - 1;
               Code := Shift_Left (Code, 6 * (Length - J + 1));
               Error := True; -- trailing byte is invalid
               exit;
            end if;
         end if;
         Code := Code * (2 ** 6) or
            (Character'Pos (Trail) and (2 ** 6 - 1));
      end loop;
      if Code in 16#d800# .. 16#dfff# then
         Error := True; --  range of surrogate pair
      end if;
      Last := I;
      Result := Code;
   end From_UTF_8;

   procedure From_UTF_8_Reverse (
      Data : String;
      First : out Positive;
      Result : out UCS_4;
      Error : out Boolean)
   is
      Length : Positive;
      Last : Natural; -- ignore
   begin
      First := Data'Last;
      while First > Data'First
         and then Data (First) in
            Character'Val (2#10000000#) .. Character'Val (2#10111111#)
      loop
         First := First - 1;
      end loop;
      UTF_8_Sequence (Data (First), Length, Error);
      if First + Length - 1 < Data'Last then
         First := Data'Last;
         Result := Character'Pos (Data (First));
         Error := True;
      else
         From_UTF_8 (Data (First .. Data'Last), Last, Result, Error);
      end if;
   end From_UTF_8_Reverse;

   procedure UTF_8_Sequence (
      Leading : Character;
      Result : out Positive;
      Error : out Boolean) is
   begin
      case Leading is
         when Character'Val (2#00000000#) .. Character'Val (2#01111111#) =>
            Result := 1;
            Error := False;
         when Character'Val (2#11000000#) .. Character'Val (2#11011111#) =>
            Result := 2;
            Error := False;
         when Character'Val (2#11100000#) .. Character'Val (2#11101111#) =>
            Result := 3;
            Error := False;
         when Character'Val (2#11110000#) .. Character'Val (2#11110111#) =>
            Result := 4;
            Error := False;
         when Character'Val (2#11111000#) .. Character'Val (2#11111011#) =>
            Result := 5;
            Error := False;
         when Character'Val (2#11111100#) .. Character'Val (2#11111101#) =>
            Result := 6;
            Error := False;
         when others =>
            Result := 1;
            Error := True; -- trailing byte or invalid code
      end case;
   end UTF_8_Sequence;

   procedure To_UTF_16 (
      Code : UCS_4;
      Result : out Wide_String;
      Last : out Natural;
      Error : out Boolean) is
   begin
      case Code is
         when 16#0000# .. 16#d7ff#
            | 16#d800# .. 16#dfff# -- without checking surrogate pair in To_XXX
            | 16#e000# .. 16#ffff# =>
            Last := Result'First;
            if Last <= Result'Last then
               Result (Last) := Wide_Character'Val (Code);
               Error := False;
            else
               Last := Result'Last;
               Error := True; -- overflow
            end if;
         when 16#00010000# .. UCS_4'Last =>
            Last := Result'First;
            if Last <= Result'Last then
               declare
                  Code_2 : constant UCS_4 := Code - 16#00010000#;
               begin
                  Result (Last) := Wide_Character'Val (
                     16#d800# or (Code_2 / (2 ** 10)));
                  Last := Last + 1;
                  if Last <= Result'Last then
                     Result (Last) := Wide_Character'Val (
                        16#dc00# or (Code_2 and (2 ** 10 - 1)));
                     Error := Code_2 >= 2 ** 20; --  over range of UTF-16
                  else
                     Last := Result'Last; --  overflow
                     Error := True;
                  end if;
               end;
            else
               Last := Result'Last; --  overflow
               Error := True;
            end if;
      end case;
   end To_UTF_16;

   procedure From_UTF_16 (
      Data : Wide_String;
      Last : out Natural;
      Result : out UCS_4;
      Error : out Boolean)
   is
      I : Natural := Data'First;
      First : constant Wide_Character := Data (I);
   begin
      case First is
         when Wide_Character'Val (16#0000#) .. Wide_Character'Val (16#d7ff#)
            | Wide_Character'Val (16#e000#) .. Wide_Character'Val (16#ffff#) =>
            Last := I;
            Result := Wide_Character'Pos (First);
            Error := False;
         when Wide_Character'Val (16#d800#) .. Wide_Character'Val (16#dbff#) =>
            declare
               Second : Wide_Character;
            begin
               if I >= Data'Last then
                  Error := True; --  trailing byte is nothing
                  Second := Wide_Character'Val (0);
               else
                  I := I + 1;
                  Second := Data (I);
                  if Second not in
                     Wide_Character'Val (16#dc00#) ..
                     Wide_Character'Val (16#dfff#)
                  then
                     I := I - 1;
                     Second := Wide_Character'Val (0);
                     Error := True; --  trailing byte is invalid
                  else
                     Error := False;
                  end if;
               end if;
               Last := I;
               declare
                  High : constant UCS_4 :=
                     Wide_Character'Pos (First) and (2 ** 10 - 1);
                  Low : constant UCS_4 :=
                     Wide_Character'Pos (Second) and (2 ** 10 - 1);
               begin
                  Result := (High * (2 ** 10)) or Low;
               end;
            end;
         when Wide_Character'Val (16#dc00#) .. Wide_Character'Val (16#dfff#) =>
            Last := I;
            Result := Wide_Character'Pos (First);
            Error := True; --  trailing byte
      end case;
   end From_UTF_16;

   procedure From_UTF_16_Reverse (
      Data : Wide_String;
      First : out Positive;
      Result : out UCS_4;
      Error : out Boolean)
   is
      Last : Natural; -- ignore
   begin
      if Data'First < Data'Last
         and then Data (Data'Last) in
            Wide_Character'Val (16#dc00#) .. Wide_Character'Val (16#dfff#)
         and then Data (Data'Last - 1) in
            Wide_Character'Val (16#d800#) .. Wide_Character'Val (16#dbff#)
      then
         First := Data'Last - 1;
      else
         First := Data'Last;
      end if;
      From_UTF_16 (Data (First .. Data'Last), Last, Result, Error);
   end From_UTF_16_Reverse;

   procedure UTF_16_Sequence (
      Leading : Wide_Character;
      Result : out Positive;
      Error : out Boolean) is
   begin
      case Leading is
         when Wide_Character'Val (16#0000#) .. Wide_Character'Val (16#d7ff#)
            | Wide_Character'Val (16#e000#) .. Wide_Character'Val (16#ffff#) =>
            Result := 1;
            Error := False;
         when Wide_Character'Val (16#d800#) .. Wide_Character'Val (16#dbff#) =>
            Result := 2;
            Error := False;
         when Wide_Character'Val (16#dc00#) .. Wide_Character'Val (16#dfff#) =>
            Result := 1;
            Error := True; --  trailing byte
      end case;
   end UTF_16_Sequence;

   procedure To_UTF_32 (
      Code : UCS_4;
      Result : out Wide_Wide_String;
      Last : out Natural;
      Error : out Boolean) is
   begin
      Last := Result'First;
      if Last <= Result'Last then
         Result (Last) := Wide_Wide_Character'Val (Code);
         Error := False;
      else
         Last := Result'Last;
         Error := True; -- overflow
      end if;
   end To_UTF_32;

   procedure From_UTF_32 (
      Data : Wide_Wide_String;
      Last : out Natural;
      Result : out UCS_4;
      Error : out Boolean)
   is
      type Unsigned_32 is mod 2 ** Wide_Wide_Character'Size;
      Code : constant Unsigned_32 :=
         Wide_Wide_Character'Pos (Data (Data'First));
   begin
      Last := Data'First;
      Result := UCS_4'Mod (Code);
      case Code is
         when 16#d800# .. 16#dfff# | 16#80000000# .. 16#ffffffff# =>
            Error := True;
         when others =>
            Error := False;
      end case;
   end From_UTF_32;

   procedure From_UTF_32_Reverse (
      Data : Wide_Wide_String;
      First : out Positive;
      Result : out UCS_4;
      Error : out Boolean)
   is
      Last : Natural; -- ignored
   begin
      First := Data'Last;
      From_UTF_32 (Data (First .. Data'Last), Last, Result, Error);
   end From_UTF_32_Reverse;

   procedure UTF_32_Sequence (
      Leading : Wide_Wide_Character;
      Result : out Positive;
      Error : out Boolean) is
   begin
      Result := 1;
      case Wide_Wide_Character'Pos (Leading) is
         when 16#d800# .. 16#dfff# | 16#80000000# .. 16#ffffffff# =>
            Error := True;
         when others =>
            Error := False;
      end case;
   end UTF_32_Sequence;

   procedure UTF_8_To_UTF_16 (
      Data : String;
      Result : out Wide_String;
      Last : out Natural;
      Error : out Boolean)
   is
      I : Natural := Data'First;
      J : Natural := Result'First;
   begin
      Last := J - 1;
      Error := False;
      while I <= Data'Last loop
         declare
            Code : UCS_4;
            Next : Natural;
            E : Boolean;
         begin
            From_UTF_8 (Data (I .. Data'Last), Next, Code, E);
            Error := Error or E;
            I := Next + 1;
            To_UTF_16 (Code, Result (J .. Result'Last), Last, E);
            Error := Error or E;
            J := Last + 1;
         end;
      end loop;
   end UTF_8_To_UTF_16;

   procedure UTF_8_To_UTF_32 (
      Data : String;
      Result : out Wide_Wide_String;
      Last : out Natural;
      Error : out Boolean)
   is
      I : Natural := Data'First;
      J : Natural := Result'First;
   begin
      Last := J - 1;
      Error := False;
      while I <= Data'Last loop
         declare
            Code : UCS_4;
            Next : Natural;
            E : Boolean;
         begin
            From_UTF_8 (Data (I .. Data'Last), Next, Code, E);
            Error := Error or E;
            I := Next + 1;
            Result (J) := Wide_Wide_Character'Val (Code);
            Last := J;
            J := Last + 1;
         end;
      end loop;
   end UTF_8_To_UTF_32;

   procedure UTF_16_To_UTF_8 (
      Data : Wide_String;
      Result : out String;
      Last : out Natural;
      Error : out Boolean)
   is
      I : Natural := Data'First;
      J : Natural := Result'First;
   begin
      Last := J - 1;
      Error := False;
      while I <= Data'Last loop
         declare
            Code : UCS_4;
            Next : Natural;
            E : Boolean;
         begin
            From_UTF_16 (Data (I .. Data'Last), Next, Code, E);
            Error := Error or E;
            I := Next + 1;
            To_UTF_8 (Code, Result (J .. Result'Last), Last, E);
            Error := Error or E;
            J := Last + 1;
         end;
      end loop;
   end UTF_16_To_UTF_8;

   procedure UTF_32_To_UTF_8 (
      Data : Wide_Wide_String;
      Result : out String;
      Last : out Natural;
      Error : out Boolean)
   is
      J : Natural := Result'First;
   begin
      Last := J - 1;
      Error := False;
      for I in Data'Range loop
         declare
            Code : UCS_4;
            Next : Natural;
            E : Boolean;
         begin
            From_UTF_32 (Data (I .. Data'Last), Next, Code, E);
            Error := Error or E;
            To_UTF_8 (Code, Result (J .. Result'Last), Last, E);
            Error := Error or E;
            J := Last + 1;
         end;
      end loop;
   end UTF_32_To_UTF_8;

end System.UTF_Conversions;
