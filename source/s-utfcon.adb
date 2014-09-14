package body System.UTF_Conversions is
   pragma Suppress (All_Checks);

   procedure UTF_8_Length (
      Code : UCS_4;
      Leading : out Character;
      Length : out Natural;
      Status : out Sequence_Status_Type);
   procedure UTF_8_Length (
      Code : UCS_4;
      Leading : out Character;
      Length : out Natural;
      Status : out Sequence_Status_Type) is
   begin
      case Code is
         when 0 .. 16#7f# =>
            Leading := Character'Val (Code);
            Length := 1;
            Status := Success;
         when 16#80# .. 2 ** (5 + 6) - 1 =>
            Leading := Character'Val (2#11000000# or Code / (2 ** 6));
            Length := 2;
            Status := Success;
         when 16#d800# .. 16#dfff# =>
            Leading := Character'Val (2#11100000# or Code / (2 ** 12));
            Length := 3;
            Status := Illegal_Sequence; -- range of surrogate pair
         when 2 ** (5 + 6) .. 16#d7ff# | 16#e000# .. 2 ** (4 + 6 + 6) - 1 =>
            Leading := Character'Val (2#11100000# or Code / (2 ** 12));
            Length := 3;
            Status := Success;
         when 2 ** (4 + 6 + 6) .. 2 ** (3 + 6 + 6 + 6) - 1 =>
            Leading := Character'Val (2#11110000# or Code / (2 ** 18));
            Length := 4;
            Status := Success;
         when 2 ** (3 + 6 + 6 + 6) .. 2 ** (2 + 6 + 6 + 6 + 6) - 1 =>
            Leading := Character'Val (2#11111000# or Code / (2 ** 24));
            Length := 5;
            Status := Success;
         when 2 ** (2 + 6 + 6 + 6 + 6) .. 2 ** (1 + 6 + 6 + 6 + 6 + 6) - 1 =>
            Leading := Character'Val (2#11111100# or Code / (2 ** 30));
            Length := 6;
            Status := Success;
      end case;
   end UTF_8_Length;

   --  implementation

   procedure To_UTF_8 (
      Code : UCS_4;
      Result : out String;
      Last : out Natural;
      Status : out To_Status_Type)
   is
      I : constant Natural := Result'First;
      Length : Natural;
      Code_2 : UCS_4;
      Dummy_Sequence_Status : Sequence_Status_Type;
      --  without checking surrogate pair in To_XXX
   begin
      if I > Result'Last then
         Last := Result'Last;
         Status := Overflow;
         return;
      end if;
      UTF_8_Length (Code, Result (I), Length, Dummy_Sequence_Status);
      Last := I + Length - 1;
      if Last > Result'Last then
         Last := Result'Last;
         Length := Result'Last + 1 - I;
         Status := Overflow;
      else
         Status := Success;
      end if;
      Code_2 := Code;
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
      Status : out From_Status_Type)
   is
      I : Natural := Data'First;
      First : constant Character := Data (I);
      Trail : Character;
      Length : Natural;
      Shortest_Leading : Character;
      Shortest_Length : Natural;
      Code : UCS_4;
   begin
      Status := Success;
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
            Result := Character'Pos (First) + (UCS_4'Last - 16#ff#); -- dummy
            Status := Illegal_Sequence; -- trailing byte or invalid code
            return;
      end case;
      --  trailing bytes
      for J in 2 .. Length loop
         if I >= Data'Last then
            Code := Code * 2 ** (6 * (Length - J + 1));
            Status := Truncated; -- trailing byte is nothing
            exit;
         else
            I := I + 1;
            Trail := Data (I);
            if Trail not in
               Character'Val (2#10000000#) .. Character'Val (2#10111111#)
            then
               I := I - 1;
               Code := Code * 2 ** (6 * (Length - J + 1));
               Status := Illegal_Sequence; -- trailing byte is invalid
               exit;
            end if;
         end if;
         Code := Code * (2 ** 6) or
            (Character'Pos (Trail) and (2 ** 6 - 1));
      end loop;
      if Status = Success then
         UTF_8_Length (
            Code,
            Shortest_Leading,
            Shortest_Length,
            Status); -- set Illegal_Sequence if surrogate pair
         if Shortest_Length /= Length then
            Status := Illegal_Sequence; -- too long
         end if;
      end if;
      Last := I;
      Result := Code;
   end From_UTF_8;

   procedure From_UTF_8_Reverse (
      Data : String;
      First : out Positive;
      Result : out UCS_4;
      Status : out From_Status_Type)
   is
      Last : Natural;
   begin
      First := Data'Last;
      while Data (First) in
         Character'Val (2#10000000#) .. Character'Val (2#10111111#)
      loop
         if First = Data'First then
            First := Data'Last; -- take 1 element
            Result := Character'Pos (Data (First)) + (UCS_4'Last - 16#ff#);
            Status := Truncated;
            return;
         elsif Data'Last - First + 1 = 6 then
            First := Data'Last; -- take 1 element
            Result := Character'Pos (Data (First)) + (UCS_4'Last - 16#ff#);
            Status := Illegal_Sequence;
            return;
         end if;
         First := First - 1;
      end loop;
      From_UTF_8 (Data (First .. Data'Last), Last, Result, Status);
      if Last /= Data'Last then
         First := Data'Last;
         Result := Character'Pos (Data (First)) + (UCS_4'Last - 16#ff#);
         Status := Illegal_Sequence; -- not Truncated
      end if;
   end From_UTF_8_Reverse;

   procedure UTF_8_Sequence (
      Leading : Character;
      Result : out Positive;
      Status : out Sequence_Status_Type) is
   begin
      case Leading is
         when Character'Val (2#00000000#) .. Character'Val (2#01111111#) =>
            Result := 1;
            Status := Success;
         when Character'Val (2#11000000#) .. Character'Val (2#11011111#) =>
            Result := 2;
            Status := Success;
         when Character'Val (2#11100000#) .. Character'Val (2#11101111#) =>
            Result := 3;
            Status := Success;
         when Character'Val (2#11110000#) .. Character'Val (2#11110111#) =>
            Result := 4;
            Status := Success;
         when Character'Val (2#11111000#) .. Character'Val (2#11111011#) =>
            Result := 5;
            Status := Success;
         when Character'Val (2#11111100#) .. Character'Val (2#11111101#) =>
            Result := 6;
            Status := Success;
         when others =>
            Result := 1;
            Status := Illegal_Sequence; -- trailing byte or invalid code
      end case;
   end UTF_8_Sequence;

   procedure To_UTF_16 (
      Code : UCS_4;
      Result : out Wide_String;
      Last : out Natural;
      Status : out To_Status_Type) is
   begin
      case Code is
         when 16#0000# .. 16#d7ff#
            | 16#d800# .. 16#dfff# -- without checking surrogate pair in To_XXX
            | 16#e000# .. 16#ffff# =>
            Last := Result'First;
            if Last <= Result'Last then
               Result (Last) := Wide_Character'Val (Code);
               Status := Success;
            else
               Last := Result'Last;
               Status := Overflow;
            end if;
         when 16#00010000# .. UCS_4'Last =>
            Last := Result'First;
            if Last <= Result'Last then
               declare
                  Code_2 : constant UCS_4 := Code - 16#00010000#;
               begin
                  Result (Last) := Wide_Character'Val (
                     16#d800# or ((Code_2 / (2 ** 10)) and (2 ** 10 - 1)));
                  Last := Last + 1;
                  if Last <= Result'Last then
                     Result (Last) := Wide_Character'Val (
                        16#dc00# or (Code_2 and (2 ** 10 - 1)));
                     if Code_2 >= 2 ** 20 then -- over range of UTF-16
                        Status := Unmappable;
                     else
                        Status := Success;
                     end if;
                  else
                     Last := Result'Last;
                     Status := Overflow;
                  end if;
               end;
            else
               Last := Result'Last;
               Status := Overflow;
            end if;
      end case;
   end To_UTF_16;

   procedure From_UTF_16 (
      Data : Wide_String;
      Last : out Natural;
      Result : out UCS_4;
      Status : out From_Status_Type)
   is
      I : Natural := Data'First;
      First : constant Wide_Character := Data (I);
   begin
      case First is
         when Wide_Character'Val (16#0000#) .. Wide_Character'Val (16#d7ff#)
            | Wide_Character'Val (16#e000#) .. Wide_Character'Val (16#ffff#) =>
            Last := I;
            Result := Wide_Character'Pos (First);
            Status := Success;
         when Wide_Character'Val (16#d800#) .. Wide_Character'Val (16#dbff#) =>
            declare
               Second : Wide_Character;
            begin
               if I >= Data'Last then
                  Status := Truncated; -- trailing byte is nothing
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
                     Status := Illegal_Sequence; -- trailing byte is invalid
                  else
                     Status := Success;
                  end if;
               end if;
               Last := I;
               declare
                  High : constant UCS_4 :=
                     Wide_Character'Pos (First) and (2 ** 10 - 1);
                  Low : constant UCS_4 :=
                     Wide_Character'Pos (Second) and (2 ** 10 - 1);
               begin
                  Result := ((High * (2 ** 10)) or Low) + 16#00010000#;
               end;
            end;
         when Wide_Character'Val (16#dc00#) .. Wide_Character'Val (16#dfff#) =>
            Last := I;
            Result := Wide_Character'Pos (First);
            Status := Illegal_Sequence; -- trailing byte
      end case;
   end From_UTF_16;

   procedure From_UTF_16_Reverse (
      Data : Wide_String;
      First : out Positive;
      Result : out UCS_4;
      Status : out From_Status_Type)
   is
      Last : Natural; -- ignore
   begin
      if Data (Data'Last) in
         Wide_Character'Val (16#dc00#) .. Wide_Character'Val (16#dfff#)
      then
         if Data'First < Data'Last
            and then Data (Data'Last - 1) in
               Wide_Character'Val (16#d800#) .. Wide_Character'Val (16#dbff#)
         then
            First := Data'Last - 1;
         else
            First := Data'Last;
            Result := Wide_Character'Pos (Data (First));
            Status := Truncated;
            return;
         end if;
      else
         First := Data'Last;
      end if;
      From_UTF_16 (Data (First .. Data'Last), Last, Result, Status);
   end From_UTF_16_Reverse;

   procedure UTF_16_Sequence (
      Leading : Wide_Character;
      Result : out Positive;
      Status : out Sequence_Status_Type) is
   begin
      case Leading is
         when Wide_Character'Val (16#0000#) .. Wide_Character'Val (16#d7ff#)
            | Wide_Character'Val (16#e000#) .. Wide_Character'Val (16#ffff#) =>
            Result := 1;
            Status := Success;
         when Wide_Character'Val (16#d800#) .. Wide_Character'Val (16#dbff#) =>
            Result := 2;
            Status := Success;
         when Wide_Character'Val (16#dc00#) .. Wide_Character'Val (16#dfff#) =>
            Result := 1;
            Status := Illegal_Sequence; -- trailing byte
      end case;
   end UTF_16_Sequence;

   procedure To_UTF_32 (
      Code : UCS_4;
      Result : out Wide_Wide_String;
      Last : out Natural;
      Status : out To_Status_Type) is
   begin
      Last := Result'First;
      if Last <= Result'Last then
         Result (Last) := Wide_Wide_Character'Val (Code);
         Status := Success;
      else
         Last := Result'Last;
         Status := Overflow;
      end if;
   end To_UTF_32;

   procedure From_UTF_32 (
      Data : Wide_Wide_String;
      Last : out Natural;
      Result : out UCS_4;
      Status : out From_Status_Type)
   is
      type Unsigned_32 is mod 2 ** Wide_Wide_Character'Size;
      Code : constant Unsigned_32 :=
         Wide_Wide_Character'Pos (Data (Data'First));
   begin
      Last := Data'First;
      Result := UCS_4'Mod (Code);
      case Code is
         when 16#d800# .. 16#dfff# | 16#80000000# .. 16#ffffffff# =>
            Status := Illegal_Sequence;
         when others =>
            Status := Success;
      end case;
   end From_UTF_32;

   procedure From_UTF_32_Reverse (
      Data : Wide_Wide_String;
      First : out Positive;
      Result : out UCS_4;
      Status : out From_Status_Type)
   is
      Last : Natural; -- ignored
   begin
      First := Data'Last;
      From_UTF_32 (Data (First .. Data'Last), Last, Result, Status);
   end From_UTF_32_Reverse;

   procedure UTF_32_Sequence (
      Leading : Wide_Wide_Character;
      Result : out Positive;
      Status : out Sequence_Status_Type) is
   begin
      Result := 1;
      case Wide_Wide_Character'Pos (Leading) is
         when 16#d800# .. 16#dfff# | 16#80000000# .. 16#ffffffff# =>
            Status := Illegal_Sequence;
         when others =>
            Status := Success;
      end case;
   end UTF_32_Sequence;

   procedure Convert_Procedure (
      Source : Source_Type;
      Result : out Target_Type;
      Last : out Natural;
      Substitute : Target_Element_Type := Target_Element_Type'Val (16#20#))
   is
      Source_Last : Natural := Source'First - 1;
   begin
      Last := Result'First - 1;
      while Source_Last < Source'Last loop
         declare
            Code : UCS_4;
            From_Status : From_Status_Type;
            To_Status : To_Status_Type; -- ignore
         begin
            From_UTF (
               Source (Source_Last + 1 .. Source'Last),
               Source_Last,
               Code,
               From_Status);
            if From_Status /= Success then
               Last := Last + 1;
               Result (Last) := Substitute;
            else
               To_UTF (
                  Code,
                  Result (Last + 1 .. Result'Last),
                  Last,
                  To_Status);
               --  ignore error
            end if;
         end;
      end loop;
   end Convert_Procedure;

   function Convert_Function (
      Source : Source_Type;
      Substitute : Target_Element_Type := Target_Element_Type'Val (16#20#))
      return Target_Type
   is
      Result : Target_Type (1 .. Source'Length * Expanding);
      Last : Natural;
   begin
      Convert_Procedure (Source, Result, Last, Substitute);
      return Result (1 .. Last);
   end Convert_Function;

end System.UTF_Conversions;
