with Ada.Integer_Text_IO;
with Ada.Wide_Wide_Text_IO;
-- all Maps and Constants
with Ada.Strings.Maps.Constants;
with Ada.Strings.Wide_Maps.Wide_Constants;
with Ada.Strings.Wide_Wide_Maps.Wide_Wide_Constants;
procedure charset is
	use type Ada.Strings.Maps.Character_Set;
	Set : Ada.Strings.Maps.Character_Set;
begin
	Set := Ada.Strings.Maps.To_Set (String'("ABC"));
	pragma Assert (Ada.Strings.Maps.To_Sequence (Set) = String'("ABC"));
	Set := Ada.Strings.Maps.Null_Set;
	Set := Set or Ada.Strings.Maps.To_Set (Character'('A'));
	Set := Set or Ada.Strings.Maps.To_Set (Character'('C'));
	Set := Set or Ada.Strings.Maps.To_Set (Character'('B'));
	pragma Assert (Ada.Strings.Maps.To_Sequence (Set) = String'("ABC"));
	pragma Assert (Ada.Strings.Maps.To_Sequence (Ada.Strings.Maps.Constants.Decimal_Digit_Set) = String'("0123456789"));
	pragma Assert (not Ada.Strings.Maps.Is_In (Character'('/'), Ada.Strings.Maps.Constants.Decimal_Digit_Set));
	pragma Assert (Ada.Strings.Maps.Is_In (Character'('5'), Ada.Strings.Maps.Constants.Decimal_Digit_Set));
	pragma Assert (not Ada.Strings.Maps.Is_In (Character'(':'), Ada.Strings.Maps.Constants.Decimal_Digit_Set));
	Ada.Wide_Wide_Text_IO.Put_Line (Ada.Strings.Wide_Wide_Maps.To_Sequence (Ada.Strings.Wide_Wide_Maps.Wide_Wide_Constants.Decimal_Number_Set));
	pragma Assert (Ada.Strings.Wide_Wide_Maps.Is_In ('5', Ada.Strings.Wide_Wide_Maps.Wide_Wide_Constants.Decimal_Number_Set));
	for I in Wide_Wide_Character'First .. Wide_Wide_Character'Val (16#110000#) loop
		declare
			C : Natural := 0;
		begin
			if Ada.Strings.Wide_Wide_Maps.Is_In (I, Ada.Strings.Wide_Wide_Maps.Wide_Wide_Constants.Unassigned_Set) then
				C := C + 1;
			end if;
			if Ada.Strings.Wide_Wide_Maps.Is_In (I, Ada.Strings.Wide_Wide_Maps.Wide_Wide_Constants.Graphic_Set) then
				C := C + 1;
				declare
					G : Natural := 0;
				begin
					if Ada.Strings.Wide_Wide_Maps.Is_In (I, Ada.Strings.Wide_Wide_Maps.Wide_Wide_Constants.Alphanumeric_Set) then
						G := G + 1;
						-- Letter, Decimal_Number, Letter_Number or Other_Number
					end if;
					if Ada.Strings.Wide_Wide_Maps.Is_In (I, Ada.Strings.Wide_Wide_Maps.Wide_Wide_Constants.Special_Set) then
						G := G + 1;
					end if;
					if G /= 1 then
						Ada.Integer_Text_IO.Put (Wide_Wide_Character'Pos (I), Base => 16);
						Ada.Wide_Wide_Text_IO.New_Line;
						pragma Assert (False);
					end if;
				end;
			end if;
			if Ada.Strings.Wide_Wide_Maps.Is_In (I, Ada.Strings.Wide_Wide_Maps.Wide_Wide_Constants.Line_Separator_Set) then
				C := C + 1;
			end if;
			if Ada.Strings.Wide_Wide_Maps.Is_In (I, Ada.Strings.Wide_Wide_Maps.Wide_Wide_Constants.Paragraph_Separator_Set) then
				C := C + 1;
			end if;
			if Ada.Strings.Wide_Wide_Maps.Is_In (I, Ada.Strings.Wide_Wide_Maps.Wide_Wide_Constants.Control_Set) then
				C := C + 1;
			end if;
			if Ada.Strings.Wide_Wide_Maps.Is_In (I, Ada.Strings.Wide_Wide_Maps.Wide_Wide_Constants.Format_Set) then
				C := C + 1;
			end if;
			if Ada.Strings.Wide_Wide_Maps.Is_In (I, Ada.Strings.Wide_Wide_Maps.Wide_Wide_Constants.Private_Use_Set) then
				C := C + 1;
			end if;
			if Ada.Strings.Wide_Wide_Maps.Is_In (I, Ada.Strings.Wide_Wide_Maps.Wide_Wide_Constants.Surrogate_Set) then
				C := C + 1;
			end if;
			if C /= 1 then
				Ada.Integer_Text_IO.Put (Wide_Wide_Character'Pos (I), Base => 16);
				Ada.Wide_Wide_Text_IO.New_Line;
				pragma Assert (False);
			end if;
		end;
	end loop;
	pragma Debug (Ada.Debug.Put ("OK"));
end charset;
