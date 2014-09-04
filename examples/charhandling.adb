with Ada.Characters.Handling;
with Ada.Characters.ASCII.Handling;
procedure charhandling is
	package UH renames Ada.Characters.Handling;
	package AH renames Ada.Characters.ASCII.Handling;
begin
	begin
		if UH.Is_Control (Character'Val (16#80#)) then -- raise Constraint_Error
			raise Program_Error;
		end if;
		raise Program_Error;
	exception
		when Constraint_Error => null;
	end;
	for I in Ada.Characters.Handling.ISO_646 loop
		begin
			pragma Assert (UH.Is_Control (I) = AH.Is_Control (I));
			pragma Assert (UH.Is_Graphic (I) = AH.Is_Graphic (I));
			pragma Assert (UH.Is_Letter (I) = AH.Is_Letter (I));
			pragma Assert (UH.Is_Lower (I) = AH.Is_Lower (I));
			pragma Assert (UH.Is_Upper (I) = AH.Is_Upper (I));
--			pragma Assert (UH.Is_Basic (I) = AH.Is_Basic (I));
			pragma Assert (UH.Is_Digit (I) = AH.Is_Digit (I));
			pragma Assert (UH.Is_Decimal_Digit (I) = AH.Is_Decimal_Digit (I));
			pragma Assert (UH.Is_Hexadecimal_Digit (I) = AH.Is_Hexadecimal_Digit (I));
			pragma Assert (UH.Is_Alphanumeric (I) = AH.Is_Alphanumeric (I));
			pragma Assert (UH.Is_Special (I) = AH.Is_Special (I));
			pragma Assert (UH.To_Lower (I) = AH.To_Lower (I));
			pragma Assert (UH.To_Upper (I) = AH.To_Upper (I));
			pragma Assert (UH.To_Case_Folding (I) = AH.To_Case_Folding (I));
			null;
		exception
			when others =>
				Ada.Debug.Put (I'Img);
				raise;
		end;
	end loop;
	-- illegal sequence
	pragma Assert (UH.To_Upper ("a" & Character'Val (16#E0#) & "b") = "A" & Character'Val (16#E0#) & "B");
	pragma Assert (UH.To_Lower ("A" & Character'Val (16#C0#) & "B") = "a" & Character'Val (16#C0#) & "b");
	pragma Debug (Ada.Debug.Put ("OK"));
end charhandling;
