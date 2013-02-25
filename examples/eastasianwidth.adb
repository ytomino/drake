with Ada.Characters.East_Asian_Width;
procedure eastasianwidth is
	use Ada.Characters.East_Asian_Width;
	subtype WWC is Wide_Wide_Character;
begin
	pragma Assert (Kind (WWC'Val (0)) = Neutral);
	pragma Assert (Kind ('A') = Narrow);
	pragma Assert (Kind (WWC'Val (16#2500#)) = Ambiguous);
	pragma Assert (Kind (WWC'Val (16#3042#)) = Wide);
	pragma Assert (Kind (WWC'Val (16#FF01#)) = Full_Width);
	pragma Assert (Kind (WWC'Val (16#FF71#)) = Half_Width);
	pragma Assert (not Is_Full_Width (Half_Width, East_Asian => False));
	pragma Assert (not Is_Full_Width (Half_Width, East_Asian => True));
	pragma Assert (not Is_Full_Width (Ambiguous, East_Asian => False));
	pragma Assert (Is_Full_Width (Ambiguous, East_Asian => True));
	pragma Assert (Is_Full_Width (Wide, East_Asian => False));
	pragma Assert (Is_Full_Width (Wide, East_Asian => True));
	pragma Debug (Ada.Debug.Put ("OK"));
	null;
end eastasianwidth;
