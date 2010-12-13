with Ada.Strings.Maps;
procedure charset is
	use type Ada.Strings.Maps.Character_Set;
	Set : Ada.Strings.Maps.Character_Set;
begin
	Set := Set or Ada.Strings.Maps.To_Set ('A');
	Set := Set or Ada.Strings.Maps.To_Set ('C');
	Set := Set or Ada.Strings.Maps.To_Set ('B');
	Ada.Debug.Put (Ada.Strings.Maps.To_Sequence (Set));
end charset;
