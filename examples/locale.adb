with Ada.Locales;
procedure locale is
	use type Ada.Locales.ISO_639_Alpha_2;
	use type Ada.Locales.ISO_639_Alpha_3;
begin
	Ada.Debug.Put (String (Ada.Locales.ISO_639_Alpha_2'(Ada.Locales.Language)));
	Ada.Debug.Put (String (Ada.Locales.ISO_639_Alpha_3'(Ada.Locales.Language)));
	Ada.Debug.Put (String (Ada.Locales.Country));
	pragma Assert (Ada.Locales.To_Alpha_2 (Ada.Locales.Language) = Ada.Locales.Language);
	pragma Assert (Ada.Locales.To_Alpha_3 (Ada.Locales.Language) = Ada.Locales.Language);
	pragma Assert (Ada.Locales.To_Alpha_2 (Ada.Locales.ISO_639_Alpha_3_Unknown) = Ada.Locales.ISO_639_Alpha_2_Unknown);
	pragma Assert (Ada.Locales.To_Alpha_3 (Ada.Locales.ISO_639_Alpha_2_Unknown) = Ada.Locales.ISO_639_Alpha_3_Unknown);
	pragma Assert (Ada.Debug.Put ("OK"));
end locale;
