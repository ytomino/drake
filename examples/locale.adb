with Ada.Locales;
procedure locale is
begin
	Ada.Debug.Put (Ada.Locales.To_String (Ada.Locales.ISO_639_Alpha_2'(Ada.Locales.Language)));
	Ada.Debug.Put (Ada.Locales.To_String (Ada.Locales.ISO_639_Alpha_3'(Ada.Locales.Language)));
	Ada.Debug.Put (Ada.Locales.To_String (Ada.Locales.Country));
end locale;
