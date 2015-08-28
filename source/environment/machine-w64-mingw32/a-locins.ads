pragma License (Unrestricted);
--  implementation unit specialized for Windows
package Ada.Locales.Inside is
   pragma Preelaborate;

   --  language code defined by ISO 639-1/2

   function Language return ISO_639_Alpha_2;
   function Language return ISO_639_Alpha_3;

   --  country code defined by ISO 3166-1

   function Country return ISO_3166_1_Alpha_2;

end Ada.Locales.Inside;
