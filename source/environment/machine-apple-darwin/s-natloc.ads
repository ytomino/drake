pragma License (Unrestricted);
--  implementation unit specialized for POSIX (Darwin, FreeBSD, or Linux)
with Ada.Locales;
package System.Native_Locales is
   pragma Preelaborate;

   subtype ISO_639_Alpha_2 is Ada.Locales.ISO_639_Alpha_2;
   subtype ISO_639_Alpha_3 is Ada.Locales.ISO_639_Alpha_3;
   subtype ISO_3166_1_Alpha_2 is Ada.Locales.ISO_3166_1_Alpha_2;

   --  language code defined by ISO 639-1/2

   function Language return ISO_639_Alpha_2;
   function Language return ISO_639_Alpha_3;

   --  country code defined by ISO 3166-1

   function Country return ISO_3166_1_Alpha_2;

end System.Native_Locales;
