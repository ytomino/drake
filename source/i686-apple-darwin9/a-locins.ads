pragma License (Unrestricted);
--  implementation unit
package Ada.Locales.Inside is
   pragma Preelaborate;

   function Language return ISO_639_Alpha_2;
   function Language return ISO_639_Alpha_3;
   function Country return ISO_3166_1_Alpha_2;

end Ada.Locales.Inside;
