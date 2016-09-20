pragma License (Unrestricted);
--  Ada 2012
package Ada.Locales is
   pragma Preelaborate;
   pragma Remote_Types;

   --  extended from here
   --  These are language code defined by ISO 639-1/2.
   --  Ada RM requires alpha-3 in spite of that
   --    almost all the operating systems use alpha-2.

   type ISO_639_Alpha_2 is new String (1 .. 2);
--    with Dynamic_Predicate => -- [gcc-4.8] rejects
--       (for all E of ISO_639_Alpha_2 => E in 'a' .. 'z');
   type ISO_639_Alpha_3 is new String (1 .. 3);
--    with Dynamic_Predicate => -- [gcc-4.8] rejects
--       (for all E of ISO_639_Alpha_3 => E in 'a' .. 'z');

   ISO_639_Alpha_2_Unknown : constant ISO_639_Alpha_2 := "un"; -- ???
   ISO_639_Alpha_3_Unknown : constant ISO_639_Alpha_3 := "und";

   function To_Alpha_2 (Item : ISO_639_Alpha_3) return ISO_639_Alpha_2;
   function To_Alpha_3 (Item : ISO_639_Alpha_2) return ISO_639_Alpha_3;

   function Language return ISO_639_Alpha_2;
   function Language return ISO_639_Alpha_3; -- Language_Code
   pragma Inline (Language); -- renamed

   --  extended from here
   --  These are country code defined by ISO 3166-1.

   type ISO_3166_1_Alpha_2 is new String (1 .. 2);
--    with Dynamic_Predicate => -- [gcc-4.8] rejects
--       (for all E of ISO_3166_1_Alpha_2 => E in 'A' .. 'Z');

   ISO_3166_1_Alpha_2_Unknown : constant ISO_3166_1_Alpha_2 := "ZZ";

   function Country return ISO_3166_1_Alpha_2; -- Country_Code
   pragma Inline (Country); -- renamed

   --  modified
--  type Language_Code is new String (1 .. 3)
--    with Dynamic_Predicate =>
--       (for all E of Language_Code => E in 'a' .. 'z'); -- AI12-0037-1
   subtype Language_Code is ISO_639_Alpha_3;

   --  modified
--  type Country_Code is new String (1 .. 2)
--    with Dynamic_Predicate =>
--       (for all E of Country_Code => E in 'A' .. 'Z'); -- AI12-0037-1
   subtype Country_Code is ISO_3166_1_Alpha_2;

--  Language_Unknown : constant Language_Code := "und";
   Language_Unknown : Language_Code
      renames ISO_639_Alpha_3_Unknown;
--  Country_Unknown : constant Country_Code := "ZZ";
   Country_Unknown : Country_Code
      renames ISO_3166_1_Alpha_2_Unknown;

--  function Language return Language_Code;
--  function Country return Country_Code;

end Ada.Locales;
