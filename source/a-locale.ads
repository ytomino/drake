pragma License (Unrestricted);
--  Ada 2012
package Ada.Locales is
   pragma Preelaborate;
   pragma Remote_Types;

   --  extended
   --  These are for ISO 639-1.
   --  Ada RM requires alpha-3 in spite of that
   --    almost all the operating systems use alpha-2.
   type ISO_639_Alpha_2 is array (1 .. 2) of Character range 'a' .. 'z';
   type ISO_639_Alpha_3 is array (1 .. 3) of Character range 'a' .. 'z';
   ISO_639_Alpha_2_Unknown : constant ISO_639_Alpha_2 := "un"; -- ???
   ISO_639_Alpha_3_Unknown : constant ISO_639_Alpha_3 := "und";
   function To_Alpha_2 (Item : ISO_639_Alpha_3) return ISO_639_Alpha_2;
   function To_Alpha_3 (Item : ISO_639_Alpha_2) return ISO_639_Alpha_3;
   function Language return ISO_639_Alpha_2;
   function Language return ISO_639_Alpha_3;

   --  extended
   --  These are for ISO 3166.
   type ISO_3166_1_Alpha_2 is array (1 .. 2) of Character range 'A' .. 'Z';
   ISO_3166_1_Alpha_2_Unknown : constant ISO_3166_1_Alpha_2 := "ZZ";
   function Country return ISO_3166_1_Alpha_2;

   --  extended
   --  These functions are for ease to put.
   subtype String_2 is String (1 .. 2);
   subtype String_3 is String (1 .. 3);
   function To_String (Item : ISO_639_Alpha_2) return String_2;
   function To_String (Item : ISO_639_Alpha_3) return String_3;
   function To_String (Item : ISO_3166_1_Alpha_2) return String_2;

--  type Language_Code is array (1 .. 3) of Character range 'a' .. 'z';
   subtype Language_Code is ISO_639_Alpha_3;
--  type Country_Code is array (1 .. 2) of Character range 'A' .. 'Z';
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
