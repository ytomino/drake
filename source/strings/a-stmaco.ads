pragma License (Unrestricted);
package Ada.Strings.Maps.Constants is
--  pragma Pure;
   pragma Preelaborate; --  Wide_Wide_Maps is "preelaborate"

--  Control_Set : constant Character_Set;
--  Graphic_Set : constant Character_Set;
--  Letter_Set : constant Character_Set;
--  Lower_Set : constant Character_Set;
--  Upper_Set : constant Character_Set;
--  Basic_Set : constant Character_Set;
--  Decimal_Digit_Set : constant Character_Set;
--  Hexadecimal_Digit_Set : constant Character_Set;
--  Alphanumeric_Set : constant Character_Set;
--  Special_Set : constant Character_Set;
--  ISO_646_Set : constant Character_Set;

--  Lower_Case_Map : constant Character_Mapping;
   function Lower_Case_Map return Character_Mapping;
   pragma Inline (Lower_Case_Map);
   --  Maps to lower case for letters, else identity
--  Upper_Case_Map : constant Character_Mapping;
   function Upper_Case_Map return Character_Mapping;
   pragma Inline (Upper_Case_Map);
   --  Maps to upper case for letters, else identity
--  Basic_Map : constant Character_Mapping;
   --  Maps to basic letter for letters, else identity

end Ada.Strings.Maps.Constants;
