pragma License (Unrestricted);
package Ada.Strings.Wide_Wide_Maps.Wide_Wide_Constants is
   pragma Preelaborate;

--  Control_Set : constant Wide_Wide_Character_Set;
--  Graphic_Set : constant Wide_Wide_Character_Set;
--  Letter_Set : constant Wide_Wide_Character_Set;
--  Lower_Set : constant Wide_Wide_Character_Set;
--  Upper_Set : constant Wide_Wide_Character_Set;
--  Basic_Set : constant Wide_Wide_Character_Set;
--  Decimal_Digit_Set : constant Wide_Wide_Character_Set;
--  Hexadecimal_Digit_Set : constant Wide_Wide_Character_Set;
--  Alphanumeric_Set : constant Wide_Wide_Character_Set;
--  Special_Set : constant Wide_Wide_Character_Set;
--  ISO_646_Set : constant Wide_Wide_Character_Set;

--  Lower_Case_Map : constant Wide_Wide_Character_Mapping;
   function Lower_Case_Map return Wide_Wide_Character_Mapping;
   pragma Inline (Lower_Case_Map);
   --  Maps to lower case for letters, else identity
--  Upper_Case_Map : constant Wide_Wide_Character_Mapping;
   --  Maps to upper case for letters, else identity
--  Basic_Map : constant Wide_Wide_Character_Mapping;
   --  Maps to basic letter for letters, else identity

   --  RM A.4.8

--  Character_Set : constant Wide_Wide_Maps.Wide_Wide_Character_Set;
   --  Contains each Wide_Wide_Character value WWC such that
   --  Characters.Conversions.Is_Character(WWC) is True
--  Wide_Character_Set : constant Wide_Wide_Maps.Wide_Wide_Character_Set;
   --  Contains each Wide_Wide_Character value WWC such that
   --  Characters.Conversions.Is_Wide_Character(WWC) is True

end Ada.Strings.Wide_Wide_Maps.Wide_Wide_Constants;
