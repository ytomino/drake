pragma License (Unrestricted);
--  implementation unit
package Ada.Characters.Inside.Sets.Constants is
   pragma Preelaborate;

   function Decimal_Digit_Set
      return not null access Character_Set;
   function Hexadecimal_Digit_Set
      return not null access Character_Set;

   function ISO_646_Set return not null access Character_Set;
   function Character_Set return not null access Character_Set
      renames ISO_646_Set; -- 16#00# .. 16#7F#
   function Wide_Character_Set
      return not null access Sets.Character_Set;

   function Letter_Set return not null access Sets.Character_Set;
   --  Ll, Lu, Lt, Lm, Lo

   function Alphanumeric_Set return not null access Sets.Character_Set;
   --  Letter + Nd, Nl, No

   function Special_Set return not null access Sets.Character_Set;
   --  Mn, Me, Mc, Zs, Pd, Ps, Pe, Pc, Po, Sm, Sc, Sk, So, Pi, Pf

   function Graphic_Set return not null access Sets.Character_Set;
   --  Alphanumeric + Special

end Ada.Characters.Inside.Sets.Constants;
