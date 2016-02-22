pragma License (Unrestricted);
--  implementation unit
package Ada.Strings.Naked_Maps.Set_Constants is
   pragma Preelaborate;

   function Decimal_Digit_Set return not null Character_Set_Access;
   function Hexadecimal_Digit_Set return not null Character_Set_Access;

   function ISO_646_Set return not null Character_Set_Access;
   function Character_Set return not null Character_Set_Access
      renames ISO_646_Set; -- 16#00# .. 16#7F#
   function Wide_Character_Set return not null Character_Set_Access;

   function Letter_Set return not null Character_Set_Access;
      --  Ll, Lu, Lt, Lm, Lo

   function Alphanumeric_Set return not null Character_Set_Access;
      --  Letter + Nd, Nl, No

   function Special_Set return not null Character_Set_Access;
      --  Mn, Me, Mc, Zs, Pd, Ps, Pe, Pc, Po, Sm, Sc, Sk, So, Pi, Pf

   function Graphic_Set return not null Character_Set_Access;
      --  Alphanumeric + Special

end Ada.Strings.Naked_Maps.Set_Constants;
