pragma License (Unrestricted);
--  extended unit
package Ada.Characters.East_Asian_Width is
   --  Unicode property "east asian width".
   pragma Pure;

   --  same order of Ada.UCD.East_Asian_Width_Type
   type Width_Kind is (
      Neutral,
      Narrow,
      Half_Width,
      Ambiguous,
      Wide,
      Full_Width);

   function Kind (C : Wide_Wide_Character) return Width_Kind;

   function Is_Full_Width (W : Width_Kind; East_Asian : Boolean)
      return Boolean;
   pragma Inline (Is_Full_Width);

end Ada.Characters.East_Asian_Width;
