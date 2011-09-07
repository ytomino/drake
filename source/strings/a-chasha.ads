pragma License (Unrestricted);
--  extended unit
package Ada.Characters.ASCII.Handling is
   --  There are functions handling only lower-half characters in
   --    16#00# .. 16#7F#.
   pragma Pure;

   function Is_Control (Item : Character) return Boolean;
   function Is_Graphic (Item : Character) return Boolean;
   function Is_Letter (Item : Character) return Boolean;
   function Is_Lower (Item : Character) return Boolean;
   function Is_Upper (Item : Character) return Boolean;
--  function Is_Basic (Item : Character) return Boolean;
   function Is_Digit (Item : Character) return Boolean;
   function Is_Decimal_Digit (Item : Character) return Boolean
     renames Is_Digit;
   function Is_Hexadecimal_Digit (Item : Character) return Boolean;
   function Is_Alphanumeric (Item : Character) return Boolean;
   function Is_Special (Item : Character) return Boolean;

   function To_Lower (Item : Character) return Character;
   function To_Upper (Item : Character) return Character;
--  function To_Basic (Item : Character) return Character;
   --  extended
   function To_Case_Folding (Item : Character) return Character
      renames To_Lower;

end Ada.Characters.ASCII.Handling;
