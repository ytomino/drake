pragma License (Unrestricted);
--  implementation unit required by compiler
package System.Wid_WChar is
   pragma Pure;

   --  required for Wide_Character'Width by compiler (s-widwch.ads)
   function Width_Wide_Character (Lo, Hi : Wide_Character) return Natural;

   --  required for Wide_Wide_Character'Width by compiler (s-widwch.ads)
   function Width_Wide_Wide_Character (Lo, Hi : Wide_Wide_Character)
      return Natural;

end System.Wid_WChar;
