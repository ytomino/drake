pragma License (Unrestricted);
--  generic implementation of Ada.Strings.Equal_Case_Insensitive
generic
   type Character_Type is (<>);
   type String_Type is array (Positive range <>) of Character_Type;
   with procedure Get (
      Item : String_Type;
      Last : out Natural;
      Value : out Wide_Wide_Character;
      Is_Illegal_Sequence : out Boolean);
function Ada.Strings.Generic_Equal_Case_Insensitive (Left, Right : String_Type)
   return Boolean;
--  pragma Pure (Ada.Strings.Generic_Equal_Case_Insensitive);
pragma Preelaborate (Ada.Strings.Generic_Equal_Case_Insensitive); -- use maps
