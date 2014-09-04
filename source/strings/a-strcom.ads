pragma License (Unrestricted);
--  extended unit
package Ada.Strings.Composites is
   --  Unicode composite character handling.
   pragma Preelaborate;

   type Class is range 0 .. 255;
   for Class'Size use Standard'Storage_Unit;

   function Combining_Class (Item : Wide_Wide_Character) return Class;

   --  get all combining characters that each combining class of them > 0
   procedure Iterate (
      Process : not null access procedure (
         Item : Wide_Wide_Character;
         Combining_Class : Class));

   function Is_Variation_Selector (Item : Wide_Wide_Character)
      return Boolean;

   --  for sequence
   type State is record
      Next_Character : Wide_Wide_Character;
      Next_Combining_Class : Class;
      Next_Last : Natural;
   end record;

   procedure Start (Item : String; State : out Composites.State);
   procedure Start (Item : Wide_String; State : out Composites.State);
   procedure Start (Item : Wide_Wide_String; State : out Composites.State);

   --  get one combining character sequence
   --    including trailing variation selector
   procedure Get_Combined (
      Item : String;
      Last : out Natural);
   procedure Get_Combined (
      State : in out Composites.State;
      Item : String;
      Last : out Natural);
   procedure Get_Combined (
      Item : Wide_String;
      Last : out Natural);
   procedure Get_Combined (
      State : in out Composites.State;
      Item : Wide_String;
      Last : out Natural);
   procedure Get_Combined (
      Item : Wide_Wide_String;
      Last : out Natural);
   procedure Get_Combined (
      State : in out Composites.State;
      Item : Wide_Wide_String;
      Last : out Natural);

end Ada.Strings.Composites;
