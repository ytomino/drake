pragma License (Unrestricted);
--  extended unit
package Ada.Characters.Normalization is
   --  This package provides unicode normalization.
   pragma Preelaborate;

   type Class is range 0 .. 255;
   for Class'Size use Standard'Storage_Unit;

   function Combining_Class (Item : Wide_Wide_Character) return Class;

   function Is_Variation_Selector (Item : Wide_Wide_Character)
      return Boolean;

   --  for sequence
   type State is record
      Next_Character : Wide_Wide_Character;
      Next_Combining_Class : Class;
      Next_Last : Natural;
   end record;

   procedure Start (Item : String; State : out Normalization.State);
   procedure Start (Item : Wide_String; State : out Normalization.State);
   procedure Start (Item : Wide_Wide_String; State : out Normalization.State);

   --  get single letter with trailing variation selector
   procedure Get_Combined (
      Item : String;
      Last : out Natural);
   procedure Get_Combined (
      State : in out Normalization.State;
      Item : String;
      Last : out Natural);
   procedure Get_Combined (
      Item : Wide_String;
      Last : out Natural);
   procedure Get_Combined (
      State : in out Normalization.State;
      Item : Wide_String;
      Last : out Natural);
   procedure Get_Combined (
      Item : Wide_Wide_String;
      Last : out Natural);
   procedure Get_Combined (
      State : in out Normalization.State;
      Item : Wide_Wide_String;
      Last : out Natural);

   Expanding : constant := 4; -- max decomposed length of single code point

   --  NFD (only reversible)

   --  single letter
   procedure Decompose (
      Item : String;
      Last : out Natural;
      Out_Item : out String;
      Out_Last : out Natural); -- UTF-8
   procedure Decompose (
      State : in out Normalization.State;
      Item : String;
      Last : out Natural;
      Out_Item : out String;
      Out_Last : out Natural); -- UTF-8 with state
   procedure Decompose (
      Item : Wide_String;
      Last : out Natural;
      Out_Item : out Wide_String;
      Out_Last : out Natural); -- UTF-16
   procedure Decompose (
      State : in out Normalization.State;
      Item : Wide_String;
      Last : out Natural;
      Out_Item : out Wide_String;
      Out_Last : out Natural); -- UTF-16 with state
   procedure Decompose (
      Item : Wide_Wide_String;
      Last : out Natural;
      Out_Item : out Wide_Wide_String;
      Out_Last : out Natural); -- UTF-32
   procedure Decompose (
      State : in out Normalization.State;
      Item : Wide_Wide_String;
      Last : out Natural;
      Out_Item : out Wide_Wide_String;
      Out_Last : out Natural); -- UTF-32 with state

   --  all letters
   procedure Decompose (
      Item : String;
      Out_Item : out String;
      Out_Last : out Natural);
   function Decompose (
      Item : String)
      return String;
   procedure Decompose (
      Item : Wide_String;
      Out_Item : out Wide_String;
      Out_Last : out Natural);
   function Decompose (
      Item : Wide_String)
      return Wide_String;
   procedure Decompose (
      Item : Wide_Wide_String;
      Out_Item : out Wide_Wide_String;
      Out_Last : out Natural);
   function Decompose (
      Item : Wide_Wide_String)
      return Wide_Wide_String;

   --  NFC (only reversible)

   --  single letter
   procedure Compose (
      Item : String;
      Last : out Natural;
      Out_Item : out String;
      Out_Last : out Natural);
   procedure Compose (
      State : in out Normalization.State;
      Item : String;
      Last : out Natural;
      Out_Item : out String;
      Out_Last : out Natural);
   procedure Compose (
      Item : Wide_String;
      Last : out Natural;
      Out_Item : out Wide_String;
      Out_Last : out Natural);
   procedure Compose (
      State : in out Normalization.State;
      Item : Wide_String;
      Last : out Natural;
      Out_Item : out Wide_String;
      Out_Last : out Natural);
   procedure Compose (
      Item : Wide_Wide_String;
      Last : out Natural;
      Out_Item : out Wide_Wide_String;
      Out_Last : out Natural);
   procedure Compose (
      State : in out Normalization.State;
      Item : Wide_Wide_String;
      Last : out Natural;
      Out_Item : out Wide_Wide_String;
      Out_Last : out Natural);

   --  all letters
   procedure Compose (
      Item : String;
      Out_Item : out String;
      Out_Last : out Natural);
   function Compose (
      Item : String)
      return String;
   procedure Compose (
      Item : Wide_String;
      Out_Item : out Wide_String;
      Out_Last : out Natural);
   function Compose (
      Item : Wide_String)
      return Wide_String;
   procedure Compose (
      Item : Wide_Wide_String;
      Out_Item : out Wide_Wide_String;
      Out_Last : out Natural);
   function Compose (
      Item : Wide_Wide_String)
      return Wide_Wide_String;

   --  comparison (use NFD)

   function Equal (
      Left, Right : String)
      return Boolean;
   function Equal (
      Left, Right : String;
      Equal_Combined : not null access function (
         Left, Right : Wide_Wide_String)
         return Boolean)
      return Boolean;
   function Equal (
      Left, Right : Wide_String)
      return Boolean;
   function Equal (
      Left, Right : Wide_String;
      Equal_Combined : not null access function (
         Left, Right : Wide_Wide_String)
         return Boolean)
      return Boolean;
   function Equal (
      Left, Right : Wide_Wide_String)
      return Boolean;
   function Equal (
      Left, Right : Wide_Wide_String;
      Equal_Combined : not null access function (
         Left, Right : Wide_Wide_String)
         return Boolean)
      return Boolean;

   function Less (
      Left, Right : String)
      return Boolean;
   function Less (
      Left, Right : String;
      Less_Combined : not null access function (
         Left, Right : Wide_Wide_String)
         return Boolean)
      return Boolean;
   function Less (
      Left, Right : Wide_String)
      return Boolean;
   function Less (
      Left, Right : Wide_String;
      Less_Combined : not null access function (
         Left, Right : Wide_Wide_String)
         return Boolean)
      return Boolean;
   function Less (
      Left, Right : Wide_Wide_String)
      return Boolean;
   function Less (
      Left, Right : Wide_Wide_String;
      Less_Combined : not null access function (
         Left, Right : Wide_Wide_String)
         return Boolean)
      return Boolean;

   --  note 1: this package does not execute unreversible normalization on NFS+
   --  note 2: sorting by combining class is unimplemented
   --  note 3: Decompose is faster than Compose on current implementation

end Ada.Characters.Normalization;
