pragma License (Unrestricted);
--  extended unit
with Ada.Strings.Composites;
package Ada.Strings.Normalization is
   --  This package provides unicode normalization.
   pragma Preelaborate;

   procedure Iterate (
      Expanded : Boolean;
      Process : not null access procedure (
         Precomposed : Wide_Wide_Character;
         Decomposed : Wide_Wide_String));

   Expanding : constant := 4; -- max decomposed length of one code point

   --  NFD (only reversible)

   --  one combining character sequence
   procedure Decompose (
      Item : String;
      Last : out Natural;
      Out_Item : out String;
      Out_Last : out Natural);
   procedure Decompose (
      State : in out Composites.State;
      Item : String;
      Last : out Natural;
      Out_Item : out String;
      Out_Last : out Natural);
   procedure Decompose (
      Item : Wide_String;
      Last : out Natural;
      Out_Item : out Wide_String;
      Out_Last : out Natural);
   procedure Decompose (
      State : in out Composites.State;
      Item : Wide_String;
      Last : out Natural;
      Out_Item : out Wide_String;
      Out_Last : out Natural);
   procedure Decompose (
      Item : Wide_Wide_String;
      Last : out Natural;
      Out_Item : out Wide_Wide_String;
      Out_Last : out Natural);
   procedure Decompose (
      State : in out Composites.State;
      Item : Wide_Wide_String;
      Last : out Natural;
      Out_Item : out Wide_Wide_String;
      Out_Last : out Natural);

   --  all sequences
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

   --  one combining character sequence
   procedure Compose (
      Item : String;
      Last : out Natural;
      Out_Item : out String;
      Out_Last : out Natural);
   procedure Compose (
      State : in out Composites.State;
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
      State : in out Composites.State;
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
      State : in out Composites.State;
      Item : Wide_Wide_String;
      Last : out Natural;
      Out_Item : out Wide_Wide_String;
      Out_Last : out Natural);

   --  all sequences
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

end Ada.Strings.Normalization;
