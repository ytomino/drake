pragma License (Unrestricted);
with Ada.Numerics.MT19937;
package Ada.Numerics.Float_Random is

   --  Basic facilities

--  type Generator is limited private;
   subtype Generator is MT19937.Generator;

   subtype Uniformly_Distributed is Float range 0.0 .. 1.0;
   function Random (Gen : Generator) return Uniformly_Distributed;

--  procedure Reset (Gen : Generator; Initiator : Integer);
   procedure Reset (Gen : in out Generator; Initiator : Integer);
--  procedure Reset (Gen : Generator);
   procedure Reset (Gen : in out Generator)
      renames MT19937.Reset;

   --  Advanced facilities

--  type State is private;
   subtype State is MT19937.State;

   procedure Save (Gen : Generator; To_State : out State)
      renames MT19937.Save;
--  procedure Reset (Gen : Generator; From_State : State);
   procedure Reset (Gen : in out Generator; From_State : State)
      renames MT19937.Reset;

   Max_Image_Width : constant := MT19937.Max_Image_Width;

   function Image (Of_State : State) return String
      renames MT19937.Image;
   function Value (Coded_State : String) return State
      renames MT19937.Value;

end Ada.Numerics.Float_Random;
