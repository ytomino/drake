pragma License (Unrestricted);
with Ada.Numerics.MT19937;
generic
   type Result_Subtype is (<>);
package Ada.Numerics.Discrete_Random is

   --  Basic facilities

--  type Generator is limited private;
   type Generator is new MT19937.Generator;

   function Random (Gen : Generator) return Result_Subtype;

--  procedure Reset (Gen : Generator; Initiator : Integer);
--  procedure Reset (Gen : Generator);
   --  procedure Reset is inherited.

   --  Advanced facilities

--  type State is private;
   subtype State is MT19937.State;

--  procedure Save (Gen : Generator; To_State : out State);
--  procedure Reset (Gen : in Generator; From_State : State);
   --  procedure Save and Load are inherited.

   Max_Image_Width : constant := MT19937.Max_Image_Width;

   function Image (Of_State : State) return String
      renames MT19937.Image;
   function Value (Coded_State : String) return State
      renames MT19937.Value;

end Ada.Numerics.Discrete_Random;
