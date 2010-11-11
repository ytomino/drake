pragma License (Unrestricted);
with Ada.Numerics.MT19937;
generic
   type Result_Subtype is (<>);
package Ada.Numerics.Discrete_Random is

   --  Basic facilities

--  type Generator is limited private;
   subtype Generator is MT19937.Generator; --  extended

   function Random (Gen : Generator) return Result_Subtype;

--  procedure Reset (Gen : Generator; Initiator : Integer);
--  procedure Reset (Gen : in Generator);

   --  extended
   procedure Reset (Gen : in out Generator; Initiator : Integer);
   procedure Reset (Gen : in out Generator)
      renames MT19937.Reset;

   --  Advanced facilities

--  type State is private;
   subtype State is MT19937.State;

   procedure Save (Gen : Generator; To_State : out State)
      renames MT19937.Save;
--  procedure Reset (Gen : in Generator; From_State : State);

   --  extended
   procedure Reset (Gen : in out Generator; From_State : State)
      renames MT19937.Reset;

   Max_Image_Width : constant := MT19937.Max_Image_Width;

   function Image (Of_State : State) return String
      renames MT19937.Image;
   function Value (Coded_State : String) return State
      renames MT19937.Value;

end Ada.Numerics.Discrete_Random;
