pragma License (Unrestricted);
--  extended unit
package Ada.Numerics.Distributions is
   --  Some kinds of distributions of random numers.
   pragma Pure;

   --  Simple distributions

   generic
      type Source is mod <>;
      type Target is (<>);
   function Linear_Discrete (X : Source) return Target;

   --  [0,1]
   generic
      type Source is mod <>;
      type Target is digits <>;
   function Linear_Float_0_To_1 (X : Source) return Target'Base;

   --  [0,1)
   generic
      type Source is mod <>;
      type Target is digits <>;
   function Linear_Float_0_To_Less_Than_1 (X : Source) return Target'Base;

   --  (0,1)
   generic
      type Source is mod <>;
      type Target is digits <>;
   function Linear_Float_Greater_Than_0_To_Less_Than_1 (X : Source)
      return Target'Base;

   --  -log(0,1] = [0,inf), RM A.5.2(53/2)
   generic
      type Source is mod <>;
      type Target is digits <>;
   function Exponentially_Float (X : Source) return Target'Base;

   --  Simple distributions for random number

   generic
      type Source is mod <>;
      type Target is (<>);
      type Generator (<>) is limited private;
      with function Get (Gen : aliased in out Generator) return Source;
   function Linear_Discrete_Random (Gen : aliased in out Generator)
      return Target;

   --  Strict uniform distributions for random number

   generic
      type Source is mod <>;
      type Target is (<>);
      type Generator (<>) is limited private;
      with function Get (Gen : aliased in out Generator) return Source;
   function Uniform_Discrete_Random (Gen : aliased in out Generator)
      return Target;

   --  [0,1]
   generic
      type Source is mod <>;
      type Target is digits <>;
      type Generator (<>) is limited private;
      with function Get (Gen : aliased in out Generator) return Source;
   function Uniform_Float_Random_0_To_1 (Gen : aliased in out Generator)
      return Target;

   --  [0,1)
   generic
      type Source is mod <>;
      type Target is digits <>;
      type Generator (<>) is limited private;
      with function Get (Gen : aliased in out Generator) return Source;
   function Uniform_Float_Random_0_To_Less_Than_1 (
      Gen : aliased in out Generator)
      return Target;

   --  (0,1)
   generic
      type Source is mod <>;
      type Target is digits <>;
      type Generator (<>) is limited private;
      with function Get (Gen : aliased in out Generator) return Source;
   function Uniform_Float_Random_Greater_Than_0_To_Less_Than_1 (
      Gen : aliased in out Generator)
      return Target;

end Ada.Numerics.Distributions;
