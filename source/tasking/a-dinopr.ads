pragma License (Unrestricted);
--  Ada 2012
package Ada.Dispatching.Non_Preemptive is
   pragma Preelaborate;

--  procedure Yield_To_Higher;
   procedure Yield_To_Same_Or_Higher
      renames Yield;

end Ada.Dispatching.Non_Preemptive;
