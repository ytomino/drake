pragma License (Unrestricted);
--  Ada 2012
package System.Multiprocessors is
   pragma Preelaborate;

   type CPU_Range is range 0 .. Integer'Last;
   Not_A_Specific_CPU : constant CPU_Range := 0;
   subtype CPU is CPU_Range range 1 .. CPU_Range'Last;

   function Number_Of_CPUs return CPU;

end System.Multiprocessors;
