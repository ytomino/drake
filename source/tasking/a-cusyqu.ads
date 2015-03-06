pragma License (Unrestricted);
--  Ada 2012
with Ada.Containers.Synchronized_Queue_Interfaces;
with System;
generic
   with package Queue_Interfaces is new Synchronized_Queue_Interfaces (<>);
   Default_Ceiling : System.Any_Priority := System.Priority'Last;
package Ada.Containers.Unbounded_Synchronized_Queues is
   pragma Preelaborate;

   package Implementation is
      --  not specified by the language

      type Node;
      type Node_Access is access Node;
      type Node is record
         Element : Queue_Interfaces.Element_Type;
         Next : Node_Access;
      end record;

   end Implementation;

   protected type Queue (Ceiling : System.Any_Priority := Default_Ceiling)
      with Priority => Ceiling is
      new Queue_Interfaces.Queue
   with

      overriding entry Enqueue (New_Item : Queue_Interfaces.Element_Type);
      overriding entry Dequeue (Element : out Queue_Interfaces.Element_Type);

      overriding function Current_Use return Count_Type;
      overriding function Peak_Use return Count_Type;

   private

      First : Implementation.Node_Access := null;
      Last : Implementation.Node_Access := null;
      Current_Length : Count_Type := 0;
      Peak_Length : Count_Type := 0;

   end Queue;

private
end Ada.Containers.Unbounded_Synchronized_Queues;
