pragma License (Unrestricted);
--  Ada 2012
with System;
with Ada.Containers.Synchronized_Queue_Interfaces;
generic
   with package Queue_Interfaces is new Synchronized_Queue_Interfaces (<>);
   type Queue_Priority is private;
   with function Get_Priority (Element : Queue_Interfaces.Element_Type)
      return Queue_Priority is <>;
   with function Before (Left, Right : Queue_Priority) return Boolean is <>;
   Default_Ceiling : System.Any_Priority := System.Priority'Last;
package Ada.Containers.Unbounded_Priority_Queues is
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

      procedure Dequeue_Only_High_Priority (
         At_Least : Queue_Priority;
         Element : in out Queue_Interfaces.Element_Type;
         Success : out Boolean);

      overriding function Current_Use return Count_Type;
      overriding function Peak_Use return Count_Type;

   private

      First : Implementation.Node_Access := null;
      Last : Implementation.Node_Access := null;
      Current_Length : Count_Type := 0;
      Peak_Length : Count_Type := 0;

   end Queue;

private
end Ada.Containers.Unbounded_Priority_Queues;
