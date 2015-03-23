package body Ada.Containers.Unbounded_Synchronized_Queues is
   use type Implementation.Node_Access;

   protected body Queue is

--    overriding
      entry Enqueue (New_Item : Queue_Interfaces.Element_Type) when True is
         New_Node : constant Implementation.Node_Access :=
            new Implementation.Node'(
               Element => New_Item,
               Next => null);
      begin
         if First = null then
            First := New_Node;
         else
            Last.Next := New_Node;
         end if;
         Last := New_Node;
         Current_Length := Current_Length + 1;
         if Current_Length > Peak_Length then
            Peak_Length := Current_Length;
         end if;
      end Enqueue;

--    overriding
      entry Dequeue (Element : out Queue_Interfaces.Element_Type)
         when First /= null is
      begin
         Element := First.Element;
         First := First.Next;
         if First = null then
            Last := null;
         end if;
         Current_Length := Current_Length - 1;
      end Dequeue;

      function Current_Use return Count_Type is
      begin
         return Current_Length;
      end Current_Use;

      function Peak_Use return Count_Type is
      begin
         return Peak_Length;
      end Peak_Use;

   end Queue;

end Ada.Containers.Unbounded_Synchronized_Queues;
