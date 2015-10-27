package body Ada.Containers.Unbounded_Priority_Queues is
   use type Implementation.Node_Access;

   protected body Queue is

      procedure Do_Dequeue (Element : out Queue_Interfaces.Element_Type);
      procedure Do_Dequeue (Element : out Queue_Interfaces.Element_Type) is
      begin
         Element := First.Element;
         First := First.Next;
         if First = null then
            Last := null;
         end if;
         Current_Length := Current_Length - 1;
      end Do_Dequeue;

      --  implementation

--    overriding
      entry Enqueue (New_Item : Queue_Interfaces.Element_Type) when True is
         New_Node : constant Implementation.Node_Access :=
            new Implementation.Node'(
               Element => New_Item,
               Next => null);
      begin
         if First = null then
            First := New_Node;
            Last := New_Node;
         else
            declare
               The_Priority : constant Queue_Priority :=
                  Get_Priority (New_Node.Element);
               Previous : Implementation.Node_Access := First;
            begin
               if Before (The_Priority, Get_Priority (Previous.Element)) then
                  --  insert at first
                  New_Node.Next := First;
                  First := New_Node;
               else
                  loop
                     if Previous.Next = null then
                        --  insert at last
                        Last.Next := New_Node;
                        Last := New_Node;
                        exit;
                     elsif Before (
                        The_Priority,
                        Get_Priority (Previous.Next.Element))
                     then
                        --  insert at Previous.Next
                        New_Node.Next := Previous.Next;
                        Previous.Next := New_Node;
                        exit;
                     else
                        Previous := Previous.Next;
                     end if;
                  end loop;
               end if;
            end;
         end if;
         Current_Length := Current_Length + 1;
         if Current_Length > Peak_Length then
            Peak_Length := Current_Length;
         end if;
      end Enqueue;

--    overriding
      entry Dequeue (Element : out Queue_Interfaces.Element_Type)
         when First /= null is
      begin
         Do_Dequeue (Element);
      end Dequeue;

      procedure Dequeue_Only_High_Priority (
         At_Least : Queue_Priority;
         Element : in out Queue_Interfaces.Element_Type;
         Success : out Boolean) is
      begin
         Success := First /= null
            and then not Before (At_Least, Get_Priority (First.Element));
         if Success then
            Do_Dequeue (Element);
         end if;
      end Dequeue_Only_High_Priority;

--    overriding
      function Current_Use return Count_Type is
      begin
         return Current_Length;
      end Current_Use;

--    overriding
      function Peak_Use return Count_Type is
      begin
         return Peak_Length;
      end Peak_Use;

   end Queue;

end Ada.Containers.Unbounded_Priority_Queues;
