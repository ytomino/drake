with Ada.Unchecked_Conversion;
with System.Secondary_Stack.Debug; -- Error_Put for System.Address
with System.Termination;
package body Ada.Containers.Inside.Binary_Trees.Arne_Andersson.Debug is

   type AA_Node_Access is access Node;

   function Downcast is new Unchecked_Conversion (Node_Access, AA_Node_Access);

   --  implementation

   function Dump (
      Container : Node_Access;
      Marker : Node_Access;
      Message : String := "")
      return Boolean
   is
      function Probe (Current : Node_Access) return Integer;
      function Probe (Current : Node_Access) return Integer is
      begin
         if Current = null then
            return 0;
         else
            return Integer'Max (
               Probe (Current.Left),
               Probe (Current.Right)) + 1;
         end if;
      end Probe;
      Deeper : constant Integer := Probe (Container);
      Indent_S : String (1 .. 2 * Deeper) := (others => ' ');
      Mark : constant array (Boolean) of Character := "-*";
      procedure Process (Current : Node_Access; Indent : Integer);
      procedure Process (Current : Node_Access; Indent : Integer) is
         B : Character;
         C : Character;
      begin
         if Current.Left /= null then
            Process (Current.Left, Indent + 2);
         else
            Indent_S (Indent) := '|';
         end if;
         if Indent > 2 then
            if Indent_S (Indent - 2) = ' ' then
               C := '|';
            else
               C := ' ';
            end if;
            Indent_S (Indent - 2) := '+';
         end if;
         Indent_S (Indent - 1) := Mark (Current = Marker);
         B := Indent_S (Indent);
         if Current.Left = null and then Current.Right = null then
            Indent_S (Indent) := '-';
         else
            Indent_S (Indent) := '+';
         end if;
         System.Termination.Error_Put (Indent_S);
         System.Termination.Error_Put (" ");
         System.Secondary_Stack.Debug.Error_Put (Current.all'Address);
         System.Termination.Error_Put (" (level =");
         System.Termination.Error_Put (
            Level_Type'Image (Downcast (Current).Level));
         System.Termination.Error_Put (")");
         System.Termination.Error_New_Line;
         Indent_S (Indent) := B;
         Indent_S (Indent - 1) := ' ';
         if Indent > 2 then
            Indent_S (Indent - 2) := C;
         end if;
         if Current.Right /= null then
            Process (Current.Right, Indent + 2);
         else
            Indent_S (Indent) := ' ';
         end if;
      end Process;
   begin
      System.Termination.Error_Put ("Tree:");
      if Message'Length > 0 then
         System.Termination.Error_Put (" ");
         System.Termination.Error_Put (Message);
      end if;
      System.Termination.Error_New_Line;
      if Container /= null then
         Process (Container, 2);
      end if;
      return True;
   end Dump;

   function Validate (
      Container : Node_Access;
      Length : Count_Type;
      Level_Check : Boolean := True)
      return Boolean
   is
      Count : Count_Type := 0;
      procedure Process (Position : not null Node_Access);
      procedure Process (Position : not null Node_Access) is
      begin
         Count := Count + 1;
         if Level_Check then
            if Downcast (Position).Level > 0 then
               pragma Assert (Position.Left /= null);
               pragma Assert (Position.Right /= null);
               null;
            else
               pragma Assert (Position.Left = null);
               null;
            end if;
            if Position.Left /= null then
               pragma Assert (Downcast (Position).Level =
                              Downcast (Position.Left).Level + 1);
               null;
            end if;
            if Position.Right /= null then
               pragma Assert (Downcast (Position).Level >=
                              Downcast (Position.Right).Level);
               pragma Assert (Downcast (Position).Level -
                              Downcast (Position.Right).Level <= 1);
               if Position.Right.Right /= null then
                  pragma Assert (Downcast (Position).Level >
                                 Downcast (Position.Right.Right).Level);
                  null;
               end if;
            end if;
         end if;
         if Position.Left /= null then
            pragma Assert (Position.Left.Parent = Position);
            Process (Position.Left);
         end if;
         if Position.Right /= null then
            pragma Assert (Position.Right.Parent = Position);
            Process (Position.Right);
         end if;
      end Process;
   begin
      if Container /= null then
         pragma Assert (Container.Parent = null);
         Process (Container);
      end if;
      pragma Assert (Count = Length);
      return True;
   end Validate;

end Ada.Containers.Inside.Binary_Trees.Arne_Andersson.Debug;
