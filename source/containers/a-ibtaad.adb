with Ada.Unchecked_Conversion;
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
      Mark : constant array (Boolean) of Character := "-*";
      procedure Process (Current : Node_Access; Indent : Integer);
      procedure Process (Current : Node_Access; Indent : Integer) is
      begin
         if Current.Left /= null then
            Process (Current.Left, Indent + 1);
         end if;
         Ada.Debug.Put ((1 .. Indent => ' ') &
               Mark (Current = Marker) &
               " (level =" & Level_Type'Image (Downcast (Current).Level) &
               ")");
         if Current.Right /= null then
            Process (Current.Right, Indent + 1);
         end if;
      end Process;
   begin
      Ada.Debug.Put ("---- " & Message);
      if Container /= null then
         Process (Container, 0);
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
