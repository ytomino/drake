with Ada.Unchecked_Conversion;
with System.Formatting.Address;
with System.Termination;
package body Ada.Containers.Binary_Trees.Arne_Andersson.Debug is

   type AA_Node_Access is access Node;

   function Downcast is new Unchecked_Conversion (Node_Access, AA_Node_Access);

   --  implementation

   function Root (Node : not null Node_Access) return not null Node_Access is
      Result : not null Node_Access := Node;
   begin
      while Result.Parent /= null loop
         Result := Result.Parent;
      end loop;
      return Result;
   end Root;

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
      subtype Buffer_Type is String (1 .. 256);
      procedure Put (
         Buffer : in out Buffer_Type;
         Last : in out Natural;
         S : String);
      procedure Put (
         Buffer : in out Buffer_Type;
         Last : in out Natural;
         S : String)
      is
         First : constant Natural := Last + 1;
      begin
         Last := Last + S'Length;
         Buffer (First .. Last) := S;
      end Put;
      Buffer : Buffer_Type;
      Last : Natural;
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
         Last := 0;
         Put (Buffer, Last, Indent_S);
         Put (Buffer, Last, " 0x");
         System.Formatting.Address.Image (
            Current.all'Address,
            Buffer (
               Last + 1 ..
               Last + System.Formatting.Address.Address_String'Length),
            Set => System.Formatting.Lower_Case);
         Last := Last + System.Formatting.Address.Address_String'Length;
         Put (Buffer, Last, " (level =");
         Put (Buffer, Last, Level_Type'Image (Downcast (Current).Level));
         Put (Buffer, Last, ")");
         System.Termination.Error_Put_Line (Buffer (1 .. Last));
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
      Last := 0;
      Put (Buffer, Last, "Tree:");
      if Message'Length > 0 then
         Put (Buffer, Last, " ");
         Put (Buffer, Last, Message);
      end if;
      System.Termination.Error_Put_Line (Buffer (1 .. Last));
      if Container /= null then
         Process (Container, 2);
      end if;
      return True;
   end Dump;

   function Valid (
      Container : Node_Access;
      Length : Count_Type;
      Level_Check : Boolean := True)
      return Boolean
   is
      Count : Count_Type := 0;
      function Process (Position : not null Node_Access) return Boolean;
      function Process (Position : not null Node_Access) return Boolean is
      begin
         Count := Count + 1;
         if Level_Check then
            if Downcast (Position).Level > 0 then
               if Position.Left = null
                  or else Position.Right = null
               then
                  return False;
               end if;
            else
               if Position.Left /= null then
                  return False;
               end if;
            end if;
            if Position.Left /= null then
               if Downcast (Position).Level /=
                  Downcast (Position.Left).Level + 1
               then
                  return False;
               end if;
            end if;
            if Position.Right /= null then
               if Downcast (Position).Level not in
                  Downcast (Position.Right).Level ..
                  Downcast (Position.Right).Level + 1
               then
                  return False;
               end if;
               if Position.Right.Right /= null then
                  if Downcast (Position).Level <=
                     Downcast (Position.Right.Right).Level
                  then
                     return False;
                  end if;
               end if;
            end if;
         end if;
         if Position.Left /= null then
            if Position.Left.Parent /= Position then
               return False;
            end if;
            if not Process (Position.Left) then
               return False;
            end if;
         end if;
         if Position.Right /= null then
            if Position.Right.Parent /= Position then
               return False;
            end if;
            if not Process (Position.Right) then
               return False;
            end if;
         end if;
         return True;
      end Process;
   begin
      if Container /= null then
         if Container.Parent /= null then
            return False;
         end if;
         if not Process (Container) then
            return False;
         end if;
      end if;
      return Count = Length;
   end Valid;

end Ada.Containers.Binary_Trees.Arne_Andersson.Debug;
