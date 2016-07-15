with Ada.Unchecked_Conversion;
with System.Formatting.Address;
with System.Termination;
package body Ada.Containers.Binary_Trees.Arne_Andersson.Debug is

   type AA_Node_Access is access Node;

   function Downcast is new Unchecked_Conversion (Node_Access, AA_Node_Access);

   function Height (Container : Node_Access) return Natural;
   function Height (Container : Node_Access) return Natural is
      procedure Nonnull_First (
         Item : in out Node_Access;
         Height : in out Natural);
      procedure Nonnull_First (
         Item : in out Node_Access;
         Height : in out Natural) is
      begin
         while Item.Left /= null loop
            Item := Item.Left;
            Height := Height + 1;
         end loop;
      end Nonnull_First;
      procedure Next (Item : in out Node_Access; Height : in out Natural);
      procedure Next (Item : in out Node_Access; Height : in out Natural) is
      begin
         if Item.Right /= null then
            Item := Item.Right;
            Height := Height + 1;
            Nonnull_First (Item, Height);
         else
            loop
               declare
                  P : constant Node_Access := Item;
               begin
                  Item := Item.Parent;
                  Height := Height - 1;
                  exit when Item = null or else Item.Right /= P;
               end;
            end loop;
         end if;
      end Next;
      Result : Integer := 0;
   begin
      if Container /= null then
         declare
            I : Node_Access := Container;
            H : Natural := 1;
         begin
            Nonnull_First (I, H);
            while I /= null loop
               Result := Integer'Max (Result, H);
               Next (I, H);
            end loop;
         end;
      end if;
      return Result;
   end Height;

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
      Indent_S : String (1 .. 2 * Height (Container)) := (others => ' ');
      Mark : constant array (Boolean) of Character := "-*";
   begin
      Last := 0;
      Put (Buffer, Last, "Tree:");
      if Message'Length > 0 then
         Put (Buffer, Last, " ");
         Put (Buffer, Last, Message);
      end if;
      System.Termination.Error_Put_Line (Buffer (1 .. Last));
      declare
         Position : Node_Access := First (Container);
      begin
         while Position /= null loop
            declare
               Indent : Natural := 2;
               B : Character;
               C : Character;
            begin
               declare
                  P : Node_Access := Position.Parent;
               begin
                  while P /= null loop
                     Indent := Indent + 2;
                     P := P.Parent;
                  end loop;
               end;
               if Position.Left = null then
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
               Indent_S (Indent - 1) := Mark (Position = Marker);
               B := Indent_S (Indent);
               if Position.Left = null and then Position.Right = null then
                  Indent_S (Indent) := '-';
               else
                  Indent_S (Indent) := '+';
               end if;
               Last := 0;
               Put (Buffer, Last, Indent_S);
               Put (Buffer, Last, " 0x");
               System.Formatting.Address.Image (
                  Position.all'Address,
                  Buffer (
                     Last + 1 ..
                     Last + System.Formatting.Address.Address_String'Length),
                  Set => System.Formatting.Lower_Case);
               Last := Last + System.Formatting.Address.Address_String'Length;
               Put (Buffer, Last, " (level = ");
               declare
                  Error : Boolean;
               begin
                  System.Formatting.Image (
                     System.Formatting.Unsigned'Mod (
                        Downcast (Position).Level),
                     Buffer (Last + 1 .. Buffer'Last),
                     Last,
                     Error => Error);
               end;
               Put (Buffer, Last, ")");
               System.Termination.Error_Put_Line (Buffer (1 .. Last));
               Indent_S (Indent) := B;
               Indent_S (Indent - 1) := ' ';
               if Indent > 2 then
                  Indent_S (Indent - 2) := C;
               end if;
               if Position.Right = null then
                  Indent_S (Indent) := ' ';
               end if;
            end;
            Position := Next (Position);
         end loop;
      end;
      return True;
   end Dump;

   function Valid (
      Container : Node_Access;
      Length : Count_Type;
      Level_Check : Boolean := True)
      return Boolean
   is
      Position : Node_Access := First (Container);
      Count : Count_Type := 0;
   begin
      while Position /= null loop
         Count := Count + 1;
         if Level_Check then
            if Downcast (Position).Level > 0 then
               if Position.Left = null or else Position.Right = null then
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
         end if;
         if Position.Right /= null then
            if Position.Right.Parent /= Position then
               return False;
            end if;
         end if;
         if Position.Parent = null then
            if Container /= Position then
               return False;
            end if;
         else
            if Position.Parent.Left /= Position
               and then Position.Parent.Right /= Position
            then
               return False;
            end if;
         end if;
         Position := Next (Position);
      end loop;
      return Count = Length;
   end Valid;

end Ada.Containers.Binary_Trees.Arne_Andersson.Debug;
