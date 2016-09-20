--  diff (Ada.Exceptions.Finally)
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System;
package body Ada.Containers.Ordered_Maps is
   use type Binary_Trees.Node_Access;
   use type Copy_On_Write.Data_Access;

   function Upcast is
      new Unchecked_Conversion (Cursor, Binary_Trees.Node_Access);
   function Downcast is
      new Unchecked_Conversion (Binary_Trees.Node_Access, Cursor);

   function Upcast is
      new Unchecked_Conversion (Data_Access, Copy_On_Write.Data_Access);
   function Downcast is
      new Unchecked_Conversion (Copy_On_Write.Data_Access, Data_Access);

--  diff (Free)
--  diff (Free)
   procedure Free is new Unchecked_Deallocation (Node, Cursor);

   function Compare_Keys (Left, Right : Key_Type) return Integer;
   function Compare_Keys (Left, Right : Key_Type) return Integer is
   begin
      if Left < Right then
         return -1;
      elsif Right < Left then
         return 1;
      else
         return 0;
      end if;
   end Compare_Keys;

   type Context_Type is limited record
      Left : not null access Key_Type;
   end record;
   pragma Suppress_Initialization (Context_Type);

   function Compare_Key (
      Position : not null Binary_Trees.Node_Access;
      Params : System.Address)
      return Integer;
   function Compare_Key (
      Position : not null Binary_Trees.Node_Access;
      Params : System.Address)
      return Integer
   is
      Context : Context_Type;
      for Context'Address use Params;
   begin
      return Compare_Keys (Context.Left.all, Downcast (Position).Key);
   end Compare_Key;

--  diff (Allocate_Element)
--
--
--
--
--
--
--
--

   procedure Allocate_Node (
      Item : out Cursor;
      Key : Key_Type;
      New_Item : Element_Type);
   procedure Allocate_Node (
      Item : out Cursor;
      Key : Key_Type;
      New_Item : Element_Type)
   is
--  diff
--  diff
--  diff
--  diff
--  diff
--  diff
--  diff
--  diff
--  diff
   begin
      Item := new Node'(Super => <>, Key => Key, Element => New_Item);
--  diff
--  diff
--  diff
--  diff
   end Allocate_Node;

   procedure Copy_Node (
      Target : out Binary_Trees.Node_Access;
      Source : not null Binary_Trees.Node_Access);
   procedure Copy_Node (
      Target : out Binary_Trees.Node_Access;
      Source : not null Binary_Trees.Node_Access)
   is
      Source_Node : constant Cursor := Downcast (Source);
      New_Node : Cursor;
   begin
      Allocate_Node (New_Node, Source_Node.Key, Source_Node.Element);
      Target := Upcast (New_Node);
   end Copy_Node;

   procedure Free_Node (Object : in out Binary_Trees.Node_Access);
   procedure Free_Node (Object : in out Binary_Trees.Node_Access) is
      X : Cursor := Downcast (Object);
   begin
--  diff
--  diff
      Free (X);
      Object := null;
   end Free_Node;

   procedure Allocate_Data (
      Target : out not null Copy_On_Write.Data_Access;
      Max_Length : Count_Type;
      Capacity : Count_Type);
   procedure Allocate_Data (
      Target : out not null Copy_On_Write.Data_Access;
      Max_Length : Count_Type;
      Capacity : Count_Type)
   is
      pragma Unreferenced (Max_Length);
      pragma Unreferenced (Capacity);
      New_Data : constant Data_Access :=
         new Data'(Super => <>, Root => null, Length => 0);
   begin
      Target := Upcast (New_Data);
   end Allocate_Data;

   procedure Copy_Data (
      Target : out not null Copy_On_Write.Data_Access;
      Source : not null Copy_On_Write.Data_Access;
      Length : Count_Type;
      Max_Length : Count_Type;
      Capacity : Count_Type);
   procedure Copy_Data (
      Target : out not null Copy_On_Write.Data_Access;
      Source : not null Copy_On_Write.Data_Access;
      Length : Count_Type;
      Max_Length : Count_Type;
      Capacity : Count_Type)
   is
      pragma Unreferenced (Length);
      pragma Unreferenced (Max_Length);
      pragma Unreferenced (Capacity);
   begin
      Allocate_Data (Target, 0, 0);
      Base.Copy (
         Downcast (Target).Root,
         Downcast (Target).Length,
         Source => Downcast (Source).Root,
         Copy => Copy_Node'Access);
   end Copy_Data;

   procedure Free is new Unchecked_Deallocation (Data, Data_Access);

   procedure Free_Data (Data : in out Copy_On_Write.Data_Access);
   procedure Free_Data (Data : in out Copy_On_Write.Data_Access) is
      X : Data_Access := Downcast (Data);
   begin
      Binary_Trees.Free (X.Root, X.Length, Free => Free_Node'Access);
      Free (X);
      Data := null;
   end Free_Data;

   procedure Unique (Container : in out Map; To_Update : Boolean);
   procedure Unique (Container : in out Map; To_Update : Boolean) is
   begin
      if Copy_On_Write.Shared (Container.Super.Data) then
         Copy_On_Write.Unique (
            Target => Container.Super'Access,
            Target_Length => 0, -- Length is unused
            Target_Capacity => 0, -- Capacity is unused
            New_Length => 0,
            New_Capacity => 0,
            To_Update => To_Update,
            Allocate => Allocate_Data'Access,
            Move => Copy_Data'Access,
            Copy => Copy_Data'Access,
            Free => Free_Data'Access);
      end if;
   end Unique;

   --  implementation

   function Equivalent_Keys (Left, Right : Key_Type) return Boolean is
   begin
      return Compare_Keys (Left, Right) = 0;
   end Equivalent_Keys;

   function Empty_Map return Map is
   begin
      return (Finalization.Controlled with Super => (null, null));
   end Empty_Map;

   function Has_Element (Position : Cursor) return Boolean is
   begin
      return Position /= No_Element;
   end Has_Element;

   overriding function "=" (Left, Right : Map) return Boolean is
      function Equivalent (Left, Right : not null Binary_Trees.Node_Access)
         return Boolean;
      function Equivalent (Left, Right : not null Binary_Trees.Node_Access)
         return Boolean is
      begin
         return Equivalent_Keys (
               Downcast (Left).Key,
               Downcast (Right).Key)
            and then Downcast (Left).Element =
               Downcast (Right).Element;
      end Equivalent;
      Left_Length : constant Count_Type := Length (Left);
      Right_Length : constant Count_Type := Length (Right);
   begin
      if Left_Length /= Right_Length then
         return False;
      elsif Left_Length = 0 or else Left.Super.Data = Right.Super.Data then
         return True;
      else
         Unique (Left'Unrestricted_Access.all, False); -- private
         Unique (Right'Unrestricted_Access.all, False); -- private
         return Binary_Trees.Equivalent (
            Downcast (Left.Super.Data).Root,
            Downcast (Right.Super.Data).Root,
            Equivalent'Access);
      end if;
   end "=";

   function Length (Container : Map) return Count_Type is
      Data : constant Data_Access := Downcast (Container.Super.Data);
   begin
      if Data = null then
         return 0;
      else
         return Data.Length;
      end if;
   end Length;

   function Is_Empty (Container : Map) return Boolean is
      Data : constant Data_Access := Downcast (Container.Super.Data);
   begin
      return Data = null or else Data.Root = null;
   end Is_Empty;

   procedure Clear (Container : in out Map) is
   begin
      Copy_On_Write.Clear (Container.Super'Access, Free => Free_Data'Access);
   end Clear;

   function Key (Position : Cursor) return Key_Type is
   begin
      return Position.Key;
   end Key;

   function Element (Position : Cursor) return Element_Type is
   begin
      return Position.Element;
   end Element;

   procedure Replace_Element (
      Container : in out Map;
      Position : Cursor;
      New_Item : Element_Type) is
   begin
      Unique (Container, True);
--  diff
      Position.Element := New_Item;
   end Replace_Element;

   procedure Query_Element (
      Position : Cursor;
      Process : not null access procedure (
         Key : Key_Type;
         Element : Element_Type)) is
   begin
      Process (Position.Key, Position.Element);
   end Query_Element;

   procedure Update_Element (
      Container : in out Map'Class;
      Position : Cursor;
      Process : not null access procedure (
         Key : Key_Type;
         Element : in out Element_Type)) is
   begin
      Process (
         Position.Key,
         Reference (Map (Container), Position).Element.all);
   end Update_Element;

   function Constant_Reference (Container : aliased Map; Position : Cursor)
      return Constant_Reference_Type
   is
      pragma Unreferenced (Container);
   begin
      return (Element => Position.all.Element'Access); -- [gcc-6] .all
   end Constant_Reference;

   function Reference (Container : aliased in out Map; Position : Cursor)
      return Reference_Type is
   begin
      Unique (Container, True);
--  diff
      return (Element => Position.all.Element'Access); -- [gcc-6] .all
   end Reference;

   function Constant_Reference (Container : aliased Map; Key : Key_Type)
      return Constant_Reference_Type is
   begin
      return Constant_Reference (Container, Find (Container, Key));
   end Constant_Reference;

   function Reference (Container : aliased in out Map; Key : Key_Type)
      return Reference_Type is
   begin
      return Reference (Container, Find (Container, Key));
   end Reference;

   procedure Assign (Target : in out Map; Source : Map) is
   begin
      Copy_On_Write.Assign (
         Target.Super'Access,
         Source.Super'Access,
         Free => Free_Data'Access);
   end Assign;

   function Copy (Source : Map) return Map is
   begin
      return Result : Map do
         Copy_On_Write.Copy (
            Result.Super'Access,
            Source.Super'Access,
            0, -- Length is unused
            0, -- Capacity is unused
            Allocate => Allocate_Data'Access,
            Copy => Copy_Data'Access);
      end return;
   end Copy;

   procedure Move (Target : in out Map; Source : in out Map) is
   begin
      Copy_On_Write.Move (
         Target.Super'Access,
         Source.Super'Access,
         Free => Free_Data'Access);
--  diff
--  diff
--  diff
   end Move;

   procedure Insert (
      Container : in out Map;
      Key : Key_Type;
      New_Item : Element_Type;
      Position : out Cursor;
      Inserted : out Boolean)
   is
--  diff
--  diff
--  diff
--  diff
--  diff
--  diff
--  diff
--  diff
--  diff
--  diff
--  diff
--  diff
--  diff
--  diff
      Before : constant Cursor := Ceiling (Container, Key);
   begin
--  diff
      Inserted := Before = null or else Key < Before.Key;
      if Inserted then
         Unique (Container, True);
         Allocate_Node (Position, Key, New_Item);
         declare
            Data : constant Data_Access := Downcast (Container.Super.Data);
         begin
            Base.Insert (
               Data.Root,
               Data.Length,
               Upcast (Before),
               Upcast (Position));
         end;
      else
         Position := Before;
      end if;
   end Insert;

   procedure Insert (
      Container : in out Map;
      Key : Key_Type;
      Position : out Cursor;
      Inserted : out Boolean)
   is
      Before : constant Cursor := Ceiling (Container, Key);
   begin
      if Before = null or else Key < Before.Key then
         Unique (Container, True);
         Position := new Node'(Super => <>, Key => Key, Element => <>);
         declare
            Data : constant Data_Access := Downcast (Container.Super.Data);
         begin
            Base.Insert (
               Data.Root,
               Data.Length,
               Upcast (Before),
               Upcast (Position));
         end;
         Inserted := True;
      else
         Position := Before;
         Inserted := False;
      end if;
   end Insert;

   procedure Insert (
      Container : in out Map;
      Key : Key_Type;
      New_Item : Element_Type)
   is
      Position : Cursor;
      Inserted : Boolean;
   begin
      Insert (Container, Key, New_Item, Position, Inserted);
      if not Inserted then
         raise Constraint_Error;
      end if;
   end Insert;

   procedure Include (
      Container : in out Map;
      Key : Key_Type;
      New_Item : Element_Type)
   is
      Position : Cursor;
      Inserted : Boolean;
   begin
      Insert (Container, Key, New_Item, Position, Inserted);
      if not Inserted then
         Replace_Element (Container, Position, New_Item);
      end if;
   end Include;

   procedure Replace (
      Container : in out Map;
      Key : Key_Type;
      New_Item : Element_Type) is
   begin
      Replace_Element (Container, Find (Container, Key), New_Item);
   end Replace;

   procedure Exclude (Container : in out Map; Key : Key_Type) is
      Position : Cursor := Find (Container, Key);
   begin
      if Position /= null then
         Delete (Container, Position);
      end if;
   end Exclude;

   procedure Delete (Container : in out Map; Key : Key_Type) is
      Position : Cursor := Find (Container, Key);
   begin
      Delete (Container, Position);
   end Delete;

   procedure Delete (Container : in out Map; Position : in out Cursor) is
      Position_2 : Binary_Trees.Node_Access := Upcast (Position);
   begin
      Unique (Container, True);
      declare
         Data : constant Data_Access := Downcast (Container.Super.Data);
      begin
         Base.Remove (Data.Root, Data.Length, Position_2);
      end;
      Free_Node (Position_2);
      Position := null;
   end Delete;

   procedure Delete_First (Container : in out Map'Class) is
      Position : Cursor := First (Map (Container));
   begin
      Delete (Map (Container), Position);
   end Delete_First;

   procedure Delete_Last (Container : in out Map'Class) is
      Position : Cursor := Last (Map (Container));
   begin
      Delete (Map (Container), Position);
   end Delete_Last;

   function First (Container : Map) return Cursor is
   begin
      if Is_Empty (Container) then
         return null;
      else
         Unique (Container'Unrestricted_Access.all, False);
         return Downcast (Binary_Trees.First (
            Downcast (Container.Super.Data).Root));
      end if;
   end First;

   function First_Element (Container : Map'Class)
      return Element_Type is
   begin
      return Element (First (Map (Container)));
   end First_Element;

   function First_Key (Container : Map'Class)
      return Key_Type is
   begin
      return Key (First (Map (Container)));
   end First_Key;

   function Last (Container : Map) return Cursor is
   begin
      if Is_Empty (Container) then
         return null;
      else
         Unique (Container'Unrestricted_Access.all, False);
         return Downcast (Binary_Trees.Last (
            Downcast (Container.Super.Data).Root));
      end if;
   end Last;

   function Last_Element (Container : Map'Class)
      return Element_Type is
   begin
      return Element (Last (Map (Container)));
   end Last_Element;

   function Last_Key (Container : Map'Class)
      return Key_Type is
   begin
      return Key (Last (Map (Container)));
   end Last_Key;

   function Next (Position : Cursor) return Cursor is
   begin
      return Downcast (Binary_Trees.Next (Upcast (Position)));
   end Next;

   procedure Next (Position : in out Cursor) is
   begin
      Position := Downcast (Binary_Trees.Next (Upcast (Position)));
   end Next;

   function Previous (Position : Cursor) return Cursor is
   begin
      return Downcast (Binary_Trees.Previous (Upcast (Position)));
   end Previous;

   procedure Previous (Position : in out Cursor) is
   begin
      Position := Downcast (Binary_Trees.Previous (Upcast (Position)));
   end Previous;

   function Find (Container : Map; Key : Key_Type) return Cursor is
   begin
      if Is_Empty (Container) then
         return null;
      else
         Unique (Container'Unrestricted_Access.all, False);
         declare
            Context : Context_Type := (Left => Key'Unrestricted_Access);
         begin
            return Downcast (Binary_Trees.Find (
               Downcast (Container.Super.Data).Root,
               Binary_Trees.Just,
               Context'Address,
               Compare => Compare_Key'Access));
         end;
      end if;
   end Find;

   function Element (
      Container : Map'Class;
      Key : Key_Type)
      return Element_Type is
   begin
      return Element (Find (Map (Container), Key));
   end Element;

   function Floor (Container : Map; Key : Key_Type) return Cursor is
   begin
      if Is_Empty (Container) then
         return null;
      else
         Unique (Container'Unrestricted_Access.all, False);
         declare
            Context : Context_Type := (Left => Key'Unrestricted_Access);
         begin
            return Downcast (Binary_Trees.Find (
               Downcast (Container.Super.Data).Root,
               Binary_Trees.Floor,
               Context'Address,
               Compare => Compare_Key'Access));
         end;
      end if;
   end Floor;

   function Ceiling (Container : Map; Key : Key_Type) return Cursor is
   begin
      if Is_Empty (Container) then
         return null;
      else
         Unique (Container'Unrestricted_Access.all, False);
         declare
            Context : Context_Type := (Left => Key'Unrestricted_Access);
         begin
            return Downcast (Binary_Trees.Find (
               Downcast (Container.Super.Data).Root,
               Binary_Trees.Ceiling,
               Context'Address,
               Compare => Compare_Key'Access));
         end;
      end if;
   end Ceiling;

   function Contains (Container : Map; Key : Key_Type) return Boolean is
   begin
      return Find (Container, Key) /= null;
   end Contains;

   function "<" (Left, Right : Cursor) return Boolean is
   begin
      return Left /= Right and then Left.Key < Right.Key;
   end "<";

   function ">" (Left, Right : Cursor) return Boolean is
   begin
      return Right < Left;
   end ">";

   function "<" (Left : Cursor; Right : Key_Type) return Boolean is
   begin
      return Left.Key < Right;
   end "<";

   function ">" (Left : Cursor; Right : Key_Type) return Boolean is
   begin
      return Right < Left;
   end ">";

   function "<" (Left : Key_Type; Right : Cursor) return Boolean is
   begin
      return Left < Right.Key;
   end "<";

   function ">" (Left : Key_Type; Right : Cursor) return Boolean is
   begin
      return Right < Left;
   end ">";

   procedure Iterate (
      Container : Map'Class;
      Process : not null access procedure (Position : Cursor))
   is
      type P1 is access procedure (Position : Cursor);
      type P2 is access procedure (Position : Binary_Trees.Node_Access);
      function Cast is new Unchecked_Conversion (P1, P2);
   begin
      if not Is_Empty (Map (Container)) then
         Unique (Map (Container)'Unrestricted_Access.all, False);
         Binary_Trees.Iterate (
            Downcast (Container.Super.Data).Root,
            Cast (Process));
      end if;
   end Iterate;

   procedure Reverse_Iterate (
      Container : Map'Class;
      Process : not null access procedure (Position : Cursor))
   is
      type P1 is access procedure (Position : Cursor);
      type P2 is access procedure (Position : Binary_Trees.Node_Access);
      function Cast is new Unchecked_Conversion (P1, P2);
   begin
      if not Is_Empty (Map (Container)) then
         Unique (Map (Container)'Unrestricted_Access.all, False);
         Binary_Trees.Reverse_Iterate (
            Downcast (Container.Super.Data).Root,
            Cast (Process));
      end if;
   end Reverse_Iterate;

   function Iterate (Container : Map'Class)
      return Map_Iterator_Interfaces.Reversible_Iterator'Class is
   begin
      return Map_Iterator'(
         First => First (Map (Container)),
         Last => Last (Map (Container)));
   end Iterate;

   function Iterate (Container : Map'Class; First, Last : Cursor)
      return Map_Iterator_Interfaces.Reversible_Iterator'Class
   is
      pragma Unreferenced (Container);
      Actual_First : Cursor := First;
      Actual_Last : Cursor := Last;
   begin
      if Actual_First = No_Element
         or else Actual_Last = No_Element
         or else Actual_Last < Actual_First
      then
         Actual_First := No_Element;
         Actual_Last := No_Element;
      end if;
      return Map_Iterator'(First => Actual_First, Last => Actual_Last);
   end Iterate;

   overriding procedure Adjust (Object : in out Map) is
   begin
      Copy_On_Write.Adjust (Object.Super'Access);
   end Adjust;

   overriding function First (Object : Map_Iterator) return Cursor is
   begin
      return Object.First;
   end First;

   overriding function Next (Object : Map_Iterator; Position : Cursor)
      return Cursor is
   begin
      if Position = Object.Last then
         return No_Element;
      else
         return Next (Position);
      end if;
   end Next;

   overriding function Last (Object : Map_Iterator) return Cursor is
   begin
      return Object.Last;
   end Last;

   overriding function Previous (Object : Map_Iterator; Position : Cursor)
      return Cursor is
   begin
      if Position = Object.First then
         return No_Element;
      else
         return Previous (Position);
      end if;
   end Previous;

   package body Streaming is

      procedure Read (
         Stream : not null access Streams.Root_Stream_Type'Class;
         Item : out Map)
      is
         Length : Count_Type'Base;
      begin
         Count_Type'Base'Read (Stream, Length);
         Clear (Item);
         for I in 1 .. Length loop
            declare
               New_Key : Key_Type;
               Position : Cursor;
               Inserted : Boolean;
               pragma Unreferenced (Inserted);
            begin
               Key_Type'Read (Stream, New_Key);
               Insert (Item, New_Key, Position, Inserted);
               Element_Type'Read (Stream, Position.Element);
            end;
         end loop;
      end Read;

      procedure Write (
         Stream : not null access Streams.Root_Stream_Type'Class;
         Item : Map)
      is
         Length : constant Count_Type := Ordered_Maps.Length (Item);
      begin
         Count_Type'Write (Stream, Length);
         if Length > 0 then
            declare
               Position : Cursor := First (Item);
            begin
               while Position /= null loop
                  Key_Type'Write (Stream, Position.Key);
                  Element_Type'Write (Stream, Position.Element);
                  Next (Position);
               end loop;
            end;
         end if;
      end Write;

   end Streaming;

end Ada.Containers.Ordered_Maps;
