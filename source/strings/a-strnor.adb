--  reference:
--  http://www.unicode.org/reports/tr15/
with Ada.Characters.Conversions;
with Ada.Strings.Canonical_Composites;
with Ada.UCD;
package body Ada.Strings.Normalization is
   use type UCD.UCS_4;

   function Standard_Equal (Left, Right : Wide_Wide_String) return Boolean;
   function Standard_Equal (Left, Right : Wide_Wide_String) return Boolean is
   begin
      return Left = Right;
   end Standard_Equal;

   function Standard_Less (Left, Right : Wide_Wide_String) return Boolean;
   function Standard_Less (Left, Right : Wide_Wide_String) return Boolean is
   begin
      return Left < Right;
   end Standard_Less;

   --  NFD

   procedure D_Buff (
      Item : in out Wide_Wide_String;
      Last : in out Natural;
      Decomposed : out Boolean);
   procedure D_Buff (
      Item : in out Wide_Wide_String;
      Last : in out Natural;
      Decomposed : out Boolean) is
   begin
      Decomposed := False;
      for I in reverse Item'First .. Last loop
         declare
            D : constant Natural := Canonical_Composites.D_Find (Item (I));
            To : Canonical_Composites.Decomposed_Wide_Wide_String;
            To_Length : Natural;
         begin
            if D > 0 then
               To := Canonical_Composites.D_Map (D).To;
               To_Length := Canonical_Composites.Decomposed_Length (To);
            elsif Wide_Wide_Character'Pos (Item (I)) in
               UCD.Hangul.SBase ..
               UCD.Hangul.SBase + UCD.Hangul.SCount - 1
            then
               --  S to LV[T]
               declare
                  SIndex : constant UCD.UCS_4 :=
                     Wide_Wide_Character'Pos (Item (I)) - UCD.Hangul.SBase;
                  L : constant UCD.UCS_4 :=
                     UCD.Hangul.LBase + SIndex / UCD.Hangul.NCount;
                  V : constant UCD.UCS_4 :=
                     UCD.Hangul.VBase
                     + SIndex rem UCD.Hangul.NCount / UCD.Hangul.TCount;
                  T : constant UCD.UCS_4 :=
                     UCD.Hangul.TBase + SIndex rem UCD.Hangul.TCount;
               begin
                  To (1) := Wide_Wide_Character'Val (L);
                  To (2) := Wide_Wide_Character'Val (V);
                  To_Length := 2;
                  if T /= UCD.Hangul.TBase then
                     To (3) := Wide_Wide_Character'Val (T);
                     To_Length := 3;
                  end if;
               end;
            else
               goto Continue;
            end if;
            --  replacing
            Decomposed := True;
            Item (I + To_Length .. Last + To_Length - 1) :=
               Item (I + 1 .. Last);
            Item (I .. I + To_Length - 1) := To (1 .. To_Length);
            Last := Last + To_Length - 1;
         end;
      <<Continue>>
         null;
      end loop;
   end D_Buff;

   --  NFC

   procedure C_Buff (
      Item : in out Wide_Wide_String;
      Last : in out Natural;
      Composed : out Boolean);
   procedure C_Buff (
      Item : in out Wide_Wide_String;
      Last : in out Natural;
      Composed : out Boolean) is
   begin
      Composed := False;
      for I in reverse Item'First .. Last - 1 loop
         Process_After_Index : loop
            declare
               From : constant
                  Canonical_Composites.Composing_Wide_Wide_String :=
                  (Item (I), Item (I + 1));
               C : constant Natural := Canonical_Composites.C_Find (From);
               To : Wide_Wide_Character;
            begin
               if C > 0 then
                  To := Canonical_Composites.C_Map (C).To;
               elsif Wide_Wide_Character'Pos (From (1)) in
                     UCD.Hangul.LBase ..
                     UCD.Hangul.LBase + UCD.Hangul.LCount - 1
                  and then Wide_Wide_Character'Pos (From (2)) in
                     UCD.Hangul.VBase ..
                     UCD.Hangul.VBase + UCD.Hangul.VCount - 1
               then
                  --  LV to S
                  declare
                     LIndex : constant UCD.UCS_4 :=
                        Wide_Wide_Character'Pos (From (1))
                        - UCD.Hangul.LBase;
                     VIndex : constant UCD.UCS_4 :=
                        Wide_Wide_Character'Pos (From (2))
                        - UCD.Hangul.VBase;
                  begin
                     To := Wide_Wide_Character'Val (
                        UCD.Hangul.SBase
                        + (LIndex * UCD.Hangul.VCount + VIndex)
                           * UCD.Hangul.TCount);
                  end;
               elsif Wide_Wide_Character'Pos (From (1)) in
                     UCD.Hangul.SBase ..
                     UCD.Hangul.SBase + UCD.Hangul.SCount - 1
                  and then Wide_Wide_Character'Pos (From (2)) in
                     UCD.Hangul.TBase ..
                     UCD.Hangul.TBase + UCD.Hangul.TCount - 1
               then
                  --  ST to T
                  declare
                     ch : constant UCD.UCS_4 :=
                        Wide_Wide_Character'Pos (From (1));
                     TIndex : constant UCD.UCS_4 :=
                        Wide_Wide_Character'Pos (From (2)) - UCD.Hangul.TBase;
                  begin
                     To := Wide_Wide_Character'Val (ch + TIndex);
                  end;
               else
                  exit Process_After_Index;
               end if;
               --  replacing
               Composed := True;
               Item (I) := To;
               Last := Last - 1;
               exit Process_After_Index when Last <= I;
               Item (I + 1 .. Last) := Item (I + 2 .. Last + 1);
            end;
         end loop Process_After_Index;
      end loop;
   end C_Buff;

   generic
      type Character_Type is (<>);
      type String_Type is array (Positive range <>) of Character_Type;
      Max_Length : Positive;
      with procedure Get (
         Data : String_Type;
         Last : out Natural;
         Result : out Wide_Wide_Character;
         Is_Illegal_Sequence : out Boolean);
      with procedure Put (
         Code : Wide_Wide_Character;
         Result : out String_Type;
         Last : out Natural);
      with procedure Start (Item : String_Type; State : out Composites.State);
      with procedure Get_Combined (
         State : in out Composites.State;
         Item : String_Type;
         Last : out Natural;
         Is_Illegal_Sequence : out Boolean);
   package Generic_Normalization is

      procedure Decode (
         Item : String_Type;
         Buffer : out Wide_Wide_String;
         Buffer_Last : out Natural);
      procedure Encode (
         Item : Wide_Wide_String;
         Buffer : out String_Type;
         Buffer_Last : out Natural);

      procedure Decompose_No_Length_Check (
         State : in out Composites.State;
         Item : String_Type;
         Last : out Natural;
         Out_Item : out String_Type;
         Out_Last : out Natural);

      procedure Decompose (
         Item : String_Type;
         Last : out Natural;
         Out_Item : out String_Type;
         Out_Last : out Natural);

      procedure Decompose (
         State : in out Composites.State;
         Item : String_Type;
         Last : out Natural;
         Out_Item : out String_Type;
         Out_Last : out Natural);

      procedure Decompose (
         Item : String_Type;
         Out_Item : out String_Type;
         Out_Last : out Natural);

      function Decompose (Item : String_Type) return String_Type;

      procedure Compose_No_Length_Check (
         State : in out Composites.State;
         Item : String_Type;
         Last : out Natural;
         Out_Item : out String_Type;
         Out_Last : out Natural);

      procedure Compose (
         Item : String_Type;
         Last : out Natural;
         Out_Item : out String_Type;
         Out_Last : out Natural);

      procedure Compose (
         State : in out Composites.State;
         Item : String_Type;
         Last : out Natural;
         Out_Item : out String_Type;
         Out_Last : out Natural);

      procedure Compose (
         Item : String_Type;
         Out_Item : out String_Type;
         Out_Last : out Natural);

      function Compose (Item : String_Type) return String_Type;

      function Equal (
         Left, Right : String_Type)
         return Boolean;

      function Equal (
         Left, Right : String_Type;
         Equal_Combined : not null access function (
            Left, Right : Wide_Wide_String)
            return Boolean)
         return Boolean;

      function Less (
         Left, Right : String_Type)
         return Boolean;

      function Less (
         Left, Right : String_Type;
         Less_Combined : not null access function (
            Left, Right : Wide_Wide_String)
            return Boolean)
         return Boolean;

   end Generic_Normalization;

   package body Generic_Normalization is

      procedure Decode (
         Item : String_Type;
         Buffer : out Wide_Wide_String;
         Buffer_Last : out Natural)
      is
         Last : Natural := Item'First - 1;
         Code : Wide_Wide_Character;
         Is_Illegal_Sequence : Boolean; -- ignore
      begin
         Buffer_Last := Buffer'First - 1;
         while Last < Item'Last loop
            Get (
               Item (Last + 1 .. Item'Last),
               Last,
               Code,
               Is_Illegal_Sequence);
            Buffer_Last := Buffer_Last + 1;
            Buffer (Buffer_Last) := Code;
         end loop;
      end Decode;

      procedure Encode (
         Item : Wide_Wide_String;
         Buffer : out String_Type;
         Buffer_Last : out Natural) is
      begin
         Buffer_Last := Buffer'First - 1;
         for I in Item'Range loop
            Put (
               Item (I),
               Buffer (Buffer_Last + 1 .. Buffer'Last),
               Buffer_Last);
         end loop;
      end Encode;

      procedure Decompose_No_Length_Check (
         State : in out Composites.State;
         Item : String_Type;
         Last : out Natural;
         Out_Item : out String_Type;
         Out_Last : out Natural)
      is
         Is_Illegal_Sequence : Boolean;
      begin
         --  get one combining character sequence
         Get_Combined (State, Item, Last, Is_Illegal_Sequence);
         if not Is_Illegal_Sequence then
            --  normalization
            declare
               Decomposed : Boolean;
               Buffer : Wide_Wide_String (
                  1 ..
                  Expanding * Max_Length * (Last - Item'First + 1));
               Buffer_Last : Natural;
            begin
               --  decoding
               Decode (
                  Item (Item'First .. Last),
                  Buffer,
                  Buffer_Last);
               --  decomposing
               D_Buff (Buffer, Buffer_Last, Decomposed);
               --  encoding
               if Decomposed then
                  Encode (Buffer (1 .. Buffer_Last), Out_Item, Out_Last);
                  return;
               end if;
            end;
         end if;
         Out_Last := Out_Item'First + (Last - Item'First);
         Out_Item (Out_Item'First .. Out_Last) := Item (Item'First .. Last);
      end Decompose_No_Length_Check;

      procedure Decompose (
         Item : String_Type;
         Last : out Natural;
         Out_Item : out String_Type;
         Out_Last : out Natural) is
      begin
         if Item'Length = 0 then
            Last := Item'First - 1;
            Out_Last := Out_Item'First - 1;
         else
            Canonical_Composites.Initialize_D;
            declare
               St : Composites.State;
            begin
               Start (Item, St);
               Decompose_No_Length_Check (St, Item, Last, Out_Item, Out_Last);
            end;
         end if;
      end Decompose;

      procedure Decompose (
         State : in out Composites.State;
         Item : String_Type;
         Last : out Natural;
         Out_Item : out String_Type;
         Out_Last : out Natural) is
      begin
         if Item'Length = 0 then
            --  finished
            Last := Item'Last;
            State.Next_Character := Wide_Wide_Character'Val (0);
            State.Next_Combining_Class := 0;
            State.Next_Is_Illegal_Sequence := False;
            State.Next_Last := Last;
            Out_Last := Out_Item'First - 1;
         else
            Canonical_Composites.Initialize_D;
            Decompose_No_Length_Check (State, Item, Last, Out_Item, Out_Last);
         end if;
      end Decompose;

      procedure Decompose (
         Item : String_Type;
         Out_Item : out String_Type;
         Out_Last : out Natural) is
      begin
         Out_Last := Out_Item'First - 1;
         if Item'Length > 0 then
            Canonical_Composites.Initialize_D;
            declare
               St : Composites.State;
               Last : Natural := Item'First - 1;
            begin
               Start (Item, St);
               while Last < Item'Last loop
                  Decompose_No_Length_Check (
                     St,
                     Item (Last + 1 .. Item'Last),
                     Last,
                     Out_Item (Out_Last + 1 .. Out_Item'Last),
                     Out_Last);
               end loop;
            end;
         end if;
      end Decompose;

      function Decompose (Item : String_Type) return String_Type is
         Result : String_Type (1 .. Expanding * Max_Length * Item'Length);
         Result_Last : Natural := Result'First - 1;
      begin
         Decompose (Item, Result, Result_Last);
         return Result (Result'First .. Result_Last);
      end Decompose;

      procedure Compose_No_Length_Check (
         State : in out Composites.State;
         Item : String_Type;
         Last : out Natural;
         Out_Item : out String_Type;
         Out_Last : out Natural)
      is
         Is_Illegal_Sequence : Boolean;
      begin
         --  get one combining character sequence
         Get_Combined (State, Item, Last, Is_Illegal_Sequence);
         if not Is_Illegal_Sequence then
            --  normalization
            declare
               Decomposed : Boolean;
               Composed : Boolean;
               Buffer : Wide_Wide_String (
                  1 ..
                  Expanding * Max_Length * (Last - Item'First + 1));
               Buffer_Last : Natural;
            begin
               --  decoding
               Decode (
                  Item (Item'First .. Last),
                  Buffer,
                  Buffer_Last);
               --  first, decomposing
               D_Buff (Buffer, Buffer_Last, Decomposed);
               --  next, composing
               C_Buff (Buffer, Buffer_Last, Composed);
               --  encoding
               if Decomposed or else Composed then
                  Encode (Buffer (1 .. Buffer_Last), Out_Item, Out_Last);
                  return;
               end if;
            end;
         end if;
         Out_Last := Out_Item'First + (Last - Item'First);
         Out_Item (Out_Item'First .. Out_Last) := Item (Item'First .. Last);
      end Compose_No_Length_Check;

      procedure Compose (
         Item : String_Type;
         Last : out Natural;
         Out_Item : out String_Type;
         Out_Last : out Natural) is
      begin
         if Item'Length = 0 then
            Last := Item'First - 1;
            Out_Last := Out_Item'First - 1;
         else
            Canonical_Composites.Initialize_C;
            declare
               St : Composites.State;
            begin
               Start (Item, St);
               Compose_No_Length_Check (St, Item, Last, Out_Item, Out_Last);
            end;
         end if;
      end Compose;

      procedure Compose (
         State : in out Composites.State;
         Item : String_Type;
         Last : out Natural;
         Out_Item : out String_Type;
         Out_Last : out Natural) is
      begin
         if Item'Length = 0 then
            --  finished
            Last := Item'Last;
            State.Next_Character := Wide_Wide_Character'Val (0);
            State.Next_Combining_Class := 0;
            State.Next_Is_Illegal_Sequence := False;
            State.Next_Last := Last;
            Out_Last := Out_Item'First - 1;
         else
            Canonical_Composites.Initialize_C;
            Compose_No_Length_Check (State, Item, Last, Out_Item, Out_Last);
         end if;
      end Compose;

      procedure Compose (
         Item : String_Type;
         Out_Item : out String_Type;
         Out_Last : out Natural) is
      begin
         Out_Last := Out_Item'First - 1;
         if Item'Length > 0 then
            Canonical_Composites.Initialize_C;
            declare
               St : Composites.State;
               Last : Natural := Item'First - 1;
            begin
               Start (Item, St);
               while Last < Item'Last loop
                  Compose_No_Length_Check (
                     St,
                     Item (Last + 1 .. Item'Last),
                     Last,
                     Out_Item (Out_Last + 1 .. Out_Item'Last),
                     Out_Last);
               end loop;
            end;
         end if;
      end Compose;

      function Compose (Item : String_Type) return String_Type is
         Result : String_Type (1 .. Expanding * Max_Length * Item'Length);
         Result_Last : Natural := Result'First - 1;
      begin
         Compose (Item, Result, Result_Last);
         return Result (Result'First .. Result_Last);
      end Compose;

      function Equal (
         Left, Right : String_Type)
         return Boolean is
      begin
         return Equal (Left, Right, Standard_Equal'Access);
      end Equal;

      function Equal (
         Left, Right : String_Type;
         Equal_Combined : not null access function (
            Left, Right : Wide_Wide_String)
            return Boolean)
         return Boolean is
      begin
         if Left'Length = 0 then
            return Right'Length = 0;
         elsif Right'Length = 0 then
            return False;
         end if;
         Canonical_Composites.Initialize_D;
         declare
            Left_State : Composites.State;
            Left_Last : Natural := Left'First - 1;
            Right_State : Composites.State;
            Right_Last : Natural := Right'First - 1;
         begin
            Start (Left, Left_State);
            Start (Right, Right_State);
            loop
               --  get one combining character sequence
               declare
                  Left_First : constant Positive := Left_Last + 1;
                  Right_First : constant Positive := Right_Last + 1;
                  Left_Is_Illegal_Sequence : Boolean;
                  Right_Is_Illegal_Sequence : Boolean;
               begin
                  Get_Combined (
                     Left_State,
                     Left (Left_First .. Left'Last),
                     Left_Last,
                     Left_Is_Illegal_Sequence);
                  Get_Combined (
                     Right_State,
                     Right (Right_First .. Right'Last),
                     Right_Last,
                     Right_Is_Illegal_Sequence);
                  if not Left_Is_Illegal_Sequence then
                     if not Right_Is_Illegal_Sequence then
                        --  left and right are legal
                        declare
                           Left_Buffer : Wide_Wide_String (
                              1 ..
                              Expanding
                                 * Max_Length
                                 * (Left_Last - Left_First + 1));
                           Left_Buffer_Last : Natural;
                           Left_Decomposed : Boolean; -- ignore
                           Right_Buffer : Wide_Wide_String (
                              1 ..
                              Expanding
                                 * Max_Length
                                 * (Right_Last - Right_First + 1));
                           Right_Buffer_Last : Natural;
                           Right_Decomposed : Boolean; -- ignore
                        begin
                           Decode (
                              Left (Left_First .. Left_Last),
                              Left_Buffer,
                              Left_Buffer_Last);
                           D_Buff (
                              Left_Buffer,
                              Left_Buffer_Last,
                              Left_Decomposed);
                           Decode (
                              Right (Right_First .. Right_Last),
                              Right_Buffer,
                              Right_Buffer_Last);
                           D_Buff (
                              Right_Buffer,
                              Right_Buffer_Last,
                              Right_Decomposed);
                           if not Equal_Combined (
                              Left_Buffer (1 .. Left_Buffer_Last),
                              Right_Buffer (1 .. Right_Buffer_Last))
                           then
                              return False;
                           end if;
                        end;
                     else
                        --  left is legal, right is illegal
                        return False;
                     end if;
                  else
                     if not Right_Is_Illegal_Sequence then
                        --  left is illegal, right is legal
                        return False;
                     else
                        --  left and right are illegal
                        if Left (Left_First .. Left_Last) /=
                           Right (Right_First .. Right_Last)
                        then
                           return False;
                        end if;
                     end if;
                  end if;
               end;
               --  detect ends
               if Left_Last >= Left'Last then
                  return Right_Last >= Right'Last;
               elsif Right_Last >= Right'Last then
                  return False;
               end if;
            end loop;
         end;
      end Equal;

      function Less (
         Left, Right : String_Type)
         return Boolean is
      begin
         return Less (Left, Right, Standard_Less'Access);
      end Less;

      function Less (
         Left, Right : String_Type;
         Less_Combined : not null access function (
            Left, Right : Wide_Wide_String)
            return Boolean)
         return Boolean is
      begin
         if Left'Length = 0 then
            return Right'Length > 0;
         elsif Right'Length = 0 then
            return False;
         end if;
         Canonical_Composites.Initialize_D;
         declare
            Left_State : Composites.State;
            Left_Last : Natural := Left'First - 1;
            Right_State : Composites.State;
            Right_Last : Natural := Right'First - 1;
         begin
            Start (Left, Left_State);
            Start (Right, Right_State);
            loop
               --  get one combining character sequence
               declare
                  Left_First : constant Positive := Left_Last + 1;
                  Right_First : constant Positive := Right_Last + 1;
                  Left_Is_Illegal_Sequence : Boolean;
                  Right_Is_Illegal_Sequence : Boolean;
               begin
                  Get_Combined (
                     Left_State,
                     Left (Left_First .. Left'Last),
                     Left_Last,
                     Left_Is_Illegal_Sequence);
                  Get_Combined (
                     Right_State,
                     Right (Right_First .. Right'Last),
                     Right_Last,
                     Right_Is_Illegal_Sequence);
                  if not Left_Is_Illegal_Sequence then
                     if not Right_Is_Illegal_Sequence then
                        --  left and right are legal
                        declare
                           Left_Buffer : Wide_Wide_String (
                              1 ..
                              Expanding
                                 * Max_Length
                                 * (Left_Last - Left_First + 1));
                           Left_Buffer_Last : Natural;
                           Left_Decomposed : Boolean; -- ignore
                           Right_Buffer : Wide_Wide_String (
                              1 ..
                              Expanding
                                 * Max_Length
                                 * (Right_Last - Right_First + 1));
                           Right_Buffer_Last : Natural;
                           Right_Decomposed : Boolean; -- ignore
                        begin
                           Decode (
                              Left (Left_First .. Left_Last),
                              Left_Buffer,
                              Left_Buffer_Last);
                           D_Buff (
                              Left_Buffer,
                              Left_Buffer_Last,
                              Left_Decomposed);
                           Decode (
                              Right (Right_First .. Right_Last),
                              Right_Buffer,
                              Right_Buffer_Last);
                           D_Buff (
                              Right_Buffer,
                              Right_Buffer_Last,
                              Right_Decomposed);
                           if Less_Combined (
                              Left_Buffer (1 .. Left_Buffer_Last),
                              Right_Buffer (1 .. Right_Buffer_Last))
                           then
                              return True;
                           elsif Less_Combined (
                              Right_Buffer (1 .. Right_Buffer_Last),
                              Left_Buffer (1 .. Left_Buffer_Last))
                           then
                              return False;
                           end if;
                        end;
                     else
                        --  left is legal, right is illegal
                        return True;
                     end if;
                  else
                     if not Right_Is_Illegal_Sequence then
                        --  left is illegal, right is legal
                        return False;
                     else
                        --  left and right are illegal
                        if Left (Left_First .. Left_Last) <
                           Right (Right_First .. Right_Last)
                        then
                           return True;
                        elsif Left (Left_First .. Left_Last) <
                           Right (Right_First .. Right_Last)
                        then
                           return False;
                        end if;
                     end if;
                  end if;
               end;
               --  detect ends
               if Left_Last >= Left'Last then
                  return Right_Last < Right'Last;
               elsif Right_Last >= Right'Last then
                  return False;
               end if;
            end loop;
         end;
      end Less;

   end Generic_Normalization;

   package Strings is
      new Generic_Normalization (
         Character,
         String,
         Characters.Conversions.Max_Length_In_String,
         Characters.Conversions.Get,
         Characters.Conversions.Put,
         Composites.Start,
         Composites.Get_Combined);
   package Wide_Strings is
      new Generic_Normalization (
         Wide_Character,
         Wide_String,
         Characters.Conversions.Max_Length_In_Wide_String,
         Characters.Conversions.Get,
         Characters.Conversions.Put,
         Composites.Start,
         Composites.Get_Combined);
   package Wide_Wide_Strings is
      new Generic_Normalization (
         Wide_Wide_Character,
         Wide_Wide_String,
         Characters.Conversions.Max_Length_In_Wide_Wide_String, -- 1
         Characters.Conversions.Get,
         Characters.Conversions.Put,
         Composites.Start,
         Composites.Get_Combined);

   --  implementation

   procedure Iterate (
      Expanded : Boolean;
      Process : not null access procedure (
         Precomposed : Wide_Wide_Character;
         Decomposed : Wide_Wide_String))
   is
      procedure Do_Iterate (
         Map : Canonical_Composites.D_Map_Array;
         Process : not null access procedure (
            Precomposed : Wide_Wide_Character;
            Decomposed : Wide_Wide_String));
      procedure Do_Iterate (
         Map : Canonical_Composites.D_Map_Array;
         Process : not null access procedure (
            Precomposed : Wide_Wide_Character;
            Decomposed : Wide_Wide_String)) is
      begin
         for I in Map'Range loop
            declare
               E : Canonical_Composites.D_Map_Element renames Map (I);
               Decomposed_Length : constant Natural :=
                  Canonical_Composites.Decomposed_Length (E.To);
            begin
               Process (E.From, E.To (1 .. Decomposed_Length));
            end;
         end loop;
      end Do_Iterate;
   begin
      if Expanded then
         Canonical_Composites.Initialize_D;
         Do_Iterate (Canonical_Composites.D_Map.all, Process);
      else
         Canonical_Composites.Initialize_Unexpanded_D;
         Do_Iterate (Canonical_Composites.Unexpanded_D_Map.all, Process);
      end if;
   end Iterate;

   procedure Decompose (
      Item : String;
      Last : out Natural;
      Out_Item : out String;
      Out_Last : out Natural)
      renames Strings.Decompose;

   procedure Decompose (
      State : in out Composites.State;
      Item : String;
      Last : out Natural;
      Out_Item : out String;
      Out_Last : out Natural)
      renames Strings.Decompose;

   procedure Decompose (
      Item : Wide_String;
      Last : out Natural;
      Out_Item : out Wide_String;
      Out_Last : out Natural)
      renames Wide_Strings.Decompose;

   procedure Decompose (
      State : in out Composites.State;
      Item : Wide_String;
      Last : out Natural;
      Out_Item : out Wide_String;
      Out_Last : out Natural)
      renames Wide_Strings.Decompose;

   procedure Decompose (
      Item : Wide_Wide_String;
      Last : out Natural;
      Out_Item : out Wide_Wide_String;
      Out_Last : out Natural)
      renames Wide_Wide_Strings.Decompose;

   procedure Decompose (
      State : in out Composites.State;
      Item : Wide_Wide_String;
      Last : out Natural;
      Out_Item : out Wide_Wide_String;
      Out_Last : out Natural)
      renames Wide_Wide_Strings.Decompose;

   procedure Decompose (
      Item : String;
      Out_Item : out String;
      Out_Last : out Natural)
      renames Strings.Decompose;

   function Decompose (
      Item : String)
      return String
      renames Strings.Decompose;

   procedure Decompose (
      Item : Wide_String;
      Out_Item : out Wide_String;
      Out_Last : out Natural)
      renames Wide_Strings.Decompose;

   function Decompose (
      Item : Wide_String)
      return Wide_String
      renames Wide_Strings.Decompose;

   procedure Decompose (
      Item : Wide_Wide_String;
      Out_Item : out Wide_Wide_String;
      Out_Last : out Natural)
      renames Wide_Wide_Strings.Decompose;

   function Decompose (
      Item : Wide_Wide_String)
      return Wide_Wide_String
      renames Wide_Wide_Strings.Decompose;

   procedure Compose (
      Item : String;
      Last : out Natural;
      Out_Item : out String;
      Out_Last : out Natural)
      renames Strings.Compose;

   procedure Compose (
      State : in out Composites.State;
      Item : String;
      Last : out Natural;
      Out_Item : out String;
      Out_Last : out Natural)
      renames Strings.Compose;

   procedure Compose (
      Item : Wide_String;
      Last : out Natural;
      Out_Item : out Wide_String;
      Out_Last : out Natural)
      renames Wide_Strings.Compose;

   procedure Compose (
      State : in out Composites.State;
      Item : Wide_String;
      Last : out Natural;
      Out_Item : out Wide_String;
      Out_Last : out Natural)
      renames Wide_Strings.Compose;

   procedure Compose (
      Item : Wide_Wide_String;
      Last : out Natural;
      Out_Item : out Wide_Wide_String;
      Out_Last : out Natural)
      renames Wide_Wide_Strings.Compose;

   procedure Compose (
      State : in out Composites.State;
      Item : Wide_Wide_String;
      Last : out Natural;
      Out_Item : out Wide_Wide_String;
      Out_Last : out Natural)
      renames Wide_Wide_Strings.Compose;

   procedure Compose (
      Item : String;
      Out_Item : out String;
      Out_Last : out Natural)
      renames Strings.Compose;

   function Compose (
      Item : String)
      return String
      renames Strings.Compose;

   procedure Compose (
      Item : Wide_String;
      Out_Item : out Wide_String;
      Out_Last : out Natural)
      renames Wide_Strings.Compose;

   function Compose (
      Item : Wide_String)
      return Wide_String
      renames Wide_Strings.Compose;

   procedure Compose (
      Item : Wide_Wide_String;
      Out_Item : out Wide_Wide_String;
      Out_Last : out Natural)
      renames Wide_Wide_Strings.Compose;

   function Compose (
      Item : Wide_Wide_String)
      return Wide_Wide_String
      renames Wide_Wide_Strings.Compose;

   function Equal (
      Left, Right : String)
      return Boolean
      renames Strings.Equal;

   function Equal (
      Left, Right : String;
      Equal_Combined : not null access function (
         Left, Right : Wide_Wide_String)
         return Boolean)
      return Boolean
      renames Strings.Equal;

   function Equal (
      Left, Right : Wide_String)
      return Boolean
      renames Wide_Strings.Equal;

   function Equal (
      Left, Right : Wide_String;
      Equal_Combined : not null access function (
         Left, Right : Wide_Wide_String)
         return Boolean)
      return Boolean
      renames Wide_Strings.Equal;

   function Equal (
      Left, Right : Wide_Wide_String)
      return Boolean
      renames Wide_Wide_Strings.Equal;

   function Equal (
      Left, Right : Wide_Wide_String;
      Equal_Combined : not null access function (
         Left, Right : Wide_Wide_String)
         return Boolean)
      return Boolean
      renames Wide_Wide_Strings.Equal;

   function Less (
      Left, Right : String)
      return Boolean
      renames Strings.Less;

   function Less (
      Left, Right : String;
      Less_Combined : not null access function (
         Left, Right : Wide_Wide_String)
         return Boolean)
      return Boolean
      renames Strings.Less;

   function Less (
      Left, Right : Wide_String)
      return Boolean
      renames Wide_Strings.Less;

   function Less (
      Left, Right : Wide_String;
      Less_Combined : not null access function (
         Left, Right : Wide_Wide_String)
         return Boolean)
      return Boolean
      renames Wide_Strings.Less;

   function Less (
      Left, Right : Wide_Wide_String)
      return Boolean
      renames Wide_Wide_Strings.Less;

   function Less (
      Left, Right : Wide_Wide_String;
      Less_Combined : not null access function (
         Left, Right : Wide_Wide_String)
         return Boolean)
      return Boolean
      renames Wide_Wide_Strings.Less;

end Ada.Strings.Normalization;
