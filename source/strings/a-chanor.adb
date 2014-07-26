--  reference:
--  http://www.unicode.org/reports/tr15/
pragma Check_Policy (Validate, Off);
with Ada.UCD.Combining_Class;
with Ada.UCD.Normalization;
with System.Once;
with System.UTF_Conversions;
package body Ada.Characters.Normalization is
   use type UCD.UCS_4;
   use type System.UTF_Conversions.UCS_4;

   procedure unreachable;
   pragma No_Return (unreachable);
   pragma Import (Intrinsic, unreachable, "__builtin_unreachable");

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

   --  see http://www.unicode.org/reports/tr15/#Hangul
   package Hangle is
      SBase : constant := 16#AC00#;
      LBase : constant := 16#1100#;
      VBase : constant := 16#1161#;
      TBase : constant := 16#11A7#;
      LCount : constant := 19;
      VCount : constant := 21;
      TCount : constant := 28;
      NCount : constant := VCount * TCount; -- 588
      SCount : constant := LCount * NCount; -- 11172
   end Hangle;

   --  NFD

   subtype Decomposed_Wide_Wide_String is Wide_Wide_String (1 .. Expanding);

   function Decomposed_Length (Item : Decomposed_Wide_Wide_String)
      return Natural;
   function Decomposed_Length (Item : Decomposed_Wide_Wide_String)
      return Natural is
   begin
      for I in reverse Item'Range loop
         if Item (I) /= Wide_Wide_Character'Val (0) then
            return I;
         end if;
      end loop;
      pragma Check (Validate, False);
      unreachable;
   end Decomposed_Length;

   type D_Map_Element is record
      From : Wide_Wide_Character;
      To : Decomposed_Wide_Wide_String;
   end record;
   pragma Suppress_Initialization (D_Map_Element);

   type D_Map_Array is
      array (1 .. UCD.Normalization.NFD_Total) of D_Map_Element;
   pragma Suppress_Initialization (D_Map_Array);

   D_Map : access D_Map_Array;
   D_Flag : aliased System.Once.Flag := 0;

   function D_Find (Item : Wide_Wide_Character) return Natural;
   function D_Find (Item : Wide_Wide_Character) return Natural is
      L : Positive := D_Map'First;
      H : Natural := D_Map'Last;
   begin
      loop
         declare
            M : constant Positive := (L + H) / 2;
         begin
            if Item < D_Map (M).From then
               H := M - 1;
            elsif Item > D_Map (M).From then
               L := M + 1;
            else
               return M;
            end if;
         end;
         if L > H then
            return 0;
         end if;
      end loop;
   end D_Find;

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
            D : constant Natural := D_Find (Item (I));
            To : Decomposed_Wide_Wide_String;
            To_Length : Natural;
         begin
            if D > 0 then
               To := D_Map (D).To;
               To_Length := Decomposed_Length (To);
            elsif Wide_Wide_Character'Pos (Item (I)) in
               Hangle.SBase ..
               Hangle.SBase + Hangle.SCount - 1
            then
               --  S to LV[T]
               declare
                  SIndex : constant UCD.UCS_4 :=
                     Wide_Wide_Character'Pos (Item (I)) - Hangle.SBase;
                  L : constant UCD.UCS_4 :=
                     Hangle.LBase + SIndex / Hangle.NCount;
                  V : constant UCD.UCS_4 :=
                     Hangle.VBase + (SIndex rem Hangle.NCount) / Hangle.TCount;
                  T : constant UCD.UCS_4 :=
                     Hangle.TBase + SIndex rem Hangle.TCount;
               begin
                  To (1) := Wide_Wide_Character'Val (L);
                  To (2) := Wide_Wide_Character'Val (V);
                  To_Length := 2;
                  if T /= Hangle.TBase then
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

   procedure D_Init;
   procedure D_Init is
      procedure Fill (
         Map : in out D_Map_Array;
         I : in out Positive;
         Table : UCD.Map_16x1_Type);
      procedure Fill (
         Map : in out D_Map_Array;
         I : in out Positive;
         Table : UCD.Map_16x1_Type) is
      begin
         for J in Table'Range loop
            Map (I).From := Wide_Wide_Character'Val (Table (J).Code);
            Map (I).To (1) := Wide_Wide_Character'Val (Table (J).Mapping);
            for K in 2 .. Expanding loop
               Map (I).To (K) := Wide_Wide_Character'Val (0);
            end loop;
            I := I + 1;
         end loop;
      end Fill;
      procedure Fill (
         Map : in out D_Map_Array;
         I : in out Positive;
         Table : UCD.Map_16x2_Type);
      procedure Fill (
         Map : in out D_Map_Array;
         I : in out Positive;
         Table : UCD.Map_16x2_Type) is
      begin
         for J in Table'Range loop
            Map (I).From := Wide_Wide_Character'Val (Table (J).Code);
            for K in 1 .. 2 loop
               Map (I).To (K) :=
                  Wide_Wide_Character'Val (Table (J).Mapping (K));
            end loop;
            for K in 3 .. Expanding loop
               D_Map (I).To (K) := Wide_Wide_Character'Val (0);
            end loop;
            I := I + 1;
         end loop;
      end Fill;
      procedure Fill (
         Map : in out D_Map_Array;
         I : in out Positive;
         Table : UCD.Map_32x2_Type);
      procedure Fill (
         Map : in out D_Map_Array;
         I : in out Positive;
         Table : UCD.Map_32x2_Type) is
      begin
         for J in Table'Range loop
            Map (I).From := Wide_Wide_Character'Val (Table (J).Code);
            for K in 1 .. 2 loop
               Map (I).To (K) :=
                  Wide_Wide_Character'Val (Table (J).Mapping (K));
            end loop;
            for K in 3 .. Expanding loop
               D_Map (I).To (K) := Wide_Wide_Character'Val (0);
            end loop;
            I := I + 1;
         end loop;
      end Fill;
   begin
      --  make table
      D_Map := new D_Map_Array;
      declare
         I : Positive := D_Map'First;
      begin
         --  16#00C0# ..
         Fill (D_Map.all, I, UCD.Normalization.NFD_D_Table_XXXX);
         --  16#0340#
         Fill (D_Map.all, I, UCD.Normalization.NFD_S_Table_XXXX);
         --  16#0344# ..
         Fill (D_Map.all, I, UCD.Normalization.NFD_E_Table_XXXX);
         --  16#1109A# ..
         Fill (D_Map.all, I, UCD.Normalization.NFD_D_Table_XXXXXXXX);
         --  16#1D15E#
         Fill (D_Map.all, I, UCD.Normalization.NFD_E_Table_XXXXXXXX);
         pragma Check (Validate, I = D_Map'Last + 1);
      end;
      --  sort
      for I in D_Map'First + 1 .. D_Map'Last loop
         for J in reverse D_Map'First .. I - 1 loop
            exit when D_Map (J).From <= D_Map (J + 1).From;
            declare
               T : constant D_Map_Element := D_Map (J);
            begin
               D_Map (J) := D_Map (J + 1);
               D_Map (J + 1) := T;
            end;
         end loop;
      end loop;
      --  expanding re-decomposable
      loop
         declare
            Expanded : Boolean := False;
         begin
            for I in D_Map'Range loop
               declare
                  To : Decomposed_Wide_Wide_String
                     renames D_Map (I).To;
                  To_Last : Natural := Decomposed_Length (To);
                  J : Natural := To_Last;
               begin
                  while J >= To'First loop
                     declare
                        D : constant Natural := D_Find (To (J));
                     begin
                        if D > 0 then
                           Expanded := True;
                           declare
                              R_Length : constant Natural :=
                                 Decomposed_Length (D_Map (D).To);
                           begin
                              To (J + R_Length .. To_Last + R_Length - 1) :=
                                 To (J + 1 .. To_Last);
                              To (J .. J + R_Length - 1) :=
                                 D_Map (D).To (1 .. R_Length);
                              To_Last := To_Last + R_Length - 1;
                              pragma Check (Validate, To_Last <= Expanding);
                              J := J + R_Length - 1;
                           end;
                        else
                           J := J - 1;
                        end if;
                     end;
                  end loop;
               end;
            end loop;
            exit when not Expanded;
         end;
      end loop;
   end D_Init;

   --  NFC

   subtype Composing_Wide_Wide_String is Wide_Wide_String (1 .. 2);

   type C_Map_Element is record
      From : Composing_Wide_Wide_String;
      To : Wide_Wide_Character;
   end record;
   pragma Suppress_Initialization (C_Map_Element);

   type C_Map_Array is
      array (1 .. UCD.Normalization.NFC_Total) of C_Map_Element;
   pragma Suppress_Initialization (C_Map_Array);

   C_Map : access C_Map_Array;
   C_Flag : aliased System.Once.Flag := 0;

   function C_Find (Item : Composing_Wide_Wide_String) return Natural;
   function C_Find (Item : Composing_Wide_Wide_String) return Natural is
      L : Positive := C_Map'First;
      H : Natural := C_Map'Last;
   begin
      loop
         declare
            M : constant Positive := (L + H) / 2;
         begin
            if Item < C_Map (M).From then
               H := M - 1;
            elsif Item > C_Map (M).From then
               L := M + 1;
            else
               return M;
            end if;
         end;
         if L > H then
            return 0;
         end if;
      end loop;
   end C_Find;

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
               From : constant Composing_Wide_Wide_String :=
                  (Item (I), Item (I + 1));
               C : constant Natural := C_Find (From);
               To : Wide_Wide_Character;
            begin
               if C > 0 then
                  To := C_Map (C).To;
               elsif Wide_Wide_Character'Pos (From (1)) in
                  Hangle.LBase ..
                  Hangle.LBase + Hangle.LCount - 1
                  and then Wide_Wide_Character'Pos (From (2)) in
                     Hangle.VBase ..
                     Hangle.VBase + Hangle.VCount - 1
               then
                  --  LV to S
                  declare
                     LIndex : constant UCD.UCS_4 :=
                        Wide_Wide_Character'Pos (From (1))
                        - Hangle.LBase;
                     VIndex : constant UCD.UCS_4 :=
                        Wide_Wide_Character'Pos (From (2))
                        - Hangle.VBase;
                  begin
                     To := Wide_Wide_Character'Val (Hangle.SBase
                        + (LIndex * Hangle.VCount + VIndex) * Hangle.TCount);
                  end;
               elsif Wide_Wide_Character'Pos (From (1)) in
                  Hangle.SBase ..
                  Hangle.SBase + Hangle.SCount - 1
                  and then Wide_Wide_Character'Pos (From (2)) in
                     Hangle.TBase ..
                     Hangle.TBase + Hangle.TCount - 1
               then
                  --  ST to T
                  declare
                     ch : constant UCD.UCS_4 :=
                        Wide_Wide_Character'Pos (From (1));
                     TIndex : constant UCD.UCS_4 :=
                        Wide_Wide_Character'Pos (From (2)) - Hangle.TBase;
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

   procedure C_Init;
   procedure C_Init is
      procedure Fill (
         Map : in out C_Map_Array;
         I : in out Positive;
         Table : UCD.Map_16x2_Type);
      procedure Fill (
         Map : in out C_Map_Array;
         I : in out Positive;
         Table : UCD.Map_16x2_Type) is
      begin
         for J in Table'Range loop
            for K in 1 .. 2 loop
               Map (I).From (K) :=
                  Wide_Wide_Character'Val (Table (J).Mapping (K));
            end loop;
            Map (I).To := Wide_Wide_Character'Val (Table (J).Code);
            I := I + 1;
         end loop;
      end Fill;
      procedure Fill (
         Map : in out C_Map_Array;
         I : in out Positive;
         Table : UCD.Map_32x2_Type);
      procedure Fill (
         Map : in out C_Map_Array;
         I : in out Positive;
         Table : UCD.Map_32x2_Type) is
      begin
         for J in Table'Range loop
            for K in 1 .. 2 loop
               Map (I).From (K) :=
                  Wide_Wide_Character'Val (Table (J).Mapping (K));
            end loop;
            Map (I).To := Wide_Wide_Character'Val (Table (J).Code);
            I := I + 1;
         end loop;
      end Fill;
   begin
      --  initialize D table, too
      System.Once.Initialize (D_Flag'Access, D_Init'Access);
      --  make table
      C_Map := new C_Map_Array;
      declare
         I : Positive := C_Map'First;
      begin
         --  (16#0041#, 16#0300#) ..
         Fill (C_Map.all, I, UCD.Normalization.NFD_D_Table_XXXX);
         --  (16#11099#, 16#110BA#) ..
         Fill (C_Map.all, I, UCD.Normalization.NFD_D_Table_XXXXXXXX);
         pragma Check (Validate, I = C_Map'Last + 1);
      end;
      --  sort
      for I in C_Map'First + 1 .. C_Map'Last loop
         for J in reverse C_Map'First .. I - 1 loop
            exit when C_Map (J).From <= C_Map (J + 1).From;
            declare
               T : constant C_Map_Element := C_Map (J);
            begin
               C_Map (J) := C_Map (J + 1);
               C_Map (J + 1) := T;
            end;
         end loop;
      end loop;
   end C_Init;

   generic
      type Character_Type is (<>);
      type String_Type is array (Positive range <>) of Character_Type;
      Max_Length : Positive;
      with procedure From_UTF (
         Data : String_Type;
         Last : out Natural;
         Result : out System.UTF_Conversions.UCS_4;
         Status : out System.UTF_Conversions.From_Status_Type);
      with procedure To_UTF (
         Code : System.UTF_Conversions.UCS_4;
         Result : out String_Type;
         Last : out Natural;
         Status : out System.UTF_Conversions.To_Status_Type);
   package Generic_Normalization is

      procedure Start (Item : String_Type; State : out Normalization.State);

      procedure Get_Combined_No_Length_Check (
         State : in out Normalization.State;
         Item : String_Type;
         Last : out Natural);

      procedure Get_Combined (
         Item : String_Type;
         Last : out Natural);

      procedure Get_Combined (
         State : in out Normalization.State;
         Item : String_Type;
         Last : out Natural);

      procedure Decode (
         Item : String_Type;
         Buffer : out Wide_Wide_String;
         Buffer_Last : out Natural);
      procedure Encode (
         Item : Wide_Wide_String;
         Buffer : out String_Type;
         Buffer_Last : out Natural);

      procedure Decompose_No_Length_Check (
         State : in out Normalization.State;
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
         State : in out Normalization.State;
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
         State : in out Normalization.State;
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
         State : in out Normalization.State;
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

      procedure Start (Item : String_Type; State : out Normalization.State) is
         Code : System.UTF_Conversions.UCS_4;
         From_Status : System.UTF_Conversions.From_Status_Type; -- ignore
      begin
         if Item'Length = 0 then
            State.Next_Character := Wide_Wide_Character'Val (0);
            State.Next_Combining_Class := 0;
            State.Next_Last := Item'Last;
         else
            From_UTF (Item, State.Next_Last, Code, From_Status);
            State.Next_Character := Wide_Wide_Character'Val (Code);
            State.Next_Combining_Class :=
               Combining_Class (State.Next_Character);
         end if;
      end Start;

      procedure Get_Combined_No_Length_Check (
         State : in out Normalization.State;
         Item : String_Type;
         Last : out Natural)
      is
         Next : Natural;
         Code : System.UTF_Conversions.UCS_4;
         From_Status : System.UTF_Conversions.From_Status_Type; -- ignore
      begin
         Last := State.Next_Last; -- skip first code point
         if State.Next_Combining_Class = 0 then
            Code := Wide_Wide_Character'Pos (State.Next_Character);
            if Code in Hangle.LBase .. Hangle.LBase + Hangle.LCount - 1 then
               From_UTF (
                  Item (Last + 1 .. Item'Last),
                  Next,
                  Code,
                  From_Status);
               if Code in Hangle.VBase .. Hangle.VBase + Hangle.VCount - 1 then
                  Last := Next; -- LV
                  From_UTF (
                     Item (Last + 1 .. Item'Last),
                     Next,
                     Code,
                     From_Status);
                  if Code in
                     Hangle.TBase ..
                     Hangle.TBase + Hangle.TCount - 1
                  then
                     Last := Next; -- LVT
                  end if;
               end if;
            elsif Code in Hangle.SBase .. Hangle.SBase + Hangle.SCount - 1 then
               From_UTF (
                  Item (Last + 1 .. Item'Last),
                  Next,
                  Code,
                  From_Status);
               if Code in Hangle.TBase .. Hangle.TBase + Hangle.TCount - 1 then
                  Last := Next; -- ST
               end if;
            end if;
         end if;
         declare
            Current_Class : Class := State.Next_Combining_Class;
         begin
            State.Next_Character := Wide_Wide_Character'Val (0);
            State.Next_Combining_Class := 0;
            State.Next_Last := Last;
            while Last < Item'Last loop
               From_UTF (
                  Item (Last + 1 .. Item'Last),
                  Next,
                  Code,
                  From_Status);
               State.Next_Character := Wide_Wide_Character'Val (Code);
               State.Next_Combining_Class :=
                  Combining_Class (State.Next_Character);
               State.Next_Last := Next;
               if State.Next_Combining_Class < Current_Class then
                  exit;
               elsif State.Next_Combining_Class = 0 then
                  if Is_Variation_Selector (State.Next_Character) then
                     --  get one variation selector
                     Last := Next;
                     if Last >= Item'Last then
                        State.Next_Character := Wide_Wide_Character'Val (0);
                        State.Next_Combining_Class := 0;
                        State.Next_Last := Last;
                     else
                        From_UTF (
                           Item (Last + 1 .. Item'Last),
                           Next,
                           Code,
                           From_Status);
                        State.Next_Character := Wide_Wide_Character'Val (Code);
                        State.Next_Combining_Class :=
                           Combining_Class (State.Next_Character);
                        State.Next_Last := Next;
                     end if;
                  end if;
                  exit;
               end if;
               Current_Class := State.Next_Combining_Class;
               Last := Next;
            end loop;
         end;
      end Get_Combined_No_Length_Check;

      procedure Get_Combined (
         Item : String_Type;
         Last : out Natural) is
      begin
         if Item'Length = 0 then
            Last := Item'Last;
         else
            declare
               St : State;
            begin
               Start (Item, St);
               Get_Combined_No_Length_Check (St, Item, Last);
            end;
         end if;
      end Get_Combined;

      procedure Get_Combined (
         State : in out Normalization.State;
         Item : String_Type;
         Last : out Natural) is
      begin
         if Item'Length = 0 then
            Last := Item'Last;
            State.Next_Character := Wide_Wide_Character'Val (0);
            State.Next_Combining_Class := 0;
            State.Next_Last := Last;
         else
            Get_Combined_No_Length_Check (State, Item, Last);
         end if;
      end Get_Combined;

      procedure Decode (
         Item : String_Type;
         Buffer : out Wide_Wide_String;
         Buffer_Last : out Natural)
      is
         Last : Natural := Item'First - 1;
         Code : System.UTF_Conversions.UCS_4;
         From_Status : System.UTF_Conversions.From_Status_Type; -- ignore
      begin
         Buffer_Last := Buffer'First - 1;
         while Last < Item'Last loop
            From_UTF (Item (Last + 1 .. Item'Last), Last, Code, From_Status);
            Buffer_Last := Buffer_Last + 1;
            Buffer (Buffer_Last) := Wide_Wide_Character'Val (Code);
         end loop;
      end Decode;

      procedure Encode (
         Item : Wide_Wide_String;
         Buffer : out String_Type;
         Buffer_Last : out Natural)
      is
         To_Status : System.UTF_Conversions.To_Status_Type; -- ignore
      begin
         Buffer_Last := Buffer'First - 1;
         for I in Item'Range loop
            To_UTF (
               Wide_Wide_Character'Pos (Item (I)),
               Buffer (Buffer_Last + 1 .. Buffer'Last),
               Buffer_Last,
               To_Status);
         end loop;
      end Encode;

      procedure Decompose_No_Length_Check (
         State : in out Normalization.State;
         Item : String_Type;
         Last : out Natural;
         Out_Item : out String_Type;
         Out_Last : out Natural) is
      begin
         --  get one combining character sequence
         Get_Combined_No_Length_Check (State, Item, Last);
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
            else
               Out_Last := Out_Item'First + (Last - Item'First);
               Out_Item (Out_Item'First .. Out_Last) :=
                  Item (Item'First .. Last);
            end if;
         end;
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
            System.Once.Initialize (D_Flag'Access, D_Init'Access);
            declare
               St : State;
            begin
               Start (Item, St);
               Decompose_No_Length_Check (St, Item, Last, Out_Item, Out_Last);
            end;
         end if;
      end Decompose;

      procedure Decompose (
         State : in out Normalization.State;
         Item : String_Type;
         Last : out Natural;
         Out_Item : out String_Type;
         Out_Last : out Natural) is
      begin
         if Item'Length = 0 then
            Last := Item'Last;
            State.Next_Character := Wide_Wide_Character'Val (0);
            State.Next_Combining_Class := 0;
            State.Next_Last := Last;
            Out_Last := Out_Item'First - 1;
         else
            System.Once.Initialize (D_Flag'Access, D_Init'Access);
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
            System.Once.Initialize (D_Flag'Access, D_Init'Access);
            declare
               St : State;
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
         State : in out Normalization.State;
         Item : String_Type;
         Last : out Natural;
         Out_Item : out String_Type;
         Out_Last : out Natural) is
      begin
         --  get one combining character sequence
         Get_Combined_No_Length_Check (State, Item, Last);
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
            else
               Out_Last := Out_Item'First + (Last - Item'First);
               Out_Item (Out_Item'First .. Out_Last) :=
                  Item (Item'First .. Last);
            end if;
         end;
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
            System.Once.Initialize (C_Flag'Access, C_Init'Access);
            declare
               St : State;
            begin
               Start (Item, St);
               Compose_No_Length_Check (St, Item, Last, Out_Item, Out_Last);
            end;
         end if;
      end Compose;

      procedure Compose (
         State : in out Normalization.State;
         Item : String_Type;
         Last : out Natural;
         Out_Item : out String_Type;
         Out_Last : out Natural) is
      begin
         if Item'Length = 0 then
            Last := Item'Last;
            State.Next_Character := Wide_Wide_Character'Val (0);
            State.Next_Combining_Class := 0;
            State.Next_Last := Last;
            Out_Last := Out_Item'First - 1;
         else
            System.Once.Initialize (C_Flag'Access, C_Init'Access);
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
            System.Once.Initialize (C_Flag'Access, C_Init'Access);
            declare
               St : State;
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
         System.Once.Initialize (D_Flag'Access, D_Init'Access);
         declare
            Left_State : State;
            Left_Last : Natural := Left'First - 1;
            Right_State : State;
            Right_Last : Natural := Right'First - 1;
         begin
            Start (Left, Left_State);
            Start (Right, Right_State);
            loop
               --  get one combining character sequence
               declare
                  Left_First : constant Positive := Left_Last + 1;
                  Right_First : constant Positive := Right_Last + 1;
               begin
                  Get_Combined (
                     Left_State,
                     Left (Left_First .. Left'Last),
                     Left_Last);
                  Get_Combined (
                     Right_State,
                     Right (Right_First .. Right'Last),
                     Right_Last);
                  declare
                     Left_Buffer : Wide_Wide_String (
                        1 ..
                        Expanding * Max_Length * (Left_Last - Left_First + 1));
                     Left_Buffer_Last : Natural;
                     Right_Buffer : Wide_Wide_String (
                        1 ..
                        Expanding * Max_Length
                           * (Right_Last - Right_First + 1));
                     Right_Buffer_Last : Natural;
                  begin
                     Decode (
                        Left (Left_First .. Left_Last),
                        Left_Buffer,
                        Left_Buffer_Last);
                     Decode (
                        Right (Right_First .. Right_Last),
                        Right_Buffer,
                        Right_Buffer_Last);
                     if not Equal_Combined (
                        Left_Buffer (1 .. Left_Buffer_Last),
                        Right_Buffer (1 .. Right_Buffer_Last))
                     then
                        return False;
                     end if;
                  end;
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
         System.Once.Initialize (D_Flag'Access, D_Init'Access);
         declare
            Left_State : State;
            Left_Last : Natural := Left'First - 1;
            Right_State : State;
            Right_Last : Natural := Right'First - 1;
         begin
            Start (Left, Left_State);
            Start (Right, Right_State);
            loop
               --  get one combining character sequence
               declare
                  Left_First : constant Positive := Left_Last + 1;
                  Right_First : constant Positive := Right_Last + 1;
               begin
                  Get_Combined (
                     Left_State,
                     Left (Left_First .. Left'Last),
                     Left_Last);
                  Get_Combined (
                     Right_State,
                     Right (Right_First .. Right'Last),
                     Right_Last);
                  declare
                     Left_Buffer : Wide_Wide_String (
                        1 ..
                        Expanding * Max_Length * (Left_Last - Left_First + 1));
                     Left_Buffer_Last : Natural;
                     Right_Buffer : Wide_Wide_String (
                        1 ..
                        Expanding * Max_Length
                           * (Right_Last - Right_First + 1));
                     Right_Buffer_Last : Natural;
                  begin
                     Decode (
                        Left (Left_First .. Left_Last),
                        Left_Buffer,
                        Left_Buffer_Last);
                     Decode (
                        Right (Right_First .. Right_Last),
                        Right_Buffer,
                        Right_Buffer_Last);
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

   package UTF_8 is
      new Generic_Normalization (
         Character,
         String,
         System.UTF_Conversions.UTF_8_Max_Length,
         System.UTF_Conversions.From_UTF_8,
         System.UTF_Conversions.To_UTF_8);
   package UTF_16 is
      new Generic_Normalization (
         Wide_Character,
         Wide_String,
         System.UTF_Conversions.UTF_16_Max_Length,
         System.UTF_Conversions.From_UTF_16,
         System.UTF_Conversions.To_UTF_16);
   package UTF_32 is
      new Generic_Normalization (
         Wide_Wide_Character,
         Wide_Wide_String,
         1,
         System.UTF_Conversions.From_UTF_32,
         System.UTF_Conversions.To_UTF_32);

   --  implementation

   function Combining_Class (Item : Wide_Wide_Character) return Class is
      Code : UCD.UCS_4 := Wide_Wide_Character'Pos (Item);
   begin
      if Code > 16#ffff# then
         declare
            L : Positive := UCD.Combining_Class.Table_1XXXX'First;
            H : Natural := UCD.Combining_Class.Table_1XXXX'Last;
         begin
            Code := Code - 16#10000#;
            loop
               declare
                  M : constant Positive := (L + H) / 2;
               begin
                  if Code < UCD.Combining_Class.Table_1XXXX (M).Start then
                     H := M - 1;
                  elsif Code >= UCD.Combining_Class.Table_1XXXX (M).Start
                     + UCD.UCS_4 (UCD.Combining_Class.Table_1XXXX (M).Length)
                  then
                     L := M + 1;
                  else
                     return Class (
                        UCD.Combining_Class.Table_1XXXX (M).Combining_Class);
                  end if;
               end;
               if L > H then
                  return 0;
               end if;
            end loop;
         end;
      else
         declare
            L : Positive := UCD.Combining_Class.Table_XXXX'First;
            H : Natural := UCD.Combining_Class.Table_XXXX'Last;
         begin
            loop
               declare
                  M : constant Positive := (L + H) / 2;
               begin
                  if Code < UCD.Combining_Class.Table_XXXX (M).Start then
                     H := M - 1;
                  elsif Code >= UCD.Combining_Class.Table_XXXX (M).Start
                     + UCD.UCS_4 (UCD.Combining_Class.Table_XXXX (M).Length)
                  then
                     L := M + 1;
                  else
                     return Class (
                        UCD.Combining_Class.Table_XXXX (M).Combining_Class);
                  end if;
               end;
               if L > H then
                  return 0;
               end if;
            end loop;
         end;
      end if;
   end Combining_Class;

   function Is_Variation_Selector (Item : Wide_Wide_Character)
      return Boolean is
   begin
      case Wide_Wide_Character'Pos (Item) is
         when 16#180B# .. 16#180D# -- MONGOLIAN FREE VARIATION SELECTOR (1..3)
            | 16#FE00# .. 16#FE0F# -- VARIATION SELECTOR (1..16)
            | 16#E0100# .. 16#E01EF# -- VARIATION SELECTOR (17..256)
         =>
            return True;
         when others =>
            return False;
      end case;
   end Is_Variation_Selector;

   procedure Start (Item : String; State : out Normalization.State)
      renames UTF_8.Start;

   procedure Start (Item : Wide_String; State : out Normalization.State)
      renames UTF_16.Start;

   procedure Start (Item : Wide_Wide_String; State : out Normalization.State)
      renames UTF_32.Start;

   procedure Get_Combined (
      Item : String;
      Last : out Natural)
      renames UTF_8.Get_Combined;

   procedure Get_Combined (
      State : in out Normalization.State;
      Item : String;
      Last : out Natural)
      renames UTF_8.Get_Combined;

   procedure Get_Combined (
      Item : Wide_String;
      Last : out Natural)
      renames UTF_16.Get_Combined;

   procedure Get_Combined (
      State : in out Normalization.State;
      Item : Wide_String;
      Last : out Natural)
      renames UTF_16.Get_Combined;

   procedure Get_Combined (
      Item : Wide_Wide_String;
      Last : out Natural)
      renames UTF_32.Get_Combined;

   procedure Get_Combined (
      State : in out Normalization.State;
      Item : Wide_Wide_String;
      Last : out Natural)
      renames UTF_32.Get_Combined;

   procedure Decompose (
      Item : String;
      Last : out Natural;
      Out_Item : out String;
      Out_Last : out Natural)
      renames UTF_8.Decompose;

   procedure Decompose (
      State : in out Normalization.State;
      Item : String;
      Last : out Natural;
      Out_Item : out String;
      Out_Last : out Natural)
      renames UTF_8.Decompose;

   procedure Decompose (
      Item : Wide_String;
      Last : out Natural;
      Out_Item : out Wide_String;
      Out_Last : out Natural)
      renames UTF_16.Decompose;

   procedure Decompose (
      State : in out Normalization.State;
      Item : Wide_String;
      Last : out Natural;
      Out_Item : out Wide_String;
      Out_Last : out Natural)
      renames UTF_16.Decompose;

   procedure Decompose (
      Item : Wide_Wide_String;
      Last : out Natural;
      Out_Item : out Wide_Wide_String;
      Out_Last : out Natural)
      renames UTF_32.Decompose;

   procedure Decompose (
      State : in out Normalization.State;
      Item : Wide_Wide_String;
      Last : out Natural;
      Out_Item : out Wide_Wide_String;
      Out_Last : out Natural)
      renames UTF_32.Decompose;

   procedure Decompose (
      Item : String;
      Out_Item : out String;
      Out_Last : out Natural)
      renames UTF_8.Decompose;

   function Decompose (
      Item : String)
      return String
      renames UTF_8.Decompose;

   procedure Decompose (
      Item : Wide_String;
      Out_Item : out Wide_String;
      Out_Last : out Natural)
      renames UTF_16.Decompose;

   function Decompose (
      Item : Wide_String)
      return Wide_String
      renames UTF_16.Decompose;

   procedure Decompose (
      Item : Wide_Wide_String;
      Out_Item : out Wide_Wide_String;
      Out_Last : out Natural)
      renames UTF_32.Decompose;

   function Decompose (
      Item : Wide_Wide_String)
      return Wide_Wide_String
      renames UTF_32.Decompose;

   procedure Compose (
      Item : String;
      Last : out Natural;
      Out_Item : out String;
      Out_Last : out Natural)
      renames UTF_8.Compose;

   procedure Compose (
      State : in out Normalization.State;
      Item : String;
      Last : out Natural;
      Out_Item : out String;
      Out_Last : out Natural)
      renames UTF_8.Compose;

   procedure Compose (
      Item : Wide_String;
      Last : out Natural;
      Out_Item : out Wide_String;
      Out_Last : out Natural)
      renames UTF_16.Compose;

   procedure Compose (
      State : in out Normalization.State;
      Item : Wide_String;
      Last : out Natural;
      Out_Item : out Wide_String;
      Out_Last : out Natural)
      renames UTF_16.Compose;

   procedure Compose (
      Item : Wide_Wide_String;
      Last : out Natural;
      Out_Item : out Wide_Wide_String;
      Out_Last : out Natural)
      renames UTF_32.Compose;

   procedure Compose (
      State : in out Normalization.State;
      Item : Wide_Wide_String;
      Last : out Natural;
      Out_Item : out Wide_Wide_String;
      Out_Last : out Natural)
      renames UTF_32.Compose;

   procedure Compose (
      Item : String;
      Out_Item : out String;
      Out_Last : out Natural)
      renames UTF_8.Compose;

   function Compose (
      Item : String)
      return String
      renames UTF_8.Compose;

   procedure Compose (
      Item : Wide_String;
      Out_Item : out Wide_String;
      Out_Last : out Natural)
      renames UTF_16.Compose;

   function Compose (
      Item : Wide_String)
      return Wide_String
      renames UTF_16.Compose;

   procedure Compose (
      Item : Wide_Wide_String;
      Out_Item : out Wide_Wide_String;
      Out_Last : out Natural)
      renames UTF_32.Compose;

   function Compose (
      Item : Wide_Wide_String)
      return Wide_Wide_String
      renames UTF_32.Compose;

   function Equal (
      Left, Right : String)
      return Boolean
      renames UTF_8.Equal;

   function Equal (
      Left, Right : String;
      Equal_Combined : not null access function (
         Left, Right : Wide_Wide_String)
         return Boolean)
      return Boolean
      renames UTF_8.Equal;

   function Equal (
      Left, Right : Wide_String)
      return Boolean
      renames UTF_16.Equal;

   function Equal (
      Left, Right : Wide_String;
      Equal_Combined : not null access function (
         Left, Right : Wide_Wide_String)
         return Boolean)
      return Boolean
      renames UTF_16.Equal;

   function Equal (
      Left, Right : Wide_Wide_String)
      return Boolean
      renames UTF_32.Equal;

   function Equal (
      Left, Right : Wide_Wide_String;
      Equal_Combined : not null access function (
         Left, Right : Wide_Wide_String)
         return Boolean)
      return Boolean
      renames UTF_32.Equal;

   function Less (
      Left, Right : String)
      return Boolean
      renames UTF_8.Less;

   function Less (
      Left, Right : String;
      Less_Combined : not null access function (
         Left, Right : Wide_Wide_String)
         return Boolean)
      return Boolean
      renames UTF_8.Less;

   function Less (
      Left, Right : Wide_String)
      return Boolean
      renames UTF_16.Less;

   function Less (
      Left, Right : Wide_String;
      Less_Combined : not null access function (
         Left, Right : Wide_Wide_String)
         return Boolean)
      return Boolean
      renames UTF_16.Less;

   function Less (
      Left, Right : Wide_Wide_String)
      return Boolean
      renames UTF_32.Less;

   function Less (
      Left, Right : Wide_Wide_String;
      Less_Combined : not null access function (
         Left, Right : Wide_Wide_String)
         return Boolean)
      return Boolean
      renames UTF_32.Less;

end Ada.Characters.Normalization;
