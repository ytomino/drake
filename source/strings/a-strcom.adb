with Ada.Characters.Conversions;
with Ada.UCD.Combining_Class;
package body Ada.Strings.Composites is
   use type UCD.UCS_4;

   type Long_Boolean is new Boolean;
   for Long_Boolean'Size use Long_Integer'Size;

   function expect (exp, c : Long_Boolean) return Long_Boolean
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_expect";

   function Search (
      Table : UCD.Combining_Class.Table_16_Type;
      Code : UCD.UCS_4)
      return UCD.Combining_Class_Type;
   function Search (
      Table : UCD.Combining_Class.Table_16_Type;
      Code : UCD.UCS_4)
      return UCD.Combining_Class_Type
   is
      L : Positive := Table'First;
      H : Natural := Table'Last;
   begin
      loop
         declare
            type Unsigned is mod 2 ** Integer'Size;
            M : constant Positive := Integer (Unsigned (L + H) / 2);
            M_Item : UCD.Combining_Class.Table_16_Item_Type
               renames Table (M);
         begin
            if Code < M_Item.Start then
               H := M - 1;
            elsif expect (
               Long_Boolean (Code >= M_Item.Start + UCD.UCS_4 (M_Item.Length)),
               True)
            then
               L := M + 1;
            else
               return M_Item.Combining_Class;
            end if;
         end;
         exit when L > H;
      end loop;
      return 0;
   end Search;

   generic
      type Character_Type is (<>);
      type String_Type is array (Positive range <>) of Character_Type;
      with procedure Get (
         Data : String_Type;
         Last : out Natural;
         Result : out Wide_Wide_Character;
         Is_Illegal_Sequence : out Boolean);
   package Generic_Composites is

      procedure Start_No_Length_Check (
         Item : String_Type;
         State : out Composites.State);

      procedure Get_Combined_No_Length_Check (
         State : in out Composites.State;
         Item : String_Type;
         Last : out Natural;
         Is_Illegal_Sequence : out Boolean);

   end Generic_Composites;

   package body Generic_Composites is

      procedure Start_No_Length_Check (
         Item : String_Type;
         State : out Composites.State) is
      begin
         Get (
            Item,
            State.Next_Last,
            State.Next_Character,
            State.Next_Is_Illegal_Sequence);
         if State.Next_Is_Illegal_Sequence then
            State.Next_Combining_Class := 0;
         else
            State.Next_Combining_Class :=
               Combining_Class (State.Next_Character);
         end if;
      end Start_No_Length_Check;

      procedure Get_Combined_No_Length_Check (
         State : in out Composites.State;
         Item : String_Type;
         Last : out Natural;
         Is_Illegal_Sequence : out Boolean) is
      begin
         Last := State.Next_Last; -- skip first code point
         Is_Illegal_Sequence := State.Next_Is_Illegal_Sequence;
         if not Is_Illegal_Sequence -- combining class of illegal sequence = 0
            and then State.Next_Combining_Class = 0
         then
            declare
               New_Last : Natural;
               Combining_Code : Wide_Wide_Character;
               Combining_Is_Illegal_Sequence : Boolean;
            begin
               if Wide_Wide_Character'Pos (State.Next_Character) in
                  UCD.Hangul.LBase ..
                  UCD.Hangul.LBase + UCD.Hangul.LCount - 1
               then
                  Get (
                     Item (Last + 1 .. Item'Last),
                     New_Last,
                     Combining_Code,
                     Combining_Is_Illegal_Sequence);
                  if not Combining_Is_Illegal_Sequence
                     and then Wide_Wide_Character'Pos (Combining_Code) in
                        UCD.Hangul.VBase ..
                        UCD.Hangul.VBase + UCD.Hangul.VCount - 1
                  then
                     Last := New_Last; -- LV
                     Get (
                        Item (Last + 1 .. Item'Last),
                        New_Last,
                        Combining_Code,
                        Combining_Is_Illegal_Sequence);
                     if not Combining_Is_Illegal_Sequence
                        and then Wide_Wide_Character'Pos (Combining_Code) in
                           UCD.Hangul.TBase ..
                           UCD.Hangul.TBase + UCD.Hangul.TCount - 1
                     then
                        Last := New_Last; -- LVT
                     end if;
                  end if;
               elsif Wide_Wide_Character'Pos (State.Next_Character) in
                  UCD.Hangul.SBase ..
                  UCD.Hangul.SBase + UCD.Hangul.SCount - 1
               then
                  Get (
                     Item (Last + 1 .. Item'Last),
                     New_Last,
                     Combining_Code,
                     Combining_Is_Illegal_Sequence);
                  if not Combining_Is_Illegal_Sequence
                     and then Wide_Wide_Character'Pos (Combining_Code) in
                        UCD.Hangul.TBase ..
                        UCD.Hangul.TBase + UCD.Hangul.TCount - 1
                  then
                     Last := New_Last; -- ST
                  end if;
               end if;
            end;
         end if;
         declare
            Current_Class : Class := State.Next_Combining_Class;
         begin
            State.Next_Character := Wide_Wide_Character'Val (0);
            State.Next_Combining_Class := 0;
            State.Next_Is_Illegal_Sequence := False;
            State.Next_Last := Last;
            while Last < Item'Last loop
               Start_No_Length_Check (Item (Last + 1 .. Item'Last), State);
               if State.Next_Is_Illegal_Sequence
                  or else State.Next_Combining_Class < Current_Class
               then
                  exit;
               elsif State.Next_Combining_Class = 0 then
                  if Is_Variation_Selector (State.Next_Character) then
                     --  get one variation selector
                     Last := State.Next_Last;
                     if Last >= Item'Last then
                        State.Next_Character := Wide_Wide_Character'Val (0);
                        State.Next_Combining_Class := 0;
                        State.Next_Is_Illegal_Sequence := False;
                     else
                        Start_No_Length_Check (
                           Item (Last + 1 .. Item'Last),
                           State);
                     end if;
                  end if;
                  exit;
               end if;
               Current_Class := State.Next_Combining_Class;
               Last := State.Next_Last;
            end loop;
         end;
      end Get_Combined_No_Length_Check;

   end Generic_Composites;

   generic
      type Character_Type is (<>);
      type String_Type is array (Positive range <>) of Character_Type;
      with procedure Start_No_Length_Check (
         Item : String_Type;
         State : out Composites.State);
   procedure Generic_Start (
      Item : String_Type;
      State : out Composites.State);

   procedure Generic_Start (
      Item : String_Type;
      State : out Composites.State) is
   begin
      if Item'Length = 0 then
         State.Next_Character := Wide_Wide_Character'Val (0);
         State.Next_Combining_Class := 0;
         State.Next_Last := Item'Last;
      else
         Start_No_Length_Check (Item, State);
      end if;
   end Generic_Start;

   generic
      type Character_Type is (<>);
      type String_Type is array (Positive range <>) of Character_Type;
      with package C is
         new Generic_Composites (Character_Type, String_Type, others => <>);
   procedure Generic_Get_Combined (
      Item : String_Type;
      Last : out Natural;
      Is_Illegal_Sequence : out Boolean);

   procedure Generic_Get_Combined (
      Item : String_Type;
      Last : out Natural;
      Is_Illegal_Sequence : out Boolean) is
   begin
      if Item'Length = 0 then
         --  finished
         Last := Item'Last;
         Is_Illegal_Sequence := True; -- ??
      else
         declare
            St : State;
         begin
            C.Start_No_Length_Check (Item, St);
            C.Get_Combined_No_Length_Check (
               St,
               Item,
               Last,
               Is_Illegal_Sequence);
         end;
      end if;
   end Generic_Get_Combined;

   generic
      type Character_Type is (<>);
      type String_Type is array (Positive range <>) of Character_Type;
      with package C is
         new Generic_Composites (Character_Type, String_Type, others => <>);
   procedure Generic_Get_Combined_With_State (
      State : in out Composites.State;
      Item : String_Type;
      Last : out Natural;
      Is_Illegal_Sequence : out Boolean);

   procedure Generic_Get_Combined_With_State (
      State : in out Composites.State;
      Item : String_Type;
      Last : out Natural;
      Is_Illegal_Sequence : out Boolean) is
   begin
      if Item'Length = 0 then
         --  finished
         Last := Item'Last;
         Is_Illegal_Sequence := True; -- ??
         State.Next_Character := Wide_Wide_Character'Val (0);
         State.Next_Combining_Class := 0;
         State.Next_Is_Illegal_Sequence := False;
         State.Next_Last := Last;
      else
         C.Get_Combined_No_Length_Check (
            State,
            Item,
            Last,
            Is_Illegal_Sequence);
      end if;
   end Generic_Get_Combined_With_State;

   package Strings is
      new Generic_Composites (
         Character,
         String,
         Characters.Conversions.Get);
   package Wide_Strings is
      new Generic_Composites (
         Wide_Character,
         Wide_String,
         Characters.Conversions.Get);
   package Wide_Wide_Strings is
      new Generic_Composites (
         Wide_Wide_Character,
         Wide_Wide_String,
         Characters.Conversions.Get);

   --  implementation

   function Combining_Class (Item : Wide_Wide_Character) return Class is
      Code : constant UCD.UCS_4 := Wide_Wide_Character'Pos (Item);
   begin
      if Code > 16#ffff# then
         return Class (
            Search (UCD.Combining_Class.Table_1XXXX, Code - 16#10000#));
      else
         return Class (Search (UCD.Combining_Class.Table_XXXX, Code));
      end if;
   end Combining_Class;

   procedure Iterate (
      Process : not null access procedure (
         Item : Wide_Wide_Character;
         Combining_Class : Class)) is
   begin
      for I in UCD.Combining_Class.Table_XXXX'Range loop
         declare
            E : UCD.Combining_Class.Table_16_Item_Type
               renames UCD.Combining_Class.Table_XXXX (I);
         begin
            for J in E.Start .. E.Start + UCD.UCS_4 (E.Length) - 1 loop
               Process (
                  Wide_Wide_Character'Val (J),
                  Class (E.Combining_Class));
            end loop;
         end;
      end loop;
      for I in UCD.Combining_Class.Table_1XXXX'Range loop
         declare
            E : UCD.Combining_Class.Table_16_Item_Type
               renames UCD.Combining_Class.Table_1XXXX (I);
         begin
            for J in E.Start .. E.Start + UCD.UCS_4 (E.Length) - 1 loop
               Process (
                  Wide_Wide_Character'Val (J + 16#10000#),
                  Class (E.Combining_Class));
            end loop;
         end;
      end loop;
   end Iterate;

   function Is_Variation_Selector (Item : Wide_Wide_Character)
      return Boolean
   is
      subtype WWC is Wide_Wide_Character; -- for the case statement
   begin
      case Item is
         when WWC'Val (16#180B#) .. WWC'Val (16#180D#)
               --  MONGOLIAN FREE VARIATION SELECTOR (1..3)
            | WWC'Val (16#FE00#) .. WWC'Val (16#FE0F#)
               --  VARIATION SELECTOR (1..16)
            | WWC'Val (16#E0100#) .. WWC'Val (16#E01EF#) =>
               --  VARIATION SELECTOR (17..256)
            return True;
         when others =>
            return False;
      end case;
   end Is_Variation_Selector;

   procedure Start (Item : String; State : out Composites.State) is
      procedure Start_String is
         new Generic_Start (Character, String, Strings.Start_No_Length_Check);
   begin
      Start_String (Item, State);
   end Start;

   procedure Start (Item : Wide_String; State : out Composites.State) is
      procedure Start_Wide_String is
         new Generic_Start (
            Wide_Character,
            Wide_String,
            Wide_Strings.Start_No_Length_Check);
   begin
      Start_Wide_String (Item, State);
   end Start;

   procedure Start (Item : Wide_Wide_String; State : out Composites.State) is
      procedure Start_Wide_Wide_String is
         new Generic_Start (
            Wide_Wide_Character,
            Wide_Wide_String,
            Wide_Wide_Strings.Start_No_Length_Check);
   begin
      Start_Wide_Wide_String (Item, State);
   end Start;

   procedure Get_Combined (
      Item : String;
      Last : out Natural;
      Is_Illegal_Sequence : out Boolean)
   is
      procedure Get_Combined_String is
         new Generic_Get_Combined (Character, String, Strings);
   begin
      Get_Combined_String (Item, Last, Is_Illegal_Sequence);
   end Get_Combined;

   procedure Get_Combined (
      State : in out Composites.State;
      Item : String;
      Last : out Natural;
      Is_Illegal_Sequence : out Boolean)
   is
      procedure Get_Combined_String is
         new Generic_Get_Combined_With_State (Character, String, Strings);
   begin
      Get_Combined_String (State, Item, Last, Is_Illegal_Sequence);
   end Get_Combined;

   procedure Get_Combined (
      Item : Wide_String;
      Last : out Natural;
      Is_Illegal_Sequence : out Boolean)
   is
      procedure Get_Combined_Wide_String is
         new Generic_Get_Combined (Wide_Character, Wide_String, Wide_Strings);
   begin
      Get_Combined_Wide_String (Item, Last, Is_Illegal_Sequence);
   end Get_Combined;

   procedure Get_Combined (
      State : in out Composites.State;
      Item : Wide_String;
      Last : out Natural;
      Is_Illegal_Sequence : out Boolean)
   is
      procedure Get_Combined_Wide_String is
         new Generic_Get_Combined_With_State (
            Wide_Character,
            Wide_String,
            Wide_Strings);
   begin
      Get_Combined_Wide_String (State, Item, Last, Is_Illegal_Sequence);
   end Get_Combined;

   procedure Get_Combined (
      Item : Wide_Wide_String;
      Last : out Natural;
      Is_Illegal_Sequence : out Boolean)
   is
      procedure Get_Combined_Wide_Wide_String is
         new Generic_Get_Combined (
            Wide_Wide_Character,
            Wide_Wide_String,
            Wide_Wide_Strings);
   begin
      Get_Combined_Wide_Wide_String (Item, Last, Is_Illegal_Sequence);
   end Get_Combined;

   procedure Get_Combined (
      State : in out Composites.State;
      Item : Wide_Wide_String;
      Last : out Natural;
      Is_Illegal_Sequence : out Boolean)
   is
      procedure Get_Combined_Wide_Wide_String is
         new Generic_Get_Combined_With_State (
            Wide_Wide_Character,
            Wide_Wide_String,
            Wide_Wide_Strings);
   begin
      Get_Combined_Wide_Wide_String (State, Item, Last, Is_Illegal_Sequence);
   end Get_Combined;

end Ada.Strings.Composites;
