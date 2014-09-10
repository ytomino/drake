with Ada.UCD.Combining_Class;
with System.UTF_Conversions;
package body Ada.Strings.Composites is
   use type UCD.UCS_4;
   use type System.UTF_Conversions.From_Status_Type;
   use type System.UTF_Conversions.UCS_4;

   generic
      type Character_Type is (<>);
      type String_Type is array (Positive range <>) of Character_Type;
      with procedure From_UTF (
         Data : String_Type;
         Last : out Natural;
         Result : out System.UTF_Conversions.UCS_4;
         Status : out System.UTF_Conversions.From_Status_Type);
   package Generic_Composites is

      procedure Start (Item : String_Type; State : out Composites.State);

      procedure Get_Combined (
         Item : String_Type;
         Last : out Natural;
         Is_Illegal_Sequence : out Boolean);

      procedure Get_Combined (
         State : in out Composites.State;
         Item : String_Type;
         Last : out Natural;
         Is_Illegal_Sequence : out Boolean);

   end Generic_Composites;

   package body Generic_Composites is

      procedure Start_No_Length_Check (
         Item : String_Type;
         State : out Composites.State);

      procedure Get_Combined_No_Length_Check (
         State : in out Composites.State;
         Item : String_Type;
         Last : out Natural;
         Is_Illegal_Sequence : out Boolean);

      procedure Start_No_Length_Check (
         Item : String_Type;
         State : out Composites.State)
      is
         Code : System.UTF_Conversions.UCS_4;
         From_Status : System.UTF_Conversions.From_Status_Type;
      begin
         From_UTF (Item, State.Next_Last, Code, From_Status);
         State.Next_Character := Wide_Wide_Character'Val (Code);
         State.Next_Is_Illegal_Sequence :=
            From_Status /= System.UTF_Conversions.Success;
         if State.Next_Is_Illegal_Sequence then
            State.Next_Combining_Class := 0;
         else
            State.Next_Combining_Class :=
               Combining_Class (State.Next_Character);
         end if;
      end Start_No_Length_Check;

      procedure Start (Item : String_Type; State : out Composites.State) is
      begin
         if Item'Length = 0 then
            State.Next_Character := Wide_Wide_Character'Val (0);
            State.Next_Combining_Class := 0;
            State.Next_Last := Item'Last;
         else
            Start_No_Length_Check (Item, State);
         end if;
      end Start;

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
               Code : System.UTF_Conversions.UCS_4;
               From_Status : System.UTF_Conversions.From_Status_Type;
            begin
               Code := Wide_Wide_Character'Pos (State.Next_Character);
               if Code in
                  UCD.Hangul.LBase ..
                  UCD.Hangul.LBase + UCD.Hangul.LCount - 1
               then
                  From_UTF (
                     Item (Last + 1 .. Item'Last),
                     New_Last,
                     Code,
                     From_Status);
                  if From_Status = System.UTF_Conversions.Success
                     and then Code in
                        UCD.Hangul.VBase ..
                        UCD.Hangul.VBase + UCD.Hangul.VCount - 1
                  then
                     Last := New_Last; -- LV
                     From_UTF (
                        Item (Last + 1 .. Item'Last),
                        New_Last,
                        Code,
                        From_Status);
                     if From_Status = System.UTF_Conversions.Success
                        and then Code in
                           UCD.Hangul.TBase ..
                           UCD.Hangul.TBase + UCD.Hangul.TCount - 1
                     then
                        Last := New_Last; -- LVT
                     end if;
                  end if;
               elsif Code in
                  UCD.Hangul.SBase ..
                  UCD.Hangul.SBase + UCD.Hangul.SCount - 1
               then
                  From_UTF (
                     Item (Last + 1 .. Item'Last),
                     New_Last,
                     Code,
                     From_Status);
                  if From_Status = System.UTF_Conversions.Success
                     and then Code in
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

      procedure Get_Combined (
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
               Start_No_Length_Check (Item, St);
               Get_Combined_No_Length_Check (
                  St,
                  Item,
                  Last,
                  Is_Illegal_Sequence);
            end;
         end if;
      end Get_Combined;

      procedure Get_Combined (
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
            Get_Combined_No_Length_Check (
               State,
               Item,
               Last,
               Is_Illegal_Sequence);
         end if;
      end Get_Combined;

   end Generic_Composites;

   package UTF_8 is
      new Generic_Composites (
         Character,
         String,
         System.UTF_Conversions.From_UTF_8);
   package UTF_16 is
      new Generic_Composites (
         Wide_Character,
         Wide_String,
         System.UTF_Conversions.From_UTF_16);
   package UTF_32 is
      new Generic_Composites (
         Wide_Wide_Character,
         Wide_Wide_String,
         System.UTF_Conversions.From_UTF_32);

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

   procedure Iterate (
      Process : not null access procedure (
         Item : Wide_Wide_Character;
         Combining_Class : Class)) is
   begin
      for I in UCD.Combining_Class.Table_XXXX'Range loop
         declare
            Item : UCD.Combining_Class.Table_16_Item_Type
               renames UCD.Combining_Class.Table_XXXX (I);
         begin
            for J in
               Item.Start ..
               Item.Start + UCD.UCS_4 (Item.Length) - 1
            loop
               Process (
                  Wide_Wide_Character'Val (J),
                  Class (Item.Combining_Class));
            end loop;
         end;
      end loop;
      for I in UCD.Combining_Class.Table_1XXXX'Range loop
         declare
            Item : UCD.Combining_Class.Table_16_Item_Type
               renames UCD.Combining_Class.Table_1XXXX (I);
         begin
            for J in
               Item.Start ..
               Item.Start + UCD.UCS_4 (Item.Length) - 1
            loop
               Process (
                  Wide_Wide_Character'Val (J + 16#10000#),
                  Class (Item.Combining_Class));
            end loop;
         end;
      end loop;
   end Iterate;

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

   procedure Start (Item : String; State : out Composites.State)
      renames UTF_8.Start;

   procedure Start (Item : Wide_String; State : out Composites.State)
      renames UTF_16.Start;

   procedure Start (Item : Wide_Wide_String; State : out Composites.State)
      renames UTF_32.Start;

   procedure Get_Combined (
      Item : String;
      Last : out Natural;
      Is_Illegal_Sequence : out Boolean)
      renames UTF_8.Get_Combined;

   procedure Get_Combined (
      State : in out Composites.State;
      Item : String;
      Last : out Natural;
      Is_Illegal_Sequence : out Boolean)
      renames UTF_8.Get_Combined;

   procedure Get_Combined (
      Item : Wide_String;
      Last : out Natural;
      Is_Illegal_Sequence : out Boolean)
      renames UTF_16.Get_Combined;

   procedure Get_Combined (
      State : in out Composites.State;
      Item : Wide_String;
      Last : out Natural;
      Is_Illegal_Sequence : out Boolean)
      renames UTF_16.Get_Combined;

   procedure Get_Combined (
      Item : Wide_Wide_String;
      Last : out Natural;
      Is_Illegal_Sequence : out Boolean)
      renames UTF_32.Get_Combined;

   procedure Get_Combined (
      State : in out Composites.State;
      Item : Wide_Wide_String;
      Last : out Natural;
      Is_Illegal_Sequence : out Boolean)
      renames UTF_32.Get_Combined;

end Ada.Strings.Composites;
