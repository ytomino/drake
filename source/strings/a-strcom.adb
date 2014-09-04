with Ada.UCD.Combining_Class;
with System.UTF_Conversions;
package body Ada.Strings.Composites is
   use type UCD.UCS_4;
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

      procedure Get_Combined_No_Length_Check (
         State : in out Composites.State;
         Item : String_Type;
         Last : out Natural);

      procedure Get_Combined (
         Item : String_Type;
         Last : out Natural);

      procedure Get_Combined (
         State : in out Composites.State;
         Item : String_Type;
         Last : out Natural);

   end Generic_Composites;

   package body Generic_Composites is

      procedure Start (Item : String_Type; State : out Composites.State) is
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
         State : in out Composites.State;
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
            if Code in
               UCD.Hangul.LBase ..
               UCD.Hangul.LBase + UCD.Hangul.LCount - 1
            then
               From_UTF (
                  Item (Last + 1 .. Item'Last),
                  Next,
                  Code,
                  From_Status);
               if Code in
                  UCD.Hangul.VBase ..
                  UCD.Hangul.VBase + UCD.Hangul.VCount - 1
               then
                  Last := Next; -- LV
                  From_UTF (
                     Item (Last + 1 .. Item'Last),
                     Next,
                     Code,
                     From_Status);
                  if Code in
                     UCD.Hangul.TBase ..
                     UCD.Hangul.TBase + UCD.Hangul.TCount - 1
                  then
                     Last := Next; -- LVT
                  end if;
               end if;
            elsif Code in
               UCD.Hangul.SBase ..
               UCD.Hangul.SBase + UCD.Hangul.SCount - 1
            then
               From_UTF (
                  Item (Last + 1 .. Item'Last),
                  Next,
                  Code,
                  From_Status);
               if Code in
                  UCD.Hangul.TBase ..
                  UCD.Hangul.TBase + UCD.Hangul.TCount - 1
               then
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
         State : in out Composites.State;
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
      Last : out Natural)
      renames UTF_8.Get_Combined;

   procedure Get_Combined (
      State : in out Composites.State;
      Item : String;
      Last : out Natural)
      renames UTF_8.Get_Combined;

   procedure Get_Combined (
      Item : Wide_String;
      Last : out Natural)
      renames UTF_16.Get_Combined;

   procedure Get_Combined (
      State : in out Composites.State;
      Item : Wide_String;
      Last : out Natural)
      renames UTF_16.Get_Combined;

   procedure Get_Combined (
      Item : Wide_Wide_String;
      Last : out Natural)
      renames UTF_32.Get_Combined;

   procedure Get_Combined (
      State : in out Composites.State;
      Item : Wide_Wide_String;
      Last : out Natural)
      renames UTF_32.Get_Combined;

end Ada.Strings.Composites;
