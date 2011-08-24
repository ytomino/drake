package body Ada.Characters.Conversions is
   pragma Suppress (All_Checks);

   function Is_Wide_Character (Item : Character) return Boolean is
   begin
      return Character'Pos (Item) <= 16#7f#;
   end Is_Wide_Character;

   function Is_Character (Item : Wide_Character) return Boolean is
   begin
      return Wide_Character'Pos (Item) <= 16#7f#;
   end Is_Character;

   function Is_String (Item : Wide_String) return Boolean is
      pragma Unreferenced (Item);
   begin
      return True;
   end Is_String;

   function Is_Wide_Wide_Character (Item : Wide_Character) return Boolean is
   begin
      return Wide_Character'Pos (Item) not in 16#d800# .. 16#dfff#;
   end Is_Wide_Wide_Character;

   function Is_Character (Item : Wide_Wide_Character) return Boolean is
   begin
      return Wide_Wide_Character'Pos (Item) <= 16#7f#;
   end Is_Character;

   function Is_String (Item : Wide_Wide_String) return Boolean is
      pragma Unreferenced (Item);
   begin
      return True;
   end Is_String;

   function Is_Wide_Character (Item : Wide_Wide_Character) return Boolean is
   begin
      return Wide_Wide_Character'Pos (Item) <= 16#d7ff#
         or else Wide_Wide_Character'Pos (Item) in 16#e000# .. 16#ffff#;
   end Is_Wide_Character;

   function To_Wide_Character (
      Item : Character;
      Substitute : Wide_Character := ' ')
      return Wide_Character is
   begin
      if Is_Wide_Character (Item) then
         return Wide_Character'Val (Character'Pos (Item));
      else
         return Substitute;
      end if;
   end To_Wide_Character;

   function To_Wide_Wide_Character (
      Item : Character;
      Substitute : Wide_Wide_Character := ' ')
      return Wide_Wide_Character is
   begin
      if Is_Wide_Wide_Character (Item) then
         return Wide_Wide_Character'Val (Character'Pos (Item));
      else
         return Substitute;
      end if;
   end To_Wide_Wide_Character;

   function To_Character (
      Item : Wide_Character;
      Substitute : Character := ' ')
      return Character is
   begin
      if Is_Character (Item) then
         return Character'Val (Wide_Character'Pos (Item));
      else
         return Substitute;
      end if;
   end To_Character;

   function To_Wide_Wide_Character (
      Item : Wide_Character;
      Substitute : Wide_Wide_Character := ' ')
      return Wide_Wide_Character is
   begin
      if Is_Wide_Wide_Character (Item) then
         return Wide_Wide_Character'Val (Wide_Character'Pos (Item));
      else
         return Substitute;
      end if;
   end To_Wide_Wide_Character;

   function To_Character (
      Item : Wide_Wide_Character;
      Substitute : Character := ' ')
      return Character is
   begin
      if Is_Character (Item) then
         return Character'Val (Wide_Wide_Character'Pos (Item));
      else
         return Substitute;
      end if;
   end To_Character;

   function To_Wide_Character (
      Item : Wide_Wide_Character;
      Substitute : Wide_Character := ' ')
      return Wide_Character is
   begin
      if Is_Wide_Character (Item) then
         return Wide_Character'Val (Wide_Wide_Character'Pos (Item));
      else
         return Substitute;
      end if;
   end To_Wide_Character;

   procedure Get (
      Item : String;
      Last : out Natural;
      Value : out Wide_Wide_Character;
      Substitute : Wide_Wide_Character := ' ')
   is
      Code : System.UTF_Conversions.UCS_4;
      Is_Illegal_Sequence : Boolean;
   begin
      System.UTF_Conversions.From_UTF_8 (
         Item,
         Last,
         Code,
         Is_Illegal_Sequence);
      if Is_Illegal_Sequence then
         Value := Substitute;
      else
         Value := Wide_Wide_Character'Val (Code);
      end if;
   end Get;

   procedure Get (
      Item : String;
      Last : out Natural;
      Value : out Wide_Wide_Character;
      Is_Illegal_Sequence : out Boolean)
   is
      Code : System.UTF_Conversions.UCS_4;
   begin
      System.UTF_Conversions.From_UTF_8 (
         Item,
         Last,
         Code,
         Is_Illegal_Sequence);
      Value := Wide_Wide_Character'Val (Code);
   end Get;

   procedure Get_Reverse (
      Item : String;
      First : out Positive;
      Value : out Wide_Wide_Character;
      Substitute : Wide_Wide_Character := ' ')
   is
      Code : System.UTF_Conversions.UCS_4;
      Is_Illegal_Sequence : Boolean;
   begin
      System.UTF_Conversions.From_UTF_8_Reverse (
         Item,
         First,
         Code,
         Is_Illegal_Sequence);
      if Is_Illegal_Sequence then
         Value := Substitute;
      else
         Value := Wide_Wide_Character'Val (Code);
      end if;
   end Get_Reverse;

   procedure Get_Reverse (
      Item : String;
      First : out Positive;
      Value : out Wide_Wide_Character;
      Is_Illegal_Sequence : out Boolean)
   is
      Code : System.UTF_Conversions.UCS_4;
   begin
      System.UTF_Conversions.From_UTF_8_Reverse (
         Item,
         First,
         Code,
         Is_Illegal_Sequence);
      Value := Wide_Wide_Character'Val (Code);
   end Get_Reverse;

   procedure Get (
      Item : Wide_String;
      Last : out Natural;
      Value : out Wide_Wide_Character;
      Substitute : Wide_Wide_Character := ' ')
   is
      Code : System.UTF_Conversions.UCS_4;
      Is_Illegal_Sequence : Boolean;
   begin
      System.UTF_Conversions.From_UTF_16 (
         Item,
         Last,
         Code,
         Is_Illegal_Sequence);
      if Is_Illegal_Sequence then
         Value := Substitute;
      else
         Value := Wide_Wide_Character'Val (Code);
      end if;
   end Get;

   procedure Get (
      Item : Wide_String;
      Last : out Natural;
      Value : out Wide_Wide_Character;
      Is_Illegal_Sequence : out Boolean)
   is
      Code : System.UTF_Conversions.UCS_4;
   begin
      System.UTF_Conversions.From_UTF_16 (
         Item,
         Last,
         Code,
         Is_Illegal_Sequence);
      Value := Wide_Wide_Character'Val (Code);
   end Get;

   procedure Get_Reverse (
      Item : Wide_String;
      First : out Positive;
      Value : out Wide_Wide_Character;
      Substitute : Wide_Wide_Character := ' ')
   is
      Code : System.UTF_Conversions.UCS_4;
      Is_Illegal_Sequence : Boolean;
   begin
      System.UTF_Conversions.From_UTF_16_Reverse (
         Item,
         First,
         Code,
         Is_Illegal_Sequence);
      if Is_Illegal_Sequence then
         Value := Substitute;
      else
         Value := Wide_Wide_Character'Val (Code);
      end if;
   end Get_Reverse;

   procedure Get_Reverse (
      Item : Wide_String;
      First : out Positive;
      Value : out Wide_Wide_Character;
      Is_Illegal_Sequence : out Boolean)
   is
      Code : System.UTF_Conversions.UCS_4;
   begin
      System.UTF_Conversions.From_UTF_16_Reverse (
         Item,
         First,
         Code,
         Is_Illegal_Sequence);
      Value := Wide_Wide_Character'Val (Code);
   end Get_Reverse;

   procedure Get (
      Item : Wide_Wide_String;
      Last : out Natural;
      Value : out Wide_Wide_Character;
      Substitute : Wide_Wide_Character := ' ')
   is
      Code : System.UTF_Conversions.UCS_4;
      Is_Illegal_Sequence : Boolean;
   begin
      System.UTF_Conversions.From_UTF_32 (
         Item,
         Last,
         Code,
         Is_Illegal_Sequence);
      if Is_Illegal_Sequence then
         Value := Substitute;
      else
         Value := Wide_Wide_Character'Val (Code);
      end if;
   end Get;

   procedure Get (
      Item : Wide_Wide_String;
      Last : out Natural;
      Value : out Wide_Wide_Character;
      Is_Illegal_Sequence : out Boolean)
   is
      Code : System.UTF_Conversions.UCS_4;
   begin
      System.UTF_Conversions.From_UTF_32 (
         Item,
         Last,
         Code,
         Is_Illegal_Sequence);
      Value := Wide_Wide_Character'Val (Code);
   end Get;

   procedure Get_Reverse (
      Item : Wide_Wide_String;
      First : out Positive;
      Value : out Wide_Wide_Character;
      Substitute : Wide_Wide_Character := ' ')
   is
      Code : System.UTF_Conversions.UCS_4;
      Is_Illegal_Sequence : Boolean;
   begin
      System.UTF_Conversions.From_UTF_32_Reverse (
         Item,
         First,
         Code,
         Is_Illegal_Sequence);
      if Is_Illegal_Sequence then
         Value := Substitute;
      else
         Value := Wide_Wide_Character'Val (Code);
      end if;
   end Get_Reverse;

   procedure Get_Reverse (
      Item : Wide_Wide_String;
      First : out Positive;
      Value : out Wide_Wide_Character;
      Is_Illegal_Sequence : out Boolean)
   is
      Code : System.UTF_Conversions.UCS_4;
   begin
      System.UTF_Conversions.From_UTF_32_Reverse (
         Item,
         First,
         Code,
         Is_Illegal_Sequence);
      Value := Wide_Wide_Character'Val (Code);
   end Get_Reverse;

   procedure Put (
      Value : Wide_Wide_Character;
      Item : out String;
      Last : out Natural)
   is
      Error : Boolean;
   begin
      System.UTF_Conversions.To_UTF_8 (
         Wide_Wide_Character'Pos (Value),
         Item,
         Last,
         Error);
      if Error then
         raise Constraint_Error; -- Strings.Length_Error ???
      end if;
   end Put;

   procedure Put (
      Value : Wide_Wide_Character;
      Item : out Wide_String;
      Last : out Natural)
   is
      Error : Boolean;
   begin
      System.UTF_Conversions.To_UTF_16 (
         Wide_Wide_Character'Pos (Value),
         Item,
         Last,
         Error);
      if Error then
         raise Constraint_Error;
      end if;
   end Put;

   procedure Put (
      Value : Wide_Wide_Character;
      Item : out Wide_Wide_String;
      Last : out Natural)
   is
      Error : Boolean;
   begin
      System.UTF_Conversions.To_UTF_32 (
         Wide_Wide_Character'Pos (Value),
         Item,
         Last,
         Error);
      if Error then
         raise Constraint_Error;
      end if;
   end Put;

end Ada.Characters.Conversions;
