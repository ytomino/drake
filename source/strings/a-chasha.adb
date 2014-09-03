package body Ada.Characters.ASCII.Handling is

   function Is_Control (Item : Character) return Boolean is
   begin
      return Item in Character'Val (0) .. Character'Val (16#1f#)
         or else Item = Character'Val (16#7f#);
   end Is_Control;

   function Is_Graphic (Item : Character) return Boolean is
   begin
      return Item in Character'Val (16#20#) .. Character'Val (16#7e#);
   end Is_Graphic;

   function Is_Letter (Item : Character) return Boolean is
   begin
      return Item in 'A' .. 'Z'
         or else Item in 'a' .. 'z';
   end Is_Letter;

   function Is_Lower (Item : Character) return Boolean is
   begin
      return Item in 'a' .. 'z';
   end Is_Lower;

   function Is_Upper (Item : Character) return Boolean is
   begin
      return Item in 'A' .. 'Z';
   end Is_Upper;

   function Is_Digit (Item : Character) return Boolean is
   begin
      return Item in '0' .. '9';
   end Is_Digit;

   function Is_Hexadecimal_Digit (Item : Character) return Boolean is
   begin
      return Item in '0' .. '9'
         or else Item in 'A' .. 'F'
         or else Item in 'a' .. 'f';
   end Is_Hexadecimal_Digit;

   function Is_Alphanumeric (Item : Character) return Boolean is
   begin
      return Item in '0' .. '9'
         or else Item in 'A' .. 'Z'
         or else Item in 'a' .. 'z';
   end Is_Alphanumeric;

   function Is_Special (Item : Character) return Boolean is
   begin
      return Item in Character'Val (16#20#) .. '/'
         or else Item in ':' .. '@'
         or else Item in '[' .. '`'
         or else Item in '{' .. '~';
   end Is_Special;

   function To_Lower (Item : Character) return Character is
   begin
      if Item in 'A' .. 'Z' then
         return Character'Val (Character'Pos (Item) + 16#20#);
      else
         return Item;
      end if;
   end To_Lower;

   function To_Upper (Item : Character) return Character is
   begin
      if Item in 'a' .. 'z' then
         return Character'Val (Character'Pos (Item) - 16#20#);
      else
         return Item;
      end if;
   end To_Upper;

   function To_Lower (Item : String) return String is
      Length : constant Natural := Item'Length;
   begin
      return Result : String (1 .. Length) do
         for I in 0 .. Length - 1 loop
            Result (Result'First + I) := To_Lower (Item (Item'First + I));
         end loop;
      end return;
   end To_Lower;

   function To_Upper (Item : String) return String is
      Length : constant Natural := Item'Length;
   begin
      return Result : String (1 .. Length) do
         for I in 0 .. Length - 1 loop
            Result (Result'First + I) := To_Upper (Item (Item'First + I));
         end loop;
      end return;
   end To_Upper;

end Ada.Characters.ASCII.Handling;
