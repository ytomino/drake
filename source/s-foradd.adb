package body System.Formatting.Address is
   pragma Suppress (All_Checks);

   procedure Image (
      Value : System.Address;
      Item : out Address_String;
      Set : Type_Set := Upper_Case)
   is
      Use_Longest : constant Boolean :=
         Standard'Address_Size > Formatting.Unsigned'Size;
      Last : Natural; -- ignore
      Error : Boolean; -- ignore
   begin
      if Use_Longest then
         Image (
            Longest_Unsigned (Value),
            Item,
            Last,
            Base => 16,
            Set => Set,
            Width => Address_String'Length,
            Error => Error);
      else
         Image (
            Unsigned (Value),
            Item,
            Last,
            Base => 16,
            Set => Set,
            Width => Address_String'Length,
            Error => Error);
      end if;
   end Image;

   procedure Value (
      Item : Address_String;
      Result : out System.Address;
      Error : out Boolean)
   is
      Use_Longest : constant Boolean :=
         Standard'Address_Size > Formatting.Unsigned'Size;
      Last : Natural;
   begin
      if Use_Longest then
         Value (
            Item,
            Last,
            System.Formatting.Longest_Unsigned (Result),
            Base => 16,
            Error => Error);
      else
         Value (
            Item,
            Last,
            System.Formatting.Unsigned (Result),
            Base => 16,
            Error => Error);
      end if;
      Error := Error or else Last /= Item'Last;
   end Value;

end System.Formatting.Address;
