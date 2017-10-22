package body System.Formatting.Address is
   pragma Suppress (All_Checks);

   subtype Word_Unsigned is Long_Long_Integer_Types.Word_Unsigned;
   subtype Long_Long_Unsigned is Long_Long_Integer_Types.Long_Long_Unsigned;

   --  implementation

   procedure Image (
      Value : System.Address;
      Item : out Address_String;
      Set : Type_Set := Upper_Case)
   is
      Use_Longest : constant Boolean :=
         Standard'Address_Size > Standard'Word_Size;
      Last : Natural; -- ignore
      Error : Boolean; -- ignore
   begin
      if Use_Longest then
         Image (
            Long_Long_Unsigned (Value),
            Item,
            Last,
            Base => 16,
            Set => Set,
            Width => Address_String'Length,
            Error => Error);
      else
         Image (
            Word_Unsigned (Value),
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
         Standard'Address_Size > Standard'Word_Size;
      Last : Natural;
   begin
      if Use_Longest then
         Value (
            Item,
            Last,
            Long_Long_Unsigned (Result),
            Base => 16,
            Error => Error);
      else
         Value (
            Item,
            Last,
            Word_Unsigned (Result),
            Base => 16,
            Error => Error);
      end if;
      Error := Error or else Last /= Item'Last;
   end Value;

end System.Formatting.Address;
