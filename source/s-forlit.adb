package body System.Formatting.Literals is
   pragma Suppress (All_Checks);
   use type Long_Long_Integer_Types.Word_Integer;
   use type Long_Long_Integer_Types.Word_Unsigned;
   use type Long_Long_Integer_Types.Long_Long_Unsigned;

   subtype Word_Integer is Long_Long_Integer_Types.Word_Integer;
   subtype Word_Unsigned is Long_Long_Integer_Types.Word_Unsigned;
   subtype Long_Long_Unsigned is Long_Long_Integer_Types.Long_Long_Unsigned;

   procedure Get (
      Item : String;
      Last : in out Natural;
      Result : out Word_Unsigned;
      Base : Number_Base;
      Error : out Boolean);
   procedure Get (
      Item : String;
      Last : in out Natural;
      Result : out Word_Unsigned;
      Base : Number_Base;
      Error : out Boolean) is
   begin
      Value (Item (Last + 1 .. Item'Last), Last, Result,
         Base => Base,
         Skip_Underscore => True,
         Error => Error);
   end Get;

   procedure Get (
      Item : String;
      Last : in out Natural;
      Result : out Long_Long_Unsigned;
      Base : Number_Base;
      Error : out Boolean);
   procedure Get (
      Item : String;
      Last : in out Natural;
      Result : out Long_Long_Unsigned;
      Base : Number_Base;
      Error : out Boolean) is
   begin
      Value (Item (Last + 1 .. Item'Last), Last, Result,
         Base => Base,
         Skip_Underscore => True,
         Error => Error);
   end Get;

   procedure Get_Literal_Without_Sign (
      Item : String;
      Last : in out Natural;
      Result : out Word_Unsigned;
      Error : out Boolean);
   procedure Get_Literal_Without_Sign (
      Item : String;
      Last : in out Natural;
      Result : out Word_Unsigned;
      Error : out Boolean)
   is
      Base : Number_Base := 10;
      Mark : Character;
      Exponent : Integer;
   begin
      Get (Item, Last, Result, Base => Base, Error => Error);
      if not Error then
         if Last < Item'Last
            and then (Item (Last + 1) = '#' or else Item (Last + 1) = ':')
         then
            Mark := Item (Last + 1);
            Last := Last + 1;
            if Result in
               Word_Unsigned (Number_Base'First) ..
               Word_Unsigned (Number_Base'Last)
            then
               Base := Number_Base (Result);
               Get (Item, Last, Result, Base => Base, Error => Error);
               if not Error then
                  if Last < Item'Last and then Item (Last + 1) = Mark then
                     Last := Last + 1;
                  else
                     Error := True;
                     return;
                  end if;
               else
                  return;
               end if;
            else
               Error := True;
               return;
            end if;
         end if;
         Get_Exponent (Item, Last, Exponent,
            Positive_Only => True,
            Error => Error);
         if not Error and then Exponent /= 0 then
            Result := Result * Word_Unsigned (Base) ** Exponent;
         end if;
      end if;
   end Get_Literal_Without_Sign;

   procedure Get_Literal_Without_Sign (
      Item : String;
      Last : in out Natural;
      Result : out Long_Long_Unsigned;
      Error : out Boolean);
   procedure Get_Literal_Without_Sign (
      Item : String;
      Last : in out Natural;
      Result : out Long_Long_Unsigned;
      Error : out Boolean)
   is
      Base : Number_Base := 10;
      Mark : Character;
      Exponent : Integer;
   begin
      Get (Item, Last, Result, Base => Base, Error => Error);
      if not Error then
         if Last < Item'Last
            and then (Item (Last + 1) = '#' or else Item (Last + 1) = ':')
         then
            Mark := Item (Last + 1);
            Last := Last + 1;
            if Result in
               Long_Long_Unsigned (Number_Base'First) ..
               Long_Long_Unsigned (Number_Base'Last)
            then
               Base := Number_Base (Result);
               Get (Item, Last, Result, Base => Base, Error => Error);
               if not Error then
                  if Last < Item'Last and then Item (Last + 1) = Mark then
                     Last := Last + 1;
                  else
                     Error := True;
                     return;
                  end if;
               else
                  return;
               end if;
            else
               Error := True;
               return;
            end if;
         end if;
         Get_Exponent (Item, Last, Exponent,
            Positive_Only => True,
            Error => Error);
         if not Error and then Exponent /= 0 then
            Result := Result * Long_Long_Unsigned (Base) ** Exponent;
         end if;
      end if;
   end Get_Literal_Without_Sign;

   --  implementation

   procedure Skip_Spaces (Item : String; Last : in out Natural) is
   begin
      while Last < Item'Last and then Item (Last + 1) = ' ' loop
         Last := Last + 1;
      end loop;
   end Skip_Spaces;

   procedure Check_Last (Item : String; Last : Natural; Error : out Boolean) is
      I : Natural := Last;
   begin
      Skip_Spaces (Item, I);
      Error := I /= Item'Last;
   end Check_Last;

   procedure Get_Exponent (
      Item : String;
      Last : in out Natural;
      Result : out Integer;
      Positive_Only : Boolean;
      Error : out Boolean) is
   begin
      if Last < Item'Last
         and then (Item (Last + 1) = 'E' or else Item (Last + 1) = 'e')
      then
         Last := Last + 1;
         if Last < Item'Last and then Item (Last + 1) = '-' then
            if not Positive_Only then
               Last := Last + 1;
               Get (Item, Last, Word_Unsigned (Result),
                  Base => 10,
                  Error => Error);
               --  ignore error
               Result := -Result;
            else
               Error := True;
            end if;
         else
            if Last < Item'Last and then Item (Last + 1) = '+' then
               Last := Last + 1;
            end if;
            Get (Item, Last, Word_Unsigned (Result),
               Base => 10,
               Error => Error);
         end if;
      else
         Result := 0;
         Error := False;
      end if;
   end Get_Exponent;

   procedure Get_Literal (
      Item : String;
      Last : out Natural;
      Result : out Long_Long_Integer_Types.Word_Integer;
      Error : out Boolean)
   is
      Unsigned_Result : Word_Unsigned;
   begin
      Last := Item'First - 1;
      Skip_Spaces (Item, Last);
      if Last < Item'Last and then Item (Last + 1) = '-' then
         Last := Last + 1;
         Get_Literal_Without_Sign (Item, Last, Unsigned_Result,
            Error => Error);
         if not Error then
            if Unsigned_Result <= -Word_Unsigned'Mod (Word_Integer'First) then
               Result := -Word_Integer (Unsigned_Result);
            else
               Error := True;
            end if;
         end if;
      else
         if Last < Item'Last and then Item (Last + 1) = '+' then
            Last := Last + 1;
         end if;
         Get_Literal_Without_Sign (Item, Last, Unsigned_Result,
            Error => Error);
         if not Error then
            if Unsigned_Result <= Word_Unsigned (Word_Integer'Last) then
               Result := Word_Integer (Unsigned_Result);
            else
               Error := True;
            end if;
         end if;
      end if;
   end Get_Literal;

   procedure Get_Literal (
      Item : String;
      Last : out Natural;
      Result : out Long_Long_Integer;
      Error : out Boolean) is
   begin
      if Standard'Word_Size < Long_Long_Integer'Size then
         --  optimized for 32bit
         declare
            Unsigned_Result : Long_Long_Unsigned;
         begin
            Last := Item'First - 1;
            Skip_Spaces (Item, Last);
            if Last < Item'Last and then Item (Last + 1) = '-' then
               Last := Last + 1;
               Get_Literal_Without_Sign (Item, Last, Unsigned_Result,
                  Error => Error);
               if not Error then
                  if Unsigned_Result <=
                     -Long_Long_Unsigned'Mod (Long_Long_Integer'First)
                  then
                     Result := -Long_Long_Integer (Unsigned_Result);
                  else
                     Error := True;
                  end if;
               end if;
            else
               if Last < Item'Last and then Item (Last + 1) = '+' then
                  Last := Last + 1;
               end if;
               Get_Literal_Without_Sign (Item, Last, Unsigned_Result,
                  Error => Error);
               if not Error then
                  if Unsigned_Result <=
                     Long_Long_Unsigned (Long_Long_Integer'Last)
                  then
                     Result := Long_Long_Integer (Unsigned_Result);
                  else
                     Error := True;
                  end if;
               end if;
            end if;
         end;
      else
         --  optimized for 64bit
         Get_Literal (Item, Last, Word_Integer (Result), Error);
      end if;
   end Get_Literal;

   procedure Get_Literal (
      Item : String;
      Last : out Natural;
      Result : out Long_Long_Integer_Types.Word_Unsigned;
      Error : out Boolean) is
   begin
      Last := Item'First - 1;
      Skip_Spaces (Item, Last);
      if Last < Item'Last and then Item (Last + 1) = '+' then
         Last := Last + 1;
      end if;
      Get_Literal_Without_Sign (Item, Last, Result, Error => Error);
   end Get_Literal;

   procedure Get_Literal (
      Item : String;
      Last : out Natural;
      Result : out Long_Long_Integer_Types.Long_Long_Unsigned;
      Error : out Boolean) is
   begin
      if Standard'Word_Size < Long_Long_Integer'Size then
         --  optimized for 32bit
         Last := Item'First - 1;
         Skip_Spaces (Item, Last);
         if Last < Item'Last and then Item (Last + 1) = '+' then
            Last := Last + 1;
         end if;
         Get_Literal_Without_Sign (Item, Last, Result, Error => Error);
      else
         --  optimized for 64bit
         Get_Literal (Item, Last, Word_Unsigned (Result), Error);
      end if;
   end Get_Literal;

end System.Formatting.Literals;
