with System.Formatting;
with System.Val_LLU;
with System.Val_Uns;
package body Ada.Text_IO.Modular_IO is
   pragma Suppress (All_Checks);
   use type System.Formatting.Longest_Unsigned;
   use type System.Formatting.Unsigned;

   procedure Get (
      File : File_Type;
      Item : out Num;
      Width : Field := 0) is
   begin
      raise Program_Error;
   end Get;

   procedure Get (
      Item : out Num;
      Width : Field := 0) is
   begin
      raise Program_Error;
   end Get;

   procedure Put (
      File : File_Type;
      Item : Num;
      Width : Field := Default_Width;
      Base : Number_Base := Default_Base) is
   begin
      raise Program_Error;
   end Put;

   procedure Put (
      Item : Num;
      Width : Field := Default_Width;
      Base : Number_Base := Default_Base) is
   begin
      raise Program_Error;
   end Put;

   procedure Get (
      From : String;
      Item : out Num;
      Last : out Positive)
   is
      Index : Positive := From'First;
   begin
      System.Val_Uns.Skip_Spaces (From, Index);
      if Index < From'Last and then From (Index) = '+' then
         Index := Index + 1;
      end if;
      if Num'Size > System.Formatting.Unsigned'Size then
         declare
            Result : System.Formatting.Longest_Unsigned;
         begin
            System.Val_LLU.Get_Longest_Unsigned_Literal_Without_Sign (
               From,
               Index,
               Result);
            if Result > System.Formatting.Longest_Unsigned (Num'Last) then
               raise Constraint_Error;
            end if;
            Item := Num (Result);
         end;
      else
         declare
            Result : System.Formatting.Unsigned;
         begin
            System.Val_Uns.Get_Unsigned_Literal_Without_Sign (
               From,
               Index,
               Result);
            if Result > System.Formatting.Unsigned (Num'Last) then
               raise Constraint_Error;
            end if;
            Item := Num (Result);
         end;
      end if;
      Last := Index - 1;
   end Get;

   procedure Put (
      To : out String;
      Item : Num;
      Base : Number_Base := Default_Base)
   is
      Index : Positive;
      Num_Last : Natural;
      Num_Width : Natural;
      Padding : Character;
      Last : Natural;
      Error : Boolean;
   begin
      if Base /= 10 then
         if Num'Size > System.Formatting.Unsigned'Size then
            Num_Width := System.Formatting.Width (
               System.Formatting.Longest_Unsigned (Num'Last),
               System.Formatting.Base_Type (Base));
         else
            Num_Width := System.Formatting.Width (
               System.Formatting.Unsigned (Num'Last),
               System.Formatting.Base_Type (Base));
         end if;
         Num_Last := To'Last - 1;
         Index := Num_Last - Num_Width;
         declare
            Base_Index : Positive := Index - 1;
         begin
            if Base >= 10 then
               Base_Index := Base_Index - 1;
            end if;
            if Base_Index < To'First then
               raise Constraint_Error;
            end if;
            for I in To'First .. Base_Index - 1 loop
               To (I) := ' ';
            end loop;
            System.Formatting.Image (
               System.Formatting.Unsigned (Base),
               To (Base_Index .. Index - 1),
               Last,
               Error => Error);
         end;
         To (Index) := '#';
         To (To'Last) := '#';
         Index := Index + 1;
         Padding := '0';
      else
         Index := To'First;
         Num_Last := To'Last;
         Num_Width := Num_Last - Index + 1;
         Padding := ' ';
      end if;
      if Num'Size > System.Formatting.Unsigned'Size then
         System.Formatting.Image (
            System.Formatting.Longest_Unsigned (Item),
            To (Index .. Num_Last),
            Last,
            Base => System.Formatting.Base_Type (Base),
            Width => Num_Width,
            Padding => Padding,
            Error => Error);
      else
         System.Formatting.Image (
            System.Formatting.Unsigned (Item),
            To (Index .. Num_Last),
            Last,
            Base => System.Formatting.Base_Type (Base),
            Width => Num_Width,
            Padding => Padding,
            Error => Error);
      end if;
      if Error or else Last /= Num_Last then
         raise Constraint_Error;
      end if;
   end Put;

end Ada.Text_IO.Modular_IO;
