with Ada.Text_IO.Formatting;
with System.Formatting.Literals;
with System.Long_Long_Integer_Types;
package body Ada.Text_IO.Integer_IO is

   subtype Word_Integer is System.Long_Long_Integer_Types.Word_Integer;

   procedure Put_To_Field (
      To : out String;
      Last : out Natural;
      Item : Num;
      Base : Number_Base;
      Padding : Character;
      Padding_Width : Field);
   procedure Put_To_Field (
      To : out String;
      Last : out Natural;
      Item : Num;
      Base : Number_Base;
      Padding : Character;
      Padding_Width : Field) is
   begin
      if Num'Size > Standard'Word_Size then
         Formatting.Integer_Image (
            To,
            Last,
            Long_Long_Integer (Item),
            Base,
            Padding,
            Padding_Width);
      else
         Formatting.Integer_Image (
            To,
            Last,
            Word_Integer (Item),
            Base,
            Padding,
            Padding_Width);
      end if;
   end Put_To_Field;

   procedure Get_From_Field (
      From : String;
      Item : out Num;
      Last : out Positive);
   procedure Get_From_Field (
      From : String;
      Item : out Num;
      Last : out Positive) is
   begin
      if Num'Size > Standard'Word_Size then
         declare
            Base_Item : Long_Long_Integer;
            Error : Boolean;
         begin
            System.Formatting.Literals.Get_Literal (
               From,
               Last,
               Base_Item,
               Error => Error);
            if Error
               or else Base_Item not in
                  Long_Long_Integer (Num'First) .. Long_Long_Integer (Num'Last)
            then
               raise Data_Error;
            end if;
            Item := Num (Base_Item);
         end;
      else
         declare
            Base_Item : Word_Integer;
            Error : Boolean;
         begin
            System.Formatting.Literals.Get_Literal (
               From,
               Last,
               Base_Item,
               Error => Error);
            if Error
               or else Base_Item not in
                  Word_Integer (Num'First) .. Word_Integer (Num'Last)
            then
               raise Data_Error;
            end if;
            Item := Num (Base_Item);
         end;
      end if;
   end Get_From_Field;

   --  implementation

   procedure Get (
      File : File_Type;
      Item : out Num;
      Width : Field := 0) is
   begin
      if Width /= 0 then
         declare
            S : String (1 .. Width);
            Last_1 : Natural;
            Last_2 : Natural;
         begin
            Formatting.Get_Field (File, S, Last_1); -- checking the predicate
            Get_From_Field (S (1 .. Last_1), Item, Last_2);
            if Last_2 /= Last_1 then
               raise Data_Error;
            end if;
         end;
      else
         declare
            S : constant String :=
               Formatting.Get_Numeric_Literal (
                  File, -- checking the predicate
                  Real => False);
            Last : Natural;
         begin
            Get_From_Field (S, Item, Last);
            if Last /= S'Last then
               raise Data_Error;
            end if;
         end;
      end if;
   end Get;

   procedure Get (
      Item : out Num;
      Width : Field := 0) is
   begin
      Get (Current_Input.all, Item, Width);
   end Get;

   procedure Get (
      File : not null File_Access;
      Item : out Num;
      Width : Field := 0) is
   begin
      Get (File.all, Item, Width);
   end Get;

   procedure Put (
      File : File_Type;
      Item : Num;
      Width : Field := Default_Width;
      Base : Number_Base := Default_Base;
      Padding : Character := Default_Padding)
   is
      S : String (1 .. 4 + Num'Width + Width); -- "16##"
      Last : Natural;
   begin
      Put_To_Field (S, Last, Item, Base, Padding, Width);
      Formatting.Tail (File, S (1 .. Last), Width); -- checking the predicate
   end Put;

   procedure Put (
      Item : Num;
      Width : Field := Default_Width;
      Base : Number_Base := Default_Base;
      Padding : Character := Default_Padding) is
   begin
      Put (Current_Output.all, Item, Width, Base, Padding);
   end Put;

   procedure Put (
      File : not null File_Access;
      Item : Num;
      Width : Field := Default_Width;
      Base : Number_Base := Default_Base;
      Padding : Character := Default_Padding) is
   begin
      Put (File.all, Item, Width, Base, Padding);
   end Put;

   procedure Get (
      From : String;
      Item : out Num;
      Last : out Positive) is
   begin
      Formatting.Get_Tail (From, First => Last);
      Get_From_Field (From (Last .. From'Last), Item, Last);
   end Get;

   procedure Put (
      To : out String;
      Item : Num;
      Base : Number_Base := Default_Base;
      Padding : Character := Default_Padding)
   is
      S : String (1 .. To'Length);
      Last : Natural;
   begin
      Put_To_Field (S, Last, Item, Base, Padding, To'Length);
      Formatting.Tail (To, S (1 .. Last));
   end Put;

end Ada.Text_IO.Integer_IO;
