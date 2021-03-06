with Ada.Text_IO.Formatting;
with System.Formatting.Literals;
with System.Long_Long_Integer_Types;
package body Ada.Text_IO.Modular_IO is
   use type System.Long_Long_Integer_Types.Word_Unsigned;
   use type System.Long_Long_Integer_Types.Long_Long_Unsigned;

   subtype Word_Unsigned is System.Long_Long_Integer_Types.Word_Unsigned;
   subtype Long_Long_Unsigned is
      System.Long_Long_Integer_Types.Long_Long_Unsigned;

   procedure Put_To_Field (
      To : out String;
      Last : out Natural;
      Item : Num;
      Base : Number_Base);
   procedure Put_To_Field (
      To : out String;
      Last : out Natural;
      Item : Num;
      Base : Number_Base) is
   begin
      if Num'Size > Standard'Word_Size then
         Formatting.Modular_Image (To, Last, Long_Long_Unsigned (Item), Base);
      else
         Formatting.Modular_Image (To, Last, Word_Unsigned (Item), Base);
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
            Base_Item : Long_Long_Unsigned;
            Error : Boolean;
         begin
            System.Formatting.Literals.Get_Literal (
               From,
               Last,
               Base_Item,
               Error => Error);
            if Error or else Base_Item > Long_Long_Unsigned (Num'Last) then
               raise Data_Error;
            end if;
            Item := Num (Base_Item);
         end;
      else
         declare
            Base_Item : Word_Unsigned;
            Error : Boolean;
         begin
            System.Formatting.Literals.Get_Literal (
               From,
               Last,
               Base_Item,
               Error => Error);
            if Error or else Base_Item > Word_Unsigned (Num'Last) then
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

   procedure Put (
      File : File_Type;
      Item : Num;
      Width : Field := Default_Width;
      Base : Number_Base := Default_Base)
   is
      S : String (1 .. 4 + Num'Width + Width); -- "16##"
      Last : Natural;
   begin
      Put_To_Field (S, Last, Item, Base);
      Formatting.Tail (File, S (1 .. Last), Width); -- checking the predicate
   end Put;

   procedure Put (
      Item : Num;
      Width : Field := Default_Width;
      Base : Number_Base := Default_Base) is
   begin
      Put (Current_Output.all, Item, Width, Base);
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
      Base : Number_Base := Default_Base)
   is
      S : String (1 .. To'Length);
      Last : Natural;
   begin
      Put_To_Field (S, Last, Item, Base);
      Formatting.Tail (To, S (1 .. Last));
   end Put;

end Ada.Text_IO.Modular_IO;
