with Ada.Exceptions;
with System.UTF_Conversions;
package body Ada.Text_IO.Inside.Wide is
   use type System.UTF_Conversions.UCS_4;

   procedure Store_Second (
      File : File_Type;
      C : Wide_Wide_Character;
      Item : out Wide_Character);
   procedure Store_Second (
      File : File_Type;
      C : Wide_Wide_Character;
      Item : out Wide_Character)
   is
      W_Buffer : Wide_String (1 .. System.UTF_Conversions.UTF_16_Max_Length);
      W_Last : Natural;
      To_Status : System.UTF_Conversions.To_Status_Type; -- ignore
   begin
      System.UTF_Conversions.To_UTF_16 (
         Wide_Wide_Character'Pos (C),
         W_Buffer,
         W_Last,
         To_Status);
      Item := W_Buffer (1);
      if W_Last >= 2 then
         --  store second of surrogate pair
         declare
            Buffer : String (1 .. System.UTF_Conversions.UTF_8_Max_Length);
            Last : Natural;
            Ref_Text : constant access Text_Type := Reference (File).all;
         begin
            System.UTF_Conversions.To_UTF_8 (
               Wide_Character'Pos (W_Buffer (2)),
               Buffer,
               Last,
               To_Status);
            Ref_Text.Last := Last;
            Ref_Text.Buffer (1 .. Last) := Buffer (1 .. Last);
         end;
      end if;
   end Store_Second;

   --  implementation

   procedure Get (File : File_Type; Item : out Wide_Character) is
      C : Wide_Wide_Character;
   begin
      Get (File, C); -- Wide_Wide
      Store_Second (File, C, Item);
   end Get;

   procedure Get (File : File_Type; Item : out Wide_Wide_Character) is
      S : String (1 .. 6);
      Length : Natural;
      From_Status : System.UTF_Conversions.From_Status_Type; -- ignore
   begin
      Get (File, S (1));
      System.UTF_Conversions.UTF_8_Sequence (S (1), Length, From_Status);
      declare
         Last : Natural;
         Code : System.UTF_Conversions.UCS_4;
      begin
         if Length >= 2 then
            Get (File, S (2 .. Length));
         end if;
         System.UTF_Conversions.From_UTF_8 (
            S (1 .. Length),
            Last,
            Code,
            From_Status);
         Item := Wide_Wide_Character'Val (Code);
      end;
   end Get;

   procedure Get_Immediate (File : File_Type; Item : out Wide_Character) is
      Available : Boolean;
   begin
      Get_Immediate (File, Item, Available, Wait => True);
   end Get_Immediate;

   procedure Get_Immediate (
      File : File_Type;
      Item : out Wide_Wide_Character)
   is
      Available : Boolean;
   begin
      Get_Immediate (File, Item, Available, Wait => True);
   end Get_Immediate;

   procedure Get_Immediate (
      File : File_Type;
      Item : out Wide_Character;
      Available : out Boolean) is
   begin
      Get_Immediate (File, Item, Available, Wait => False);
   end Get_Immediate;

   procedure Get_Immediate (
      File : File_Type;
      Item : out Wide_Wide_Character;
      Available : out Boolean) is
   begin
      Get_Immediate (File, Item, Available, Wait => False);
   end Get_Immediate;

   procedure Get_Immediate (
      File : File_Type;
      Item : out Wide_Character;
      Available : out Boolean;
      Wait : Boolean)
   is
      C : Wide_Wide_Character;
   begin
      Get_Immediate (File, C, Available, Wait); -- Wide_Wide
      if Available then
         Store_Second (File, C, Item);
      end if;
   end Get_Immediate;

   procedure Get_Immediate (
      File : File_Type;
      Item : out Wide_Wide_Character;
      Available : out Boolean;
      Wait : Boolean)
   is
      S : String (1 .. 6);
      Read_Last : Natural;
      Length : Natural;
      From_Status : System.UTF_Conversions.From_Status_Type; -- ignore
   begin
      Get_Immediate (
         Reference (File).all,
         S (1 .. 1),
         Read_Last,
         Wait);
      Available := Read_Last > 0;
      System.UTF_Conversions.UTF_8_Sequence (S (1), Length, From_Status);
      declare
         Last : Natural;
         Code : System.UTF_Conversions.UCS_4;
      begin
         if Length >= 2 then
            Get_Immediate (
               Reference (File).all,
               S (2 .. Length),
               Read_Last,
               Wait => True);
         end if;
         System.UTF_Conversions.From_UTF_8 (
            S (1 .. Length),
            Last,
            Code,
            From_Status);
         Item := Wide_Wide_Character'Val (Code);
      end;
   end Get_Immediate;

   procedure Look_Ahead (
      File : File_Type;
      Item : out Wide_Character;
      End_Of_Line : out Boolean)
   is
      C : Wide_Wide_Character;
   begin
      Look_Ahead (File, C, End_Of_Line); -- Wide_Wide
      if End_Of_Line then
         Item := Wide_Character'Val (0);
      else
         declare
            Wide_Buffer : Wide_String (1 .. 2);
            Wide_Last : Natural;
            To_Status : System.UTF_Conversions.To_Status_Type; -- ignore
         begin
            System.UTF_Conversions.To_UTF_16 (
               Wide_Wide_Character'Pos (C),
               Wide_Buffer,
               Wide_Last,
               To_Status);
            Item := Wide_Buffer (1);
         end;
      end if;
   end Look_Ahead;

   procedure Look_Ahead (
      File : File_Type;
      Item : out Wide_Wide_Character;
      End_Of_Line : out Boolean)
   is
      C : Character;
   begin
      Look_Ahead (File, C, End_Of_Line);
      if End_Of_Line then
         Item := Wide_Wide_Character'Val (0);
      else
         declare
            Length : Natural;
            From_Status : System.UTF_Conversions.From_Status_Type; -- ignore
         begin
            System.UTF_Conversions.UTF_8_Sequence (C, Length, From_Status);
            declare
               S : String (1 .. Length);
               Last : Natural;
               EOL : Boolean;
               Conv_Last : Natural;
               Code : System.UTF_Conversions.UCS_4;
            begin
               Look_Ahead (Reference (File).all, S, Last, EOL);
               System.UTF_Conversions.From_UTF_8 (
                  S (1 .. Last),
                  Conv_Last,
                  Code,
                  From_Status);
               Item := Wide_Wide_Character'Val (Code);
            end;
         end;
      end if;
   end Look_Ahead;

   procedure Put (File : File_Type; Item : Wide_Character) is
      Ref_Text : constant access Text_Type := Reference (File).all;
      From_Status : System.UTF_Conversions.From_Status_Type; -- ignore
   begin
      if Ref_Text.Last > 0 then
         declare
            First : System.UTF_Conversions.UCS_4;
            Code : System.UTF_Conversions.UCS_4;
         begin
            declare
               Last : Natural;
               Length : Natural;
            begin
               --  restore first of surrogate pair
               System.UTF_Conversions.From_UTF_8 (
                  Ref_Text.Buffer (1 .. Ref_Text.Last),
                  Last,
                  First,
                  From_Status);
               System.UTF_Conversions.UTF_16_Sequence (
                  Wide_Character'Val (First),
                  Length,
                  From_Status);
               if Length /= 2
                  or else First >= 16#ffff#
                  or else Last /= Ref_Text.Last
               then
                  --  previous data is wrong
                  Exceptions.Raise_Exception_From_Here (Data_Error'Identity);
               end if;
            end;
            declare
               Wide_Buffer : Wide_String (1 .. 2);
               Wide_Last : Natural;
            begin
               Wide_Buffer (1) := Wide_Character'Val (First);
               Wide_Buffer (2) := Item;
               System.UTF_Conversions.From_UTF_16 (
                  Wide_Buffer,
                  Wide_Last,
                  Code,
                  From_Status);
            end;
            Ref_Text.Last := 0;
            Put (File, Wide_Wide_Character'Val (Code));
         end;
      else
         declare
            Length : Natural;
            To_Status : System.UTF_Conversions.To_Status_Type; -- ignore
         begin
            System.UTF_Conversions.UTF_16_Sequence (Item, Length, From_Status);
            if Length = 2 then
               --  store first of surrogate pair
               System.UTF_Conversions.To_UTF_8 (
                  Wide_Character'Pos (Item),
                  Ref_Text.Buffer,
                  Ref_Text.Last,
                  To_Status);
            else
               --  single character
               Put (File, Wide_Wide_Character'Val (Wide_Character'Pos (Item)));
            end if;
         end;
      end if;
   end Put;

   procedure Put (File : File_Type; Item : Wide_Wide_Character) is
      Ref_Text : constant access Text_Type := Reference (File).all;
   begin
      if Ref_Text.Last > 0 then
         --  previous data is rested
         Exceptions.Raise_Exception_From_Here (Data_Error'Identity);
      else
         declare
            Buffer : String (1 .. System.UTF_Conversions.UTF_8_Max_Length);
            Last : Natural;
            To_Status : System.UTF_Conversions.To_Status_Type; -- ignore
         begin
            System.UTF_Conversions.To_UTF_8 (
               Wide_Wide_Character'Pos (Item),
               Buffer,
               Last,
               To_Status);
            Put (File, Buffer (1 .. Last));
         end;
      end if;
   end Put;

end Ada.Text_IO.Inside.Wide;
