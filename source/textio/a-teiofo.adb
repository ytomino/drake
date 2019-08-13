with Ada.Exception_Identification.From_Here;
with Ada.Exceptions.Finally;
with System.Formatting;
with System.UTF_Conversions;
package body Ada.Text_IO.Formatting is
   use Exception_Identification.From_Here;
   use type System.Long_Long_Integer_Types.Word_Integer;
   use type System.Long_Long_Integer_Types.Word_Unsigned;
   use type System.Long_Long_Integer_Types.Long_Long_Unsigned;

   subtype Word_Integer is System.Long_Long_Integer_Types.Word_Integer;
   subtype Word_Unsigned is System.Long_Long_Integer_Types.Word_Unsigned;
   subtype Long_Long_Unsigned is
      System.Long_Long_Integer_Types.Long_Long_Unsigned;

   procedure Skip_Spaces (
      File : File_Type); -- Input_File_Type
   procedure Skip_Spaces (
      File : File_Type)
   is
      Item : Character;
      End_Of_Line : Boolean;
   begin
      loop
         Look_Ahead (File, Item, End_Of_Line); -- checking the predicate
         exit when not End_Of_Line
            and then Item /= ' '
            and then Item /= Character'Val (9);
         Skip_Ahead (File);
      end loop;
   end Skip_Spaces;

   procedure Adjust (
      File : File_Type; -- Output_File_Type
      Width : Field);
   procedure Adjust (
      File : File_Type;
      Width : Field)
   is
      Line : constant Count := Line_Length (File); -- checking the predicate
   begin
      if Line > 0 then
         if Count (Width) > Line then
            Raise_Exception (Layout_Error'Identity);
         elsif Col (File) + Count (Width) - 1 > Line then
            New_Line (File);
         end if;
      end if;
   end Adjust;

   procedure Add (
      Buffer : in out String_Access;
      Last : in out Natural;
      Item : Character);
   procedure Add (
      Buffer : in out String_Access;
      Last : in out Natural;
      Item : Character) is
   begin
      Last := Last + 1;
      if Last > Buffer'Last then
         Reallocate (Buffer, 1, String_Grow (Buffer'Last)); -- Buffer'First = 1
      end if;
      Buffer (Last) := Item;
   end Add;

   procedure Get_Num (
      File : File_Type; -- Input_File_Type
      Buffer : in out String_Access;
      Last : in out Natural;
      Based : Boolean);
   procedure Get_Num (
      File : File_Type;
      Buffer : in out String_Access;
      Last : in out Natural;
      Based : Boolean)
   is
      Start : constant Natural := Last;
      Item : Character;
      End_Of_Line : Boolean;
   begin
      Look_Ahead (File, Item, End_Of_Line); -- checking the predicate
      loop
         if Item = '_' then
            exit when Last = Start;
            Add (Buffer, Last, Item);
            Skip_Ahead (File);
            Look_Ahead (File, Item, End_Of_Line);
         end if;
         exit when Item not in '0' .. '9'
            and then (
               not Based
               or else (
                  Item not in 'A' .. 'F' and then Item not in 'a' .. 'f'));
         Add (Buffer, Last, Item);
         Skip_Ahead (File);
         Look_Ahead (File, Item, End_Of_Line);
      end loop;
   end Get_Num;

   procedure Get_Numeric_Literal_To_Buffer (
      File : File_Type; -- Input_File_Type
      Buffer : in out String_Access;
      Last : in out Natural;
      Real : Boolean);
   procedure Get_Numeric_Literal_To_Buffer (
      File : File_Type; -- Input_File_Type
      Buffer : in out String_Access;
      Last : in out Natural;
      Real : Boolean)
   is
      Prev_Last : Natural;
      Mark : Character;
      Item : Character;
      End_Of_Line : Boolean;
   begin
      Look_Ahead (File, Item, End_Of_Line);
      if Item = '+' or else Item = '-' then
         Add (Buffer, Last, Item);
         Skip_Ahead (File);
         Look_Ahead (File, Item, End_Of_Line);
      end if;
      Prev_Last := Last;
      Get_Num (File, Buffer, Last, Based => False);
      if Last > Prev_Last then
         Look_Ahead (File, Item, End_Of_Line);
         if Item = '#' or else Item = ':' then
            Mark := Item;
            Add (Buffer, Last, Item);
            Skip_Ahead (File);
            Get_Num (File, Buffer, Last, Based => True);
            Look_Ahead (File, Item, End_Of_Line);
            if Item = '.' and then Real then
               Add (Buffer, Last, Item);
               Skip_Ahead (File);
               Get_Num (File, Buffer, Last, Based => False);
               Look_Ahead (File, Item, End_Of_Line);
            end if;
            if Item = Mark then
               Add (Buffer, Last, Item);
               Skip_Ahead (File);
               Look_Ahead (File, Item, End_Of_Line);
            end if;
         elsif Item = '.' and then Real then
            Add (Buffer, Last, Item);
            Skip_Ahead (File);
            Get_Num (File, Buffer, Last, Based => False);
            Look_Ahead (File, Item, End_Of_Line);
         end if;
         if Item = 'E' or else Item = 'e' then
            Add (Buffer, Last, Item);
            Skip_Ahead (File);
            Look_Ahead (File, Item, End_Of_Line);
            if Item = '+' or else Item = '-' then
               Add (Buffer, Last, Item);
               Skip_Ahead (File);
               Look_Ahead (File, Item, End_Of_Line);
            end if;
            Get_Num (File, Buffer, Last, Based => False);
         end if;
      end if;
   end Get_Numeric_Literal_To_Buffer;

   --  implementation

   procedure Integer_Image (
      To : out String;
      Last : out Natural;
      Item : System.Long_Long_Integer_Types.Word_Integer;
      Base : Number_Base)
   is
      Unsigned_Item : Word_Unsigned;
   begin
      Last := To'First - 1;
      if Item < 0 then
         Last := Last + 1;
         To (Last) := '-';
         Unsigned_Item := -Word_Unsigned'Mod (Item);
      else
         Unsigned_Item := Word_Unsigned (Item);
      end if;
      Modular_Image (To (Last + 1 .. To'Last), Last, Unsigned_Item, Base);
   end Integer_Image;

   procedure Integer_Image (
      To : out String;
      Last : out Natural;
      Item : Long_Long_Integer;
      Base : Number_Base) is
   begin
      if Standard'Word_Size < Long_Long_Unsigned'Size then
         --  optimized for 32bit
         declare
            Unsigned_Item : Long_Long_Unsigned;
         begin
            Last := To'First - 1;
            if Item < 0 then
               Last := Last + 1;
               To (Last) := '-';
               Unsigned_Item := -Long_Long_Unsigned'Mod (Item);
            else
               Unsigned_Item := Long_Long_Unsigned (Item);
            end if;
            Modular_Image (
               To (Last + 1 .. To'Last),
               Last,
               Unsigned_Item,
               Base);
         end;
      else
         --  optimized for 64bit
         Integer_Image (To, Last, Word_Integer (Item), Base);
      end if;
   end Integer_Image;

   procedure Modular_Image (
      To : out String;
      Last : out Natural;
      Item : System.Long_Long_Integer_Types.Word_Unsigned;
      Base : Number_Base)
   is
      Error : Boolean;
   begin
      Last := To'First - 1;
      if Base /= 10 then
         System.Formatting.Image (
            Word_Unsigned (Base),
            To (Last + 1 .. To'Last),
            Last,
            Error => Error);
         if Error then
            Raise_Exception (Layout_Error'Identity);
         end if;
         Last := Last + 1;
         if Last > To'Last then
            Raise_Exception (Layout_Error'Identity);
         end if;
         To (Last) := '#';
      end if;
      System.Formatting.Image (
         Item,
         To (Last + 1 .. To'Last),
         Last,
         Base => Base,
         Error => Error);
      if Error then
         Raise_Exception (Layout_Error'Identity);
      end if;
      if Base /= 10 then
         Last := Last + 1;
         if Last > To'Last then
            Raise_Exception (Layout_Error'Identity);
         end if;
         To (Last) := '#';
      end if;
   end Modular_Image;

   procedure Modular_Image (
      To : out String;
      Last : out Natural;
      Item : System.Long_Long_Integer_Types.Long_Long_Unsigned;
      Base : Number_Base) is
   begin
      if Standard'Word_Size < Long_Long_Unsigned'Size then
         --  optimized for 32bit
         declare
            Error : Boolean;
         begin
            Last := To'First - 1;
            if Base /= 10 then
               System.Formatting.Image (
                  Word_Unsigned (Base),
                  To (Last + 1 .. To'Last),
                  Last,
                  Error => Error);
               if Error then
                  Raise_Exception (Layout_Error'Identity);
               end if;
               Last := Last + 1;
               if Last > To'Last then
                  Raise_Exception (Layout_Error'Identity);
               end if;
               To (Last) := '#';
            end if;
            System.Formatting.Image (
               Item,
               To (Last + 1 .. To'Last),
               Last,
               Base => Base,
               Error => Error);
            if Error then
               Raise_Exception (Layout_Error'Identity);
            end if;
            if Base /= 10 then
               Last := Last + 1;
               if Last > To'Last then
                  Raise_Exception (Layout_Error'Identity);
               end if;
               To (Last) := '#';
            end if;
         end;
      else
         --  optimized for 64bit
         Modular_Image (To, Last, Word_Unsigned (Item), Base);
      end if;
   end Modular_Image;

   function Get_Numeric_Literal (
      File : File_Type;
      Real : Boolean)
      return String is
   begin
      Skip_Spaces (File); -- checking the predicate
      declare
         Buffer : aliased String_Access := new String (1 .. 256);
         Last : Natural := 0;
         package Holder is
            new Exceptions.Finally.Scoped_Holder (String_Access, Free);
      begin
         Holder.Assign (Buffer);
         Get_Numeric_Literal_To_Buffer (File, Buffer, Last, Real => Real);
         return Buffer (1 .. Last);
      end;
   end Get_Numeric_Literal;

   function Get_Complex_Literal (
      File : File_Type)
      return String is
   begin
      Skip_Spaces (File); -- checking the predicate
      declare
         Buffer : aliased String_Access := new String (1 .. 256);
         Last : Natural := 0;
         package Holder is
            new Exceptions.Finally.Scoped_Holder (String_Access, Free);
      begin
         Holder.Assign (Buffer);
         declare
            Item : Character;
            End_Of_Line : Boolean;
            Paren : Boolean;
         begin
            Look_Ahead (File, Item, End_Of_Line);
            Paren := Item = '(';
            if Paren then
               Add (Buffer, Last, Item);
               Skip_Ahead (File);
               Skip_Spaces (File);
            end if;
            Get_Numeric_Literal_To_Buffer (File, Buffer, Last, Real => True);
            Skip_Spaces (File);
            Look_Ahead (File, Item, End_Of_Line);
            if Item = ',' then
               Add (Buffer, Last, Item);
               Skip_Ahead (File);
               Skip_Spaces (File);
            else
               Add (Buffer, Last, ' ');
            end if;
            Get_Numeric_Literal_To_Buffer (File, Buffer, Last, Real => True);
            if Paren then
               Skip_Spaces (File);
               Look_Ahead (File, Item, End_Of_Line);
               if Item = ')' then
                  Add (Buffer, Last, Item);
                  Skip_Ahead (File);
               else
                  Raise_Exception (Data_Error'Identity);
               end if;
            end if;
         end;
         return Buffer (1 .. Last);
      end;
   end Get_Complex_Literal;

   function Get_Enum_Literal (
      File : File_Type)
      return String is
   begin
      Skip_Spaces (File); -- checking the predicate
      declare
         Buffer : aliased String_Access := new String (1 .. 256);
         Last : Natural := 0;
         package Holder is
            new Exceptions.Finally.Scoped_Holder (String_Access, Free);
      begin
         Holder.Assign (Buffer);
         declare
            Item : Character;
            End_Of_Line : Boolean;
         begin
            Look_Ahead (File, Item, End_Of_Line);
            if Item = ''' then
               Add (Buffer, Last, Item);
               Skip_Ahead (File);
               Look_Ahead (File, Item, End_Of_Line);
               if not End_Of_Line then
                  declare
                     Length : Natural;
                     Sequence_Status :
                        System.UTF_Conversions.Sequence_Status_Type; -- ignore
                  begin
                     System.UTF_Conversions.UTF_8_Sequence (
                        Item,
                        Length,
                        Sequence_Status);
                     Add (Buffer, Last, Item);
                     Skip_Ahead (File);
                     for I in 2 .. Length loop
                        Look_Ahead (File, Item, End_Of_Line);
                        exit when End_Of_Line;
                        Add (Buffer, Last, Item);
                        Skip_Ahead (File);
                     end loop;
                  end;
                  Look_Ahead (File, Item, End_Of_Line);
                  if Item = ''' then
                     Add (Buffer, Last, Item);
                     Skip_Ahead (File);
                  end if;
               end if;
            elsif Item in 'A' .. 'Z'
               or else Item in 'a' .. 'z'
               or else Item >= Character'Val (16#80#)
            then
               while not End_Of_Line
                  and then Item /= ' '
                  and then Item /= Character'Val (9)
               loop
                  if Item = '_' then
                     Add (Buffer, Last, Item);
                     Skip_Ahead (File);
                     Look_Ahead (File, Item, End_Of_Line);
                  end if;
                  exit when not (
                     Item in 'A' .. 'Z'
                     or else Item in 'a' .. 'z'
                     or else Item in '0' .. '9'
                     or else Item >= Character'Val (16#80#));
                  Add (Buffer, Last, Item);
                  Skip_Ahead (File);
                  Look_Ahead (File, Item, End_Of_Line);
               end loop;
            end if;
         end;
         return Buffer (1 .. Last);
      end;
   end Get_Enum_Literal;

   procedure Get_Field (
      File : File_Type;
      Item : out String;
      Last : out Natural)
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Open (File) or else raise Status_Error);
      pragma Check (Dynamic_Predicate,
         Check => Mode (File) = In_File or else raise Mode_Error);
      Has_Data : Boolean := False;
      End_Of_Line : Boolean;
   begin
      Last := Item'First - 1;
      for I in Item'Range loop
         Look_Ahead (File, Item (I), End_Of_Line);
         exit when End_Of_Line;
         Skip_Ahead (File);
         if Item (I) = Character'Val (9) then
            Item (I) := ' '; -- treat tab as space, RM A.10.6(5/2)
         elsif Item (I) /= ' ' then
            Has_Data := True;
         end if;
         Last := I;
      end loop;
      if not Has_Data then
         if End_Of_File (File) then
            Raise_Exception (End_Error'Identity);
         else
            Raise_Exception (Data_Error'Identity);
         end if;
      end if;
   end Get_Field;

   procedure Head (
      File : File_Type;
      Item : String;
      Width : Field) is
   begin
      Adjust (File, Field'Max (Width, Item'Last)); -- checking the predicate
      Put (File, Item);
      for I in Item'Length + 1 .. Width loop
         Put (File, ' ');
      end loop;
   end Head;

   procedure Tail (
      File : File_Type;
      Item : String;
      Width : Field) is
   begin
      Adjust (File, Field'Max (Width, Item'Last)); -- checking the predicate
      for I in Item'Length + 1 .. Width loop
         Put (File, ' ');
      end loop;
      Put (File, Item);
   end Tail;

   procedure Get_Head (
      Item : String;
      First : out Positive;
      Last : out Natural) is
   begin
      Get_Tail (Item, First); -- skip first spaces
      Last := First;
      while Last < Item'Last
         and then Item (Last + 1) /= ' '
         and then Item (Last + 1) /= Character'Val (9)
      loop
         Last := Last + 1;
      end loop;
   end Get_Head;

   procedure Get_Tail (Item : String; First : out Positive) is
   begin
      First := Item'First;
      loop
         if First > Item'Last then
            Raise_Exception (End_Error'Identity);
         end if;
         exit when Item (First) /= ' '
            and then Item (First) /= Character'Val (9);
         First := First + 1;
      end loop;
   end Get_Tail;

   procedure Head (Target : out String; Source : String) is
      Source_Length : constant Natural := Source'Length;
   begin
      if Target'Length < Source_Length then
         Raise_Exception (Layout_Error'Identity);
      end if;
      Target (Target'First .. Target'First + Source_Length - 1) := Source;
      System.Formatting.Fill_Padding (
         Target (Target'First + Source_Length .. Target'Last),
         ' ');
   end Head;

   procedure Tail (Target : out String; Source : String) is
      Source_Length : constant Natural := Source'Length;
   begin
      if Target'Length < Source_Length then
         Raise_Exception (Layout_Error'Identity);
      end if;
      Target (Target'Last - Source_Length + 1 .. Target'Last) := Source;
      System.Formatting.Fill_Padding (
         Target (Target'First .. Target'Last - Source_Length),
         ' ');
   end Tail;

   procedure Tail (Target : out Wide_String; Source : Wide_String) is
      Source_Length : constant Natural := Source'Length;
   begin
      if Target'Length < Source_Length then
         Raise_Exception (Layout_Error'Identity);
      end if;
      for I in Target'First .. Target'Last - Source_Length loop
         Target (I) := ' ';
      end loop;
      Target (Target'Last - Source_Length + 1 .. Target'Last) := Source;
   end Tail;

   procedure Tail (Target : out Wide_Wide_String; Source : Wide_Wide_String) is
      Source_Length : constant Natural := Source'Length;
   begin
      if Target'Length < Source_Length then
         Raise_Exception (Layout_Error'Identity);
      end if;
      for I in Target'First .. Target'Last - Source_Length loop
         Target (I) := ' ';
      end loop;
      Target (Target'Last - Source_Length + 1 .. Target'Last) := Source;
   end Tail;

end Ada.Text_IO.Formatting;
