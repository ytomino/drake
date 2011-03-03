with Ada.Text_IO.Float_IO;
with Ada.Text_IO.Inside.Formatting;
package body Ada.Text_IO.Complex_IO is

   package Real_IO is new Float_IO (Complex_Types.Real);

   procedure Get_From_Field (
      From : String;
      Item : out Complex_Types.Complex;
      Last : out Positive);
   procedure Get_From_Field (
      From : String;
      Item : out Complex_Types.Complex;
      Last : out Positive)
   is
      Paren : Boolean;
   begin
      Inside.Formatting.Get_Tail (From, Last);
      Paren := From (Last) = '(';
      if Paren then
         Last := Last + 1;
      end if;
      Real_IO.Get (From (Last .. From'Last), Item.Re, Last);
      Inside.Formatting.Get_Tail (From (Last + 1 .. From'Last), Last);
      if From (Last) = ',' then
         Last := Last + 1;
      end if;
      Real_IO.Get (From (Last .. From'Last), Item.Im, Last);
      if Paren then
         Inside.Formatting.Get_Tail (From (Last + 1 .. From'Last), Last);
         if From (Last) /= ')' then
            raise Data_Error;
         end if;
      end if;
   end Get_From_Field;

   procedure Get (
      File : File_Type;
      Item : out Complex_Types.Complex;
      Width : Field := 0) is
   begin
      if Width /= 0 then
         declare
            S : String (1 .. Width);
            Last_1 : Natural;
            Last_2 : Natural;
         begin
            Inside.Formatting.Get_Field (File, S, Last_1);
            Get_From_Field (S (1 .. Last_1), Item, Last_2);
            if Last_2 /= Last_1 then
               raise Data_Error;
            end if;
         end;
      else
         declare
            S : constant String :=
               Inside.Formatting.Get_Complex_Literal (File);
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
      Item : out Complex_Types.Complex;
      Width : Field := 0) is
   begin
      Get (Current_Input.all, Item, Width);
   end Get;

   procedure Put (
      File : File_Type;
      Item : Complex_Types.Complex;
      Fore : Field := Default_Fore;
      Aft : Field := Default_Aft;
      Exp : Field := Default_Exp) is
   begin
      Put (File, '(');
      Real_IO.Put (File, Item.Re, Fore, Aft, Exp);
      Put (File, ',');
      Real_IO.Put (File, Item.Im, Fore, Aft, Exp);
      Put (File, ')');
   end Put;

   procedure Put (
      Item : Complex_Types.Complex;
      Fore : Field := Default_Fore;
      Aft : Field := Default_Aft;
      Exp : Field := Default_Exp) is
   begin
      Put (Current_Output.all, Item, Fore, Aft, Exp);
   end Put;

   procedure Get (
      From : String;
      Item : out Complex_Types.Complex;
      Last : out Positive)
      renames Get_From_Field;

   procedure Put (
      To : out String;
      Item : Complex_Types.Complex;
      Aft : Field := Default_Aft;
      Exp : Field := Default_Exp)
   is
      Index : Natural;
   begin
      if To'Length < (Aft + Exp) * 2 + 7 then -- "(0.,0.)"
         raise Layout_Error;
      end if;
      To (To'First) := '(';
      Real_IO.Put (To (To'First + 1 .. To'Last), Index, Item.Re, Aft, Exp);
      Index := Index + 1;
      if Index > To'Last then
         raise Layout_Error;
      end if;
      To (Index) := ',';
      Real_IO.Put (To (Index + 1 .. To'Last - 1), Item.Im, Aft, Exp);
      To (To'Last) := ')';
   end Put;

end Ada.Text_IO.Complex_IO;
