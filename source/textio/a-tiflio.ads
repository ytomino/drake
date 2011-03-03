pragma License (Unrestricted);
--  separated and auto-loaded by compiler
generic
   type Num is digits <>;
package Ada.Text_IO.Float_IO is

   Default_Fore : Field := 2;
   Default_Aft : Field := Num'Digits - 1;
   Default_Exp : Field := 3;

   procedure Get (
      File : File_Type;
      Item : out Num;
      Width : Field := 0);
   procedure Get (
      Item : out Num;
      Width : Field := 0);
   --  extended
   procedure Get (
      File : not null File_Access;
      Item : out Num;
      Width : Field := 0);

   procedure Put (
      File : File_Type;
      Item : Num;
      Fore : Field := Default_Fore;
      Aft : Field := Default_Aft;
      Exp : Field := Default_Exp);
   procedure Put (
      Item : Num;
      Fore : Field := Default_Fore;
      Aft : Field := Default_Aft;
      Exp : Field := Default_Exp);
   --  extended
   procedure Put (
      File : not null File_Access;
      Item : Num;
      Fore : Field := Default_Fore;
      Aft : Field := Default_Aft;
      Exp : Field := Default_Exp);

   procedure Get (
      From : String;
      Item : out Num;
      Last : out Positive);
   procedure Put (
      To : out String;
      Item : Num;
      Aft : Field := Default_Aft;
      Exp : Field := Default_Exp);

   --  extended for Complex_IO
   procedure Put (
      To : out String;
      Last : out Natural;
      Item : Num;
      Aft : Field := Default_Aft;
      Exp : Field := Default_Exp);

end Ada.Text_IO.Float_IO;
