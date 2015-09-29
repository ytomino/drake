pragma License (Unrestricted);
with Ada.Numerics.Generic_Complex_Types;
generic
   with package Complex_Types is new Numerics.Generic_Complex_Types (<>);
package Ada.Text_IO.Complex_IO is
--  use Complex_Types;

   Default_Fore : Field := 2;
   Default_Aft : Field := Complex_Types.Real'Digits - 1;
   Default_Exp : Field := 3;

   procedure Get (
      File : File_Type; -- Input_File_Type
      Item : out Complex_Types.Complex;
      Width : Field := 0);
   procedure Get (
      Item : out Complex_Types.Complex;
      Width : Field := 0);

   procedure Put (
      File : File_Type; -- Output_File_Type
      Item : Complex_Types.Complex;
      Fore : Field := Default_Fore;
      Aft : Field := Default_Aft;
      Exp : Field := Default_Exp);
   procedure Put (
      Item : Complex_Types.Complex;
      Fore : Field := Default_Fore;
      Aft : Field := Default_Aft;
      Exp : Field := Default_Exp);

   --  extended
   procedure Overloaded_Get (
      From : String;
      Item : out Complex_Types.Complex;
      Last : out Positive);
   procedure Overloaded_Get (
      From : Wide_String;
      Item : out Complex_Types.Complex;
      Last : out Positive);
   procedure Overloaded_Get (
      From : Wide_Wide_String;
      Item : out Complex_Types.Complex;
      Last : out Positive);

   --  extended
   procedure Overloaded_Put (
      To : out String;
      Item : Complex_Types.Complex;
      Aft : Field := Default_Aft;
      Exp : Field := Default_Exp);
   procedure Overloaded_Put (
      To : out Wide_String;
      Item : Complex_Types.Complex;
      Aft : Field := Default_Aft;
      Exp : Field := Default_Exp);
   procedure Overloaded_Put (
      To : out Wide_Wide_String;
      Item : Complex_Types.Complex;
      Aft : Field := Default_Aft;
      Exp : Field := Default_Exp);

   procedure Get (
      From : String;
      Item : out Complex_Types.Complex;
      Last : out Positive)
      renames Overloaded_Get;
   procedure Put (
      To : out String;
      Item : Complex_Types.Complex;
      Aft : Field := Default_Aft;
      Exp : Field := Default_Exp)
      renames Overloaded_Put;

end Ada.Text_IO.Complex_IO;
