pragma License (Unrestricted);
with Ada.Numerics.Generic_Complex_Types;
with Ada.Text_IO.Complex_IO;
generic
   with package Complex_Types is new Numerics.Generic_Complex_Types (<>);
package Ada.Wide_Wide_Text_IO.Complex_IO is
--  use Complex_Types;

   --  for renaming
   package Strings is new Text_IO.Complex_IO (Complex_Types);

   Default_Fore : Field -- := 2;
      renames Strings.Default_Fore;
   Default_Aft : Field -- := Complex_Types.Real'Digits - 1;
      renames Strings.Default_Aft;
   Default_Exp : Field -- := 3;
      renames Strings.Default_Exp;

   procedure Get (
      File : File_Type; -- Input_File_Type
      Item : out Complex_Types.Complex;
      Width : Field := 0)
      renames Strings.Get;
   procedure Get (
      Item : out Complex_Types.Complex;
      Width : Field := 0)
      renames Strings.Get;

   procedure Put (
      File : File_Type; -- Output_File_Type
      Item : Complex_Types.Complex;
      Fore : Field := Default_Fore;
      Aft : Field := Default_Aft;
      Exp : Field := Default_Exp)
      renames Strings.Put;
   procedure Put (
      Item : Complex_Types.Complex;
      Fore : Field := Default_Fore;
      Aft : Field := Default_Aft;
      Exp : Field := Default_Exp)
      renames Strings.Put;

   procedure Get (
      From : Wide_Wide_String;
      Item : out Complex_Types.Complex;
      Last : out Positive)
      renames Strings.Overloaded_Get;
   procedure Put (
      To : out Wide_Wide_String;
      Item : Complex_Types.Complex;
      Aft : Field := Default_Aft;
      Exp : Field := Default_Exp)
      renames Strings.Overloaded_Put;

end Ada.Wide_Wide_Text_IO.Complex_IO;
