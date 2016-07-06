pragma License (Unrestricted);
with Ada.Numerics.Generic_Complex_Types;
with Ada.Text_IO.Complex_IO;
generic
   with package Complex_Types is new Numerics.Generic_Complex_Types (<>);
package Ada.Wide_Text_IO.Complex_IO is
--  use Complex_Types;

   --  for renaming
   package Strings is new Text_IO.Complex_IO (Complex_Types);

   Default_Fore : Field
      renames Strings.Default_Fore; -- 2
   Default_Aft : Field
      renames Strings.Default_Aft; -- Complex_Types.Real'Digits - 1
   Default_Exp : Field
      renames Strings.Default_Exp; -- 3

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
      From : Wide_String;
      Item : out Complex_Types.Complex;
      Last : out Positive)
      renames Strings.Overloaded_Get;
   procedure Put (
      To : out Wide_String;
      Item : Complex_Types.Complex;
      Aft : Field := Default_Aft;
      Exp : Field := Default_Exp)
      renames Strings.Overloaded_Put;

end Ada.Wide_Text_IO.Complex_IO;
