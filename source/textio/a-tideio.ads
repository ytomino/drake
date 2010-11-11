pragma License (Unrestricted);
--  separated and auto-loaded by compiler
generic
   type Num is delta <> digits <>;
package Ada.Text_IO.Decimal_IO is

   Default_Fore : Field := Num'Fore;
   Default_Aft : Field := Num'Aft;
   Default_Exp : Field := 0;

--  procedure Get (
--    File : File_Type;
--    Item : out Num;
--    Width : Field := 0);
--  procedure Get (
--    Item : out Num;
--    Width : Field := 0);

--  procedure Put (
--    File : File_Type;
--    Item : Num;
--    Fore : Field := Default_Fore;
--    Aft : Field := Default_Aft;
--    Exp : Field := Default_Exp);
--  procedure Put (
--    Item : Num;
--    Fore : Field := Default_Fore;
--    Aft : Field := Default_Aft;
--    Exp : Field := Default_Exp);

--  procedure Get (
--    From : String;
--    Item : out Num;
--    Last : out Positive);
--  procedure Put (
--    To : out String;
--    Item : Num;
--    Aft : Field := Default_Aft;
--    Exp : Field := Default_Exp);

end Ada.Text_IO.Decimal_IO;
