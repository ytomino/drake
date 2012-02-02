with Ada.Text_IO.Inside.Formatting;
package body Ada.Text_IO.Enumeration_IO is

   procedure Put_To_Field (
      To : out String;
      Last : out Natural;
      Item : Enum;
      Set : Type_Set);
   procedure Put_To_Field (
      To : out String;
      Last : out Natural;
      Item : Enum;
      Set : Type_Set)
   is
      Image : String := Enum'Image (Item);
   begin
      if Image (Image'First) /= ''' then
         case Set is
            when Upper_Case =>
               null;
            when Lower_Case =>
               for I in Image'Range loop
                  if Image (I) in 'A' .. 'Z' then
                     Image (I) := Character'Val (
                        Character'Pos (Image (I)) + 16#20#);
                  end if;
               end loop;
         end case;
      end if;
      Last := To'First + Image'Length - 1;
      To (To'First .. Last) := Image;
   end Put_To_Field;

   procedure Get_From_Field (
      From : String;
      Item : out Enum);
   procedure Get_From_Field (
      From : String;
      Item : out Enum) is
   begin
      Item := Enum'Value (From);
      if Item < Enum'First or else Item > Enum'Last then
         raise Data_Error;
      end if;
   exception
      when Constraint_Error =>
         raise Data_Error;
   end Get_From_Field;

   --  implementation

   procedure Get (
      File : File_Type;
      Item : out Enum)
   is
      S : constant String := Inside.Formatting.Get_Enum_Literal (File);
   begin
      Get_From_Field (S, Item);
   end Get;

   procedure Get (
      Item : out Enum) is
   begin
      Get (Current_Input.all, Item);
   end Get;

   procedure Get (
      File : not null File_Access;
      Item : out Enum) is
   begin
      Get (File.all, Item);
   end Get;

   procedure Put (
      File : File_Type;
      Item : Enum;
      Width : Field := Default_Width;
      Set : Type_Set := Default_Setting)
   is
      S : String (1 .. Enum'Width);
      Last : Natural;
   begin
      Put_To_Field (S, Last, Item, Set);
      Inside.Formatting.Head (File, S (1 .. Last), Width);
   end Put;

   procedure Put (
      Item : Enum;
      Width : Field := Default_Width;
      Set : Type_Set := Default_Setting) is
   begin
      Put (Current_Output.all, Item, Width, Set);
   end Put;

   procedure Put (
      File : not null File_Access;
      Item : Enum;
      Width : Field := Default_Width;
      Set : Type_Set := Default_Setting) is
   begin
      Put (File.all, Item, Width, Set);
   end Put;

   procedure Get (
      From : String;
      Item : out Enum;
      Last : out Positive)
   is
      First : Positive;
   begin
      Inside.Formatting.Get_Head (From, First, Last);
      Get_From_Field (From (First .. Last), Item);
   end Get;

   procedure Put (
      To : out String;
      Item : Enum;
      Set : Type_Set := Default_Setting)
   is
      S : String (1 .. Enum'Width);
      Last : Natural;
   begin
      Put_To_Field (S, Last, Item, Set);
      Inside.Formatting.Head (To, S (1 .. Last));
   end Put;

end Ada.Text_IO.Enumeration_IO;
