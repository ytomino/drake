with Ada;
procedure System.Formatting.Address_Image (
   Value : Address;
   Item : out String;
   Last : out Natural;
   Set : Type_Set := Upper_Case)
is
   pragma Suppress (All_Checks);
   procedure Runtime_Error (
      Condition : Boolean;
      S : String;
      Source_Location : String := Ada.Debug.Source_Location;
      Enclosing_Entity : String := Ada.Debug.Enclosing_Entity);
   pragma Import (Ada, Runtime_Error, "__drake_runtime_error");
   Use_Longest : constant Boolean :=
      Standard'Address_Size > Formatting.Unsigned'Size;
   Width : constant Natural := (Standard'Address_Size + 3) / 4;
   Error : Boolean;
begin
   if Use_Longest then
      Image (
         Longest_Unsigned (Value),
         Item,
         Last,
         Base => 16,
         Set => Set,
         Width => Width,
         Error => Error);
   else
      Image (
         Unsigned (Value),
         Item,
         Last,
         Base => 16,
         Set => Set,
         Width => Width,
         Error => Error);
   end if;
   pragma Debug (Runtime_Error (Error, "Item'Length is short"));
end System.Formatting.Address_Image;
