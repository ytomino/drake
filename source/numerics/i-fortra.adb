with System.UTF_Conversions;
package body Interfaces.Fortran is
   pragma Suppress (All_Checks);
   use type System.UTF_Conversions.From_Status_Type;
   use type System.UTF_Conversions.To_Status_Type;
   use type System.UTF_Conversions.UCS_4;

   function To_Fortran (
      Item : Character;
      Substitute : Character_Set := '?')
      return Character_Set is
   begin
      if Item < Character'Val (16#80#) then
         return Character_Set (Item);
      else
         return Substitute;
      end if;
   end To_Fortran;

   function To_Ada (
      Item : Character_Set;
      Substitute : Character := '?')
      return Character is
   begin
      if Item  < Character_Set'Val (16#80#) then
         return Character (Item);
      else
         return Substitute;
      end if;
   end To_Ada;

   function To_Fortran (
      Item : String;
      Substitute : Fortran_Character := "?")
      return Fortran_Character
   is
      Result : Fortran_Character (1 .. Item'Length * Substitute'Length);
      Last : Natural;
   begin
      To_Fortran (Item, Result, Last, Substitute => Substitute);
      return Result (Result'First .. Last);
   end To_Fortran;

   function To_Ada (
      Item : Fortran_Character;
      Substitute : String := "?")
      return String
   is
      pragma Unreferenced (Substitute);
      Expanding : constant := 2; -- Latin-1 to UTF-8
      Result : String (
         1 ..
         Item'Length
            * Expanding); -- Integer'Max (Expanding, Substitute'Length)
      Last : Natural;
   begin
      To_Ada (Item, Result, Last,
         Substitute => ""); -- unreferenced
      return Result (Result'First .. Last);
   end To_Ada;

   procedure To_Fortran (
      Item : String;
      Target : out Fortran_Character;
      Last : out Natural;
      Substitute : Fortran_Character := "?")
   is
      Item_Last : Natural := Item'First - 1;
   begin
      Last := Target'First - 1;
      while Item_Last < Item'Last loop
         declare
            Code : System.UTF_Conversions.UCS_4;
            From_Status : System.UTF_Conversions.From_Status_Type;
         begin
            System.UTF_Conversions.From_UTF_8 (
               Item (Item_Last + 1 .. Item'Last),
               Item_Last,
               Code,
               From_Status);
            if From_Status = System.UTF_Conversions.Success
               and then Code < 16#100#
            then
               declare
                  New_Last : constant Natural := Last + 1;
               begin
                  if New_Last > Target'Last then
                     raise Constraint_Error; -- overflow
                  end if;
                  Target (New_Last) := Character_Set'Val (Code);
                  Last := New_Last;
               end;
            else
               declare
                  New_Last : constant Natural := Last + Substitute'Length;
               begin
                  if New_Last > Target'Last then
                     raise Constraint_Error; -- overflow
                  end if;
                  Target (Last + 1 .. New_Last) := Substitute;
                  Last := New_Last;
               end;
            end if;
         end;
      end loop;
   end To_Fortran;

   procedure To_Ada (
      Item : Fortran_Character;
      Target : out String;
      Last : out Natural;
      Substitute : String := "?")
   is
      pragma Unreferenced (Substitute);
      Item_Last : Natural := Item'First - 1;
   begin
      Last := Target'First - 1;
      while Item_Last < Item'Last loop
         declare
            Code : System.UTF_Conversions.UCS_4;
            To_Status : System.UTF_Conversions.To_Status_Type;
         begin
            Item_Last := Item_Last + 1;
            Code := Character_Set'Pos (Item (Item_Last));
            System.UTF_Conversions.To_UTF_8 (
               Code,
               Target (Last + 1 .. Target'Last),
               Last,
               To_Status);
            if To_Status /= System.UTF_Conversions.Success then
               raise Constraint_Error; -- overflow
            end if;
         end;
      end loop;
   end To_Ada;

end Interfaces.Fortran;
