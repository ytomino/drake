package body System.Val_Enum is
   pragma Suppress (All_Checks);

   function Value_Enumeration_8 (
      Names : String;
      Indexes : Address;
      Num : Natural;
      Str : String)
      return Natural
   is
      type Index_Type is mod 2 ** 8;
      type Array_Type is array (Natural) of Index_Type;
      Indexes_2 : Array_Type;
      for Indexes_2'Address use Indexes;
      First : Positive;
      Last : Natural;
   begin
      Trim (Str, First, Last);
      declare
         S : String := Str (First .. Last);
      begin
         if S (First) /= ''' then
            To_Upper (S);
         end if;
         for I in 0 .. Num loop
            declare
               First : constant Natural := Natural (Indexes_2 (I));
               Next : constant Natural := Natural (Indexes_2 (I + 1));
            begin
               if S = Names (First .. Next - 1) then
                  return I;
               end if;
            end;
         end loop;
      end;
      raise Constraint_Error;
   end Value_Enumeration_8;

   function Value_Enumeration_16 (
      Names : String;
      Indexes : Address;
      Num : Natural;
      Str : String)
      return Natural
   is
      type Index_Type is mod 2 ** 16;
      type Array_Type is array (Natural) of Index_Type;
      Indexes_2 : Array_Type;
      for Indexes_2'Address use Indexes;
      First : Positive;
      Last : Natural;
   begin
      Trim (Str, First, Last);
      declare
         S : String := Str (First .. Last);
      begin
         if S (First) /= ''' then
            To_Upper (S);
         end if;
         for I in 0 .. Num loop
            declare
               First : constant Natural := Natural (Indexes_2 (I));
               Next : constant Natural := Natural (Indexes_2 (I + 1));
            begin
               if S = Names (First .. Next - 1) then
                  return I;
               end if;
            end;
         end loop;
      end;
      raise Constraint_Error;
   end Value_Enumeration_16;

   function Value_Enumeration_32 (
      Names : String;
      Indexes : Address;
      Num : Natural;
      Str : String)
      return Natural
   is
      type Index_Type is mod 2 ** 32;
      type Array_Type is array (Natural) of Index_Type;
      Indexes_2 : Array_Type;
      for Indexes_2'Address use Indexes;
      First : Positive;
      Last : Natural;
   begin
      Trim (Str, First, Last);
      declare
         S : String := Str (First .. Last);
      begin
         if S (First) /= ''' then
            To_Upper (S);
         end if;
         for I in 0 .. Num loop
            declare
               First : constant Natural := Natural (Indexes_2 (I));
               Next : constant Natural := Natural (Indexes_2 (I + 1));
            begin
               if S = Names (First .. Next - 1) then
                  return I;
               end if;
            end;
         end loop;
      end;
      raise Constraint_Error;
   end Value_Enumeration_32;

   procedure Trim (S : String; First : out Positive; Last : out Natural) is
   begin
      First := S'First;
      Last := S'Last;
      while First <= Last and then S (First) = ' ' loop
         First := First + 1;
      end loop;
      while First <= Last and then S (Last) = ' ' loop
         Last := Last - 1;
      end loop;
   end Trim;

   procedure To_Upper (S : in out String) is
   begin
      for I in S'Range loop
         if S (I) in 'a' .. 'z' then
            S (I) := Character'Val (Character'Pos (S (I)) -
               (Character'Pos ('a') - Character'Pos ('A')));
         end if;
      end loop;
   end To_Upper;

end System.Val_Enum;
