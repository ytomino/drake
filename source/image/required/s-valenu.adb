with System.Value_Errors;
package body System.Val_Enum is

   function Value_Enumeration_8 (
      Names : String;
      Indexes : Address;
      Num : Natural;
      Str : String)
      return Natural
   is
      First : Positive;
      Last : Natural;
   begin
      Trim (Str, First, Last);
      if First <= Last then
         declare
            pragma Suppress (Alignment_Check);
            type Index_Type is mod 2 ** 8;
            type Index_Array_Type is array (0 .. Num + 1) of Index_Type;
            Indexes_All : Index_Array_Type;
            for Indexes_All'Address use Indexes;
            S : String := Str (First .. Last);
            Next : Natural := Natural (Indexes_All (0));
         begin
            if S (First) /= ''' then
               To_Upper (S);
            end if;
            for I in 0 .. Num loop
               declare
                  P : constant Positive := Next;
               begin
                  Next := Natural (Indexes_All (I + 1));
                  if S = Names (P .. Next - 1) then
                     return I;
                  end if;
               end;
            end loop;
         end;
      end if;
      Value_Errors.Raise_Discrete_Value_Failure ("Enum", Str);
      declare
         Uninitialized : Natural;
         pragma Unmodified (Uninitialized);
      begin
         return Uninitialized;
      end;
   end Value_Enumeration_8;

   function Value_Enumeration_16 (
      Names : String;
      Indexes : Address;
      Num : Natural;
      Str : String)
      return Natural
   is
      First : Positive;
      Last : Natural;
   begin
      Trim (Str, First, Last);
      if First <= Last then
         declare
            pragma Suppress (Alignment_Check);
            type Index_Type is mod 2 ** 16;
            type Index_Array_Type is array (0 .. Num + 1) of Index_Type;
            Indexes_All : Index_Array_Type;
            for Indexes_All'Address use Indexes;
            S : String := Str (First .. Last);
            Next : Natural := Natural (Indexes_All (0));
         begin
            if S (First) /= ''' then
               To_Upper (S);
            end if;
            for I in 0 .. Num loop
               declare
                  P : constant Positive := Next;
               begin
                  Next := Natural (Indexes_All (I + 1));
                  if S = Names (P .. Next - 1) then
                     return I;
                  end if;
               end;
            end loop;
         end;
      end if;
      Value_Errors.Raise_Discrete_Value_Failure ("Enum", Str);
      declare
         Uninitialized : Natural;
         pragma Unmodified (Uninitialized);
      begin
         return Uninitialized;
      end;
   end Value_Enumeration_16;

   function Value_Enumeration_32 (
      Names : String;
      Indexes : Address;
      Num : Natural;
      Str : String)
      return Natural
   is
      First : Positive;
      Last : Natural;
   begin
      Trim (Str, First, Last);
      if First <= Last then
         declare
            pragma Suppress (Alignment_Check);
            type Index_Type is mod 2 ** 32;
            type Index_Array_Type is array (0 .. Num + 1) of Index_Type;
            Indexes_All : Index_Array_Type;
            for Indexes_All'Address use Indexes;
            S : String := Str (First .. Last);
            Next : Natural := Natural (Indexes_All (0));
         begin
            if S (First) /= ''' then
               To_Upper (S);
            end if;
            for I in 0 .. Num loop
               declare
                  P : constant Positive := Next;
               begin
                  Next := Natural (Indexes_All (I + 1));
                  if S = Names (P .. Next - 1) then
                     return I;
                  end if;
               end;
            end loop;
         end;
      end if;
      Value_Errors.Raise_Discrete_Value_Failure ("Enum", Str);
      declare
         Uninitialized : Natural;
         pragma Unmodified (Uninitialized);
      begin
         return Uninitialized;
      end;
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
            S (I) := Character'Val (
               Character'Pos (S (I))
                  - (Character'Pos ('a') - Character'Pos ('A')));
         end if;
      end loop;
   end To_Upper;

end System.Val_Enum;
