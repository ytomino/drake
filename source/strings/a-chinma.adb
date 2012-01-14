with Ada.Strings;
with System.UTF_Conversions;
package body Ada.Characters.Inside.Maps is
   pragma Suppress (All_Checks);
   use type System.UTF_Conversions.UCS_4;

   function To_Mapping (
      From, To : Character_Sequence;
      Initial_Reference_Count : System.Reference_Counting.Counter)
      return Character_Mapping
   is
      From_Length : constant Natural := From'Length;
   begin
      if From_Length /= To'Length then
         raise Strings.Translation_Error;
      else
         declare
            Sorted_From : Character_Sequence (1 .. From_Length) := From;
            Sorted_To : Character_Sequence (1 .. From_Length) := To;
            Last : Natural;
         begin
            Sort (Sorted_From, Sorted_To, Last);
            return Character_Mapping'(
               Length => Last,
               Reference_Count => Initial_Reference_Count,
               From => Sorted_From (1 .. Last),
               To => Sorted_To (1 .. Last));
         end;
      end if;
   end To_Mapping;

   function Value (
      Map : not null access constant Character_Mapping;
      Element : Character_Type)
      return Character_Type
   is
      L : Positive := Map.From'First;
      H : Natural := Map.From'Last;
   begin
      loop
         exit when L > H;
         declare
            M : constant Positive := (L + H) / 2;
         begin
            if Element < Map.From (M) then
               H := M - 1;
            elsif Element > Map.From (M) then
               L := M + 1;
            else
               return Map.To (M);
            end if;
         end;
      end loop;
      return Element;
   end Value;

   function Value (
      Map : not null access constant Character_Mapping;
      Element : Character)
      return Character is
   begin
      return To_Character (Value (Map, To_Wide_Wide_Character (Element)));
   end Value;

   procedure Translate (
      Source : String;
      Mapping : not null access constant Character_Mapping;
      Item : out String; -- Source'Length * 6, at least
      Last : out Natural)
   is
      I : Natural := Source'First;
      J : Natural := Item'First;
   begin
      while I <= Source'Last loop
         declare
            Code : System.UTF_Conversions.UCS_4;
            I_Next : Natural;
            J_Next : Natural;
            Error : Boolean; -- ignore
         begin
            --  get single unicode character
            System.UTF_Conversions.From_UTF_8 (
               Source (I .. Source'Last),
               I_Next,
               Code,
               Error);
            --  map it
            Code := Wide_Wide_Character'Pos (
               Value (Mapping, Wide_Wide_Character'Val (Code)));
            --  put it
            System.UTF_Conversions.To_UTF_8 (
               Code,
               Item (J .. Item'Last),
               J_Next,
               Error);
            --  forwarding
            I := I_Next + 1;
            J := J_Next + 1;
         end;
      end loop;
      Last := J - 1;
   end Translate;

   function Compare (
      Left : String;
      Right : String;
      Mapping : not null access constant Character_Mapping)
      return Integer
   is
      I : Natural := Left'First;
      J : Natural := Right'First;
   begin
      while I <= Left'Last and then J <= Right'Last loop
         declare
            I_Code : System.UTF_Conversions.UCS_4;
            I_Next : Natural;
            J_Code : System.UTF_Conversions.UCS_4;
            J_Next : Natural;
            Error : Boolean; -- ignore
         begin
            System.UTF_Conversions.From_UTF_8 (
               Left (I .. Left'Last),
               I_Next,
               I_Code,
               Error);
            I := I_Next + 1;
            I_Code := Wide_Wide_Character'Pos (
               Value (Mapping, Wide_Wide_Character'Val (I_Code)));
            System.UTF_Conversions.From_UTF_8 (
               Right (J .. Right'Last),
               J_Next,
               J_Code,
               Error);
            J := J_Next + 1;
            J_Code := Wide_Wide_Character'Pos (
               Value (Mapping, Wide_Wide_Character'Val (J_Code)));
            if I_Code < J_Code then
               return -1;
            elsif I_Code > J_Code then
               return 1;
            end if;
         end;
      end loop;
      return Boolean'Pos (I <= Left'Last) - Boolean'Pos (J <= Right'Last);
   end Compare;

   procedure Sort (From, To : in out Character_Sequence) is
      pragma Assert (From'First = To'First);
   begin
      for I in From'First + 1 .. From'Last loop
         for J in reverse From'First .. I - 1 loop
            declare
               Temp_F : Character_Type;
               Temp_T : Character_Type;
               K : constant Positive := J + 1;
            begin
               if From (J) = From (K) then
                  raise Strings.Translation_Error;
               end if;
               exit when From (J) <= From (K);
               Temp_F := From (J);
               Temp_T := To (J);
               From (J) := From (K);
               To (J) := To (K);
               From (K) := Temp_F;
               To (K) := Temp_T;
            end;
         end loop;
      end loop;
   end Sort;

   procedure Sort (From, To : in out Character_Sequence; Last : out Natural) is
      pragma Assert (From'First = To'First);
   begin
      Last := From'Last;
      for I in reverse From'Range loop
         if From (I) = To (I) then
            From (I) := From (Last);
            To (I) := To (Last);
            Last := Last - 1;
         end if;
      end loop;
      Sort (From (From'First .. Last), To (To'First .. Last));
   end Sort;

end Ada.Characters.Inside.Maps;
