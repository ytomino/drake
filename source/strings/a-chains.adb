with System.UTF_Conversions;
package body Ada.Characters.Inside is
   use type System.UTF_Conversions.UCS_4;

   function To_Mapping (
      From, To : Character_Sequence;
      Initial_Reference_Count : Interfaces.Integer_32 := -1)
      return Character_Mapping is
   begin
      if From'Length /= To'Length then
         raise Constraint_Error;
      else
         return Result : aliased Character_Mapping := (
            Length => From'Length,
            Reference_Count => Initial_Reference_Count,
            From => From,
            To => To)
         do
            Sort (Result'Access);
         end return;
      end if;
   end To_Mapping;

   function Value (
      Map : not null access constant Character_Mapping;
      Element : Character_Type)
      return Character_Type
   is
      function Search (
         A : Character_Sequence;
         E : Character_Type)
         return Integer;
      function Search (
         A : Character_Sequence;
         E : Character_Type)
         return Integer is
      begin
         if A'First > A'Last then
            return -1;
         else
            declare
               Middle : constant Integer := (A'First + A'Last) / 2;
            begin
               if E < A (Middle) then
                  return Search (A (A'First .. Middle - 1), E);
               elsif E > A (Middle) then
                  return Search (A (Middle + 1 .. A'Last), E);
               else
                  return Middle;
               end if;
            end;
         end if;
      end Search;
      Index : constant Integer := Search (Map.From, Element);
   begin
      if Index < 0 then
         return Element;
      else
         return Map.To (Index);
      end if;
   end Value;

   procedure Translate (
      Source : String;
      Mapping : not null access constant Character_Mapping;
      Item : out String;
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
            Error : Boolean; --  ignore
         begin
            --  get single unicode character
            System.UTF_Conversions.From_UTF_8 (
               Source (I .. Source'Last),
               I_Next,
               Code,
               Error);
            I := I_Next + 1;
            --  map it
            Code := Wide_Wide_Character'Pos (
               Value (Mapping, Wide_Wide_Character'Val (Code)));
            --  put it
            System.UTF_Conversions.To_UTF_8 (
               Code,
               Item (J .. Item'Last),
               J_Next,
               Error);
            J := J_Next + 1;
         end;
      end loop;
      Last := J - 1;
   end Translate;

   function Translate (
      Source : String;
      Mapping : not null access constant Character_Mapping)
      return String
   is
      Result : String (
         1 ..
         Source'Length * System.UTF_Conversions.UTF_8_Max_Length);
      Last : Natural;
   begin
      Translate (Source, Mapping, Result, Last);
      return Result (1 .. Last);
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
            Error : Boolean; --  ignore
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

   procedure Sort (Map : not null access Character_Mapping) is
      Offset : constant Integer := Map.To'First - Map.From'First;
      pragma Warnings (Off);
      pragma Compile_Time_Error (Offset /= 0, "always zero");
      pragma Warnings (On);
   begin
      for I in Map.From'First + 1 .. Map.From'Last loop
         for J in reverse Map.From'First .. I - 1 loop
            declare
               Temp_F : Character_Type;
               Temp_T : Character_Type;
               K : constant Positive := J + 1;
            begin
               if Map.From (J) = Map.From (K) then
                  raise Constraint_Error;
               end if;
               exit when Map.From (J) <= Map.From (K);
                  Temp_F := Map.From (J);
                  Temp_T := Map.To (Offset + J);
                  Map.From (J) := Map.From (K);
                  Map.To (Offset + J) := Map.To (Offset + K);
                  Map.From (K) := Temp_F;
                  Map.To (Offset + K) := Temp_T;
            end;
         end loop;
      end loop;
   end Sort;

end Ada.Characters.Inside;
