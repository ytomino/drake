with System.UTF_Conversions;
package body Ada.Strings.Maps is

   --  functions in Ada.Characters.Conversions uses substitute
   --  instead of raising exception,
   --  functions in this package do not have substitute parameter.

   function To_Character (Item : Wide_Wide_Character)
      return Character;
   function To_Character (Item : Wide_Wide_Character)
      return Character is
   begin
      if Wide_Wide_Character'Pos (Item) >= 16#80# then
         raise Constraint_Error;
      else
         return Character'Val (Wide_Wide_Character'Pos (Item));
      end if;
   end To_Character;

   function To_Wide_Wide_Character (Item : Character)
      return Wide_Wide_Character;
   function To_Wide_Wide_Character (Item : Character)
      return Wide_Wide_Character is
   begin
      if Character'Pos (Item) >= 16#80# then
         raise Constraint_Error;
      else
         return Wide_Wide_Character'Val (Character'Pos (Item));
      end if;
   end To_Wide_Wide_Character;

   function Identity return Character_Mapping is
   begin
      return Character_Mapping (Wide_Wide_Maps.Identity);
   end Identity;

   function Is_In (Element : Character; Set : Character_Set)
      return Boolean is
   begin
      return Wide_Wide_Maps.Is_In (
         To_Wide_Wide_Character (Element),
         Wide_Wide_Maps.Wide_Wide_Character_Set (Set));
   end Is_In;

   function Is_Subset (Elements : Character_Set; Set : Character_Set)
      return Boolean is
   begin
      return Wide_Wide_Maps.Is_Subset (
         Wide_Wide_Maps.Wide_Wide_Character_Set (Elements),
         Wide_Wide_Maps.Wide_Wide_Character_Set (Set));
   end Is_Subset;

   function Null_Set return Character_Set is
   begin
      return Character_Set (Wide_Wide_Maps.Null_Set);
   end Null_Set;

   function To_Domain (Map : Character_Mapping)
      return Character_Sequence
   is
      W : constant Wide_Wide_String := Wide_Wide_Maps.To_Domain (
         Wide_Wide_Maps.Wide_Wide_Character_Mapping (Map));
      S : String (1 .. W'Length * System.UTF_Conversions.UTF_8_Max_Length);
      Last : Natural;
      Error : Boolean; --  ignore
   begin
      System.UTF_Conversions.UTF_32_To_UTF_8 (W, S, Last, Error);
      return S (1 .. Last);
   end To_Domain;

   function To_Mapping (From, To : Character_Sequence)
      return Character_Mapping
   is
      WF : Wide_Wide_String (1 .. From'Length);
      WF_Last : Natural;
      WT : Wide_Wide_String (1 .. To'Length);
      WT_Last : Natural;
      Error : Boolean; --  ignore
   begin
      System.UTF_Conversions.UTF_8_To_UTF_32 (From, WF, WF_Last, Error);
      System.UTF_Conversions.UTF_8_To_UTF_32 (To, WT, WT_Last, Error);
      return Character_Mapping (Wide_Wide_Maps.To_Mapping (
         WF (1 .. WF_Last),
         WT (1 .. WT_Last)));
   end To_Mapping;

   function To_Sequence (Set : Character_Set) return Character_Sequence is
      W : constant Wide_Wide_String := Wide_Wide_Maps.To_Sequence (
         Wide_Wide_Maps.Wide_Wide_Character_Set (Set));
      S : String (1 .. W'Length * System.UTF_Conversions.UTF_8_Max_Length);
      Last : Natural;
      Error : Boolean; --  ignore
   begin
      System.UTF_Conversions.UTF_32_To_UTF_8 (W, S, Last, Error);
      return S (1 .. Last);
   end To_Sequence;

   function To_Set (Ranges : Character_Ranges) return Character_Set is
      WR : Wide_Wide_Maps.Wide_Wide_Character_Ranges (Ranges'Range);
   begin
      for I in Ranges'Range loop
         WR (I).Low := To_Wide_Wide_Character (Ranges (I).Low);
         WR (I).High := To_Wide_Wide_Character (Ranges (I).High);
      end loop;
      return Character_Set (Wide_Wide_Maps.To_Set (WR));
   end To_Set;

   function To_Set (Span : Character_Range) return Character_Set is
   begin
      return Character_Set (Wide_Wide_Maps.To_Set (
         Wide_Wide_Maps.Wide_Wide_Character_Range'(
            Low => To_Wide_Wide_Character (Span.Low),
            High => To_Wide_Wide_Character (Span.High))));
   end To_Set;

   function To_Set (Sequence : Character_Sequence) return Character_Set is
      WS : Wide_Wide_String (1 .. Sequence'Length);
      WS_Last : Natural;
      Error : Boolean; --  ignore
   begin
      System.UTF_Conversions.UTF_8_To_UTF_32 (Sequence, WS, WS_Last, Error);
      return Character_Set (Wide_Wide_Maps.To_Set (
         WS (1 .. WS_Last)));
   end To_Set;

   function To_Set (Singleton : Character) return Character_Set is
   begin
      return Character_Set (Wide_Wide_Maps.To_Set (
         To_Wide_Wide_Character (Singleton)));
   end To_Set;

   function To_Range (Map : Character_Mapping)
      return Character_Sequence
   is
      W : constant Wide_Wide_String := Wide_Wide_Maps.To_Range (
         Wide_Wide_Maps.Wide_Wide_Character_Mapping (Map));
      S : String (1 .. W'Length * System.UTF_Conversions.UTF_8_Max_Length);
      Last : Natural;
      Error : Boolean; --  ignore
   begin
      System.UTF_Conversions.UTF_32_To_UTF_8 (W, S, Last, Error);
      return S (1 .. Last);
   end To_Range;

   function Value (Map : Character_Mapping; Element : Character)
      return Character is
   begin
      return To_Character (Wide_Wide_Maps.Value (
         Wide_Wide_Maps.Wide_Wide_Character_Mapping (Map),
         To_Wide_Wide_Character (Element)));
   end Value;

   function "=" (Left, Right : Character_Set) return Boolean is
   begin
      return Wide_Wide_Maps."=" (
         Wide_Wide_Maps.Wide_Wide_Character_Set (Left),
         Wide_Wide_Maps.Wide_Wide_Character_Set (Right));
   end "=";

   function "not" (Right : Character_Set) return Character_Set is
   begin
      return Character_Set (Wide_Wide_Maps."not" (
         Wide_Wide_Maps.Wide_Wide_Character_Set (Right)));
   end "not";

   function "and" (Left, Right : Character_Set) return Character_Set is
   begin
      return Character_Set (Wide_Wide_Maps."and" (
         Wide_Wide_Maps.Wide_Wide_Character_Set (Left),
         Wide_Wide_Maps.Wide_Wide_Character_Set (Right)));
   end "and";

   function "or" (Left, Right : Character_Set) return Character_Set is
   begin
      return Character_Set (Wide_Wide_Maps."or" (
         Wide_Wide_Maps.Wide_Wide_Character_Set (Left),
         Wide_Wide_Maps.Wide_Wide_Character_Set (Right)));
   end "or";

   function "xor" (Left, Right : Character_Set) return Character_Set is
   begin
      return Character_Set (Wide_Wide_Maps."xor" (
         Wide_Wide_Maps.Wide_Wide_Character_Set (Left),
         Wide_Wide_Maps.Wide_Wide_Character_Set (Right)));
   end "xor";

   function "-" (Left, Right : Character_Set) return Character_Set is
   begin
      return Character_Set (Wide_Wide_Maps."and" (
         Wide_Wide_Maps.Wide_Wide_Character_Set (Left),
         Wide_Wide_Maps.Wide_Wide_Character_Set (Right)));
   end "-";

end Ada.Strings.Maps;
