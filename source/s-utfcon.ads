pragma License (Unrestricted);
--  implementation unit
package System.UTF_Conversions is
   pragma Pure;

   --  UCS-4 defined 31 bit.
   type UCS_4 is mod 16#80000000#;

   UTF_8_Max_Length : constant := 6;

   type From_Status_Type is
      (Success, Illegal_Sequence, Non_Shortest, Truncated);
   pragma Discard_Names (From_Status_Type);

   subtype Sequence_Status_Type is
      From_Status_Type range Success .. Illegal_Sequence;

   type To_Status_Type is (Success, Overflow, Unmappable);
   pragma Discard_Names (To_Status_Type);

   procedure To_UTF_8 (
      Code : UCS_4;
      Result : out String;
      Last : out Natural;
      Status : out To_Status_Type);
   procedure From_UTF_8 (
      Data : String;
      Last : out Natural;
      Result : out UCS_4;
      Status : out From_Status_Type);
   procedure From_UTF_8_Reverse (
      Data : String;
      First : out Positive;
      Result : out UCS_4;
      Status : out From_Status_Type);
   procedure UTF_8_Sequence (
      Leading : Character;
      Result : out Positive;
      Status : out Sequence_Status_Type);

   UTF_16_Max_Length : constant := 2;

   procedure To_UTF_16 (
      Code : UCS_4;
      Result : out Wide_String;
      Last : out Natural;
      Status : out To_Status_Type);
   procedure From_UTF_16 (
      Data : Wide_String;
      Last : out Natural;
      Result : out UCS_4;
      Status : out From_Status_Type);
   procedure From_UTF_16_Reverse (
      Data : Wide_String;
      First : out Positive;
      Result : out UCS_4;
      Status : out From_Status_Type);
   procedure UTF_16_Sequence (
      Leading : Wide_Character;
      Result : out Positive;
      Status : out Sequence_Status_Type);

   procedure To_UTF_32 (
      Code : UCS_4;
      Result : out Wide_Wide_String;
      Last : out Natural;
      Status : out To_Status_Type);
   procedure From_UTF_32 (
      Data : Wide_Wide_String;
      Last : out Natural;
      Result : out UCS_4;
      Status : out From_Status_Type);
   procedure From_UTF_32_Reverse (
      Data : Wide_Wide_String;
      First : out Positive;
      Result : out UCS_4;
      Status : out From_Status_Type);
   procedure UTF_32_Sequence (
      Leading : Wide_Wide_Character;
      Result : out Positive;
      Status : out Sequence_Status_Type);

   generic
      type Source_Element_Type is (<>);
      type Source_Type is array (Positive range <>) of Source_Element_Type;
      type Target_Element_Type is (<>);
      type Target_Type is array (Positive range <>) of Target_Element_Type;
      with procedure From_UTF (
         Data : Source_Type;
         Last : out Natural;
         Result : out UCS_4;
         Status : out From_Status_Type);
      with procedure To_UTF (
         Code : UCS_4;
         Result : out Target_Type;
         Last : out Natural;
         Status : out To_Status_Type);
   procedure Convert_Procedure (
      Source : Source_Type;
      Result : out Target_Type;
      Last : out Natural;
      Substitute : Target_Type :=
         (1 => Target_Element_Type'Val (Character'Pos ('?'))));

   generic
      type Source_Element_Type is (<>);
      type Source_Type is array (Positive range <>) of Source_Element_Type;
      type Target_Element_Type is (<>);
      type Target_Type is array (Positive range <>) of Target_Element_Type;
      Expanding : Positive;
      with procedure Convert_Procedure (
         Source : Source_Type;
         Result : out Target_Type;
         Last : out Natural;
         Substitute : Target_Type);
   function Convert_Function (
      Source : Source_Type;
      Substitute : Target_Type :=
         (1 => Target_Element_Type'Val (Character'Pos ('?'))))
      return Target_Type;

   --  the rates of expansion
   --  16#ef# 16#bf# 16#bf#        : 16#ffff#          : 16#0000ffff#
   --  16#f0# 16#90# 16#80# 16#80# : 16#d800# 16#dc00# : 16#00010000#
   --  16#f4# 16#8f# 16#bf# 16#bf# : 16#dbff# 16#dfff# : 16#0010ffff#

   Expanding_From_8_To_16 : constant := 1;
   Expanding_From_8_To_32 : constant := 1;
   Expanding_From_16_To_8 : constant := 3;
   Expanding_From_16_To_32 : constant := 1;
   Expanding_From_32_To_8 : constant := 6;
   Expanding_From_32_To_16 : constant := 2;

end System.UTF_Conversions;
