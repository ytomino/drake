pragma License (Unrestricted);
--  implementation package
package System.UTF_Conversions is
   pragma Pure;

   --  UCS-4 defined 31 bit.
   type UCS_4 is mod 16#80000000#;

   function Shift_Left (Left : UCS_4; Right : Natural) return UCS_4;
   pragma Import (Intrinsic, Shift_Left);

   UTF_8_Max_Length : constant := 6;

   procedure To_UTF_8 (
      Code : UCS_4;
      Result : out String;
      Last : out Natural;
      Error : out Boolean);
   procedure From_UTF_8 (
      Data : String;
      Last : out Natural;
      Result : out UCS_4;
      Error : out Boolean);
   procedure From_UTF_8_Reverse (
      Data : String;
      First : out Positive;
      Result : out UCS_4;
      Error : out Boolean);
   procedure UTF_8_Sequence (
      Leading : Character;
      Result : out Positive;
      Error : out Boolean);

   UTF_16_Max_Length : constant := 2;

   procedure To_UTF_16 (
      Code : UCS_4;
      Result : out Wide_String;
      Last : out Natural;
      Error : out Boolean);
   procedure From_UTF_16 (
      Data : Wide_String;
      Last : out Natural;
      Result : out UCS_4;
      Error : out Boolean);
   procedure From_UTF_16_Reverse (
      Data : Wide_String;
      First : out Positive;
      Result : out UCS_4;
      Error : out Boolean);
   procedure UTF_16_Sequence (
      Leading : Wide_Character;
      Result : out Positive;
      Error : out Boolean);

   procedure To_UTF_32 (
      Code : UCS_4;
      Result : out Wide_Wide_String;
      Last : out Natural;
      Error : out Boolean);
   procedure From_UTF_32 (
      Data : Wide_Wide_String;
      Last : out Natural;
      Result : out UCS_4;
      Error : out Boolean);
   procedure From_UTF_32_Reverse (
      Data : Wide_Wide_String;
      First : out Positive;
      Result : out UCS_4;
      Error : out Boolean);
   procedure UTF_32_Sequence (
      Leading : Wide_Wide_Character;
      Result : out Positive;
      Error : out Boolean);

   procedure UTF_8_To_UTF_16 (
      Data : String;
      Result : out Wide_String;
      Last : out Natural;
      Error : out Boolean);
   procedure UTF_8_To_UTF_32 (
      Data : String;
      Result : out Wide_Wide_String;
      Last : out Natural;
      Error : out Boolean);
   procedure UTF_16_To_UTF_8 (
      Data : Wide_String;
      Result : out String;
      Last : out Natural;
      Error : out Boolean);
   procedure UTF_32_To_UTF_8 (
      Data : Wide_Wide_String;
      Result : out String;
      Last : out Natural;
      Error : out Boolean);

end System.UTF_Conversions;
