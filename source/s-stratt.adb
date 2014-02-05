with Ada.Exceptions;
with Ada.Unchecked_Conversion;
package body System.Stream_Attributes is
   pragma Suppress (All_Checks);
   use type Ada.Streams.Stream_Element_Offset;

   type IO_Boolean is new Boolean;
   for IO_Boolean'Size use
      ((Boolean'Stream_Size + Ada.Streams.Stream_Element'Size - 1)
         / Ada.Streams.Stream_Element'Size)
         * Standard'Storage_Unit;

   subtype S_AD is Ada.Streams.Stream_Element_Array (
      1 ..
      Standard'Address_Size * 2 / Ada.Streams.Stream_Element'Size);

   subtype S_AS is Ada.Streams.Stream_Element_Array (
      1 ..
      Standard'Address_Size / Ada.Streams.Stream_Element'Size);

   subtype S_B is Ada.Streams.Stream_Element_Array (
      1 ..
      IO_Boolean'Stream_Size / Ada.Streams.Stream_Element'Size);

   subtype S_C is Ada.Streams.Stream_Element_Array (
      1 ..
      Character'Stream_Size / Ada.Streams.Stream_Element'Size);

   subtype S_F is Ada.Streams.Stream_Element_Array (
      1 ..
      Float'Stream_Size / Ada.Streams.Stream_Element'Size);

   subtype S_I is Ada.Streams.Stream_Element_Array (
      1 ..
      Integer'Stream_Size / Ada.Streams.Stream_Element'Size);

   subtype S_LF is Ada.Streams.Stream_Element_Array (
      1 ..
      Long_Float'Stream_Size / Ada.Streams.Stream_Element'Size);

   subtype S_LI is Ada.Streams.Stream_Element_Array (
      1 ..
      Long_Integer'Stream_Size / Ada.Streams.Stream_Element'Size);

   subtype S_LLF is Ada.Streams.Stream_Element_Array (
      1 ..
      Long_Long_Float'Stream_Size / Ada.Streams.Stream_Element'Size);

   subtype S_LLI is Ada.Streams.Stream_Element_Array (
      1 ..
      Long_Long_Integer'Stream_Size / Ada.Streams.Stream_Element'Size);

   subtype S_LLU is Ada.Streams.Stream_Element_Array (
      1 ..
      Unsigned_Types.Long_Long_Unsigned'Stream_Size
         / Ada.Streams.Stream_Element'Size);

   subtype S_LU is Ada.Streams.Stream_Element_Array (
      1 ..
      Unsigned_Types.Long_Unsigned'Stream_Size
         / Ada.Streams.Stream_Element'Size);

   subtype S_SF is Ada.Streams.Stream_Element_Array (
      1 ..
      Short_Float'Stream_Size / Ada.Streams.Stream_Element'Size);

   subtype S_SI is Ada.Streams.Stream_Element_Array (
      1 ..
      Short_Integer'Stream_Size / Ada.Streams.Stream_Element'Size);

   subtype S_SSI is Ada.Streams.Stream_Element_Array (
      1 ..
      Short_Short_Integer'Stream_Size / Ada.Streams.Stream_Element'Size);

   subtype S_SSU is Ada.Streams.Stream_Element_Array (
      1 ..
      Unsigned_Types.Short_Short_Unsigned'Stream_Size
         / Ada.Streams.Stream_Element'Size);

   subtype S_SU is Ada.Streams.Stream_Element_Array (
      1 ..
      Unsigned_Types.Short_Unsigned'Stream_Size
         / Ada.Streams.Stream_Element'Size);

   subtype S_U is Ada.Streams.Stream_Element_Array (
      1 ..
      Unsigned_Types.Unsigned'Stream_Size / Ada.Streams.Stream_Element'Size);

   subtype S_WC is Ada.Streams.Stream_Element_Array (
      1 ..
      Wide_Character'Stream_Size / Ada.Streams.Stream_Element'Size);

   subtype S_WWC is Ada.Streams.Stream_Element_Array (
      1 ..
      Wide_Wide_Character'Stream_Size / Ada.Streams.Stream_Element'Size);

   procedure Read_Just (
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : out Ada.Streams.Stream_Element_Array;
      Line : Natural := Ada.Debug.Line);
   procedure Read_Just (
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : out Ada.Streams.Stream_Element_Array;
      Line : Natural := Ada.Debug.Line)
   is
      Last : Ada.Streams.Stream_Element_Offset;
   begin
      Ada.Streams.Read (Stream.all, Item, Last);
      if Last < Item'Last then
         Ada.Exceptions.Raise_Exception_From_Here (
            End_Error'Identity,
            Line => Line);
      end if;
   end Read_Just;

   --  implementation

   function I_AD (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Fat_Pointer is
      function Cast is new Ada.Unchecked_Conversion (S_AD, Fat_Pointer);
      Buffer : S_AD;
   begin
      Read_Just (Stream, Buffer);
      return Cast (Buffer);
   end I_AD;

   function I_AS (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Thin_Pointer
   is
      function Cast is new Ada.Unchecked_Conversion (S_AS, Thin_Pointer);
      Buffer : S_AS;
   begin
      Read_Just (Stream, Buffer);
      return Cast (Buffer);
   end I_AS;

   function I_B (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Boolean
   is
      function Cast is new Ada.Unchecked_Conversion (S_B, IO_Boolean);
      Buffer : S_B;
   begin
      Read_Just (Stream, Buffer);
      return Boolean (Cast (Buffer));
   end I_B;

   function I_C (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Character
   is
      function Cast is new Ada.Unchecked_Conversion (S_C, Character);
      Buffer : S_C;
   begin
      Read_Just (Stream, Buffer);
      return Cast (Buffer);
   end I_C;

   function I_F (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Float
   is
      function Cast is new Ada.Unchecked_Conversion (S_F, Float);
      Buffer : S_F;
   begin
      Read_Just (Stream, Buffer);
      return Cast (Buffer);
   end I_F;

   function I_I (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Integer
   is
      function Cast is new Ada.Unchecked_Conversion (S_I, Integer);
      Buffer : S_I;
   begin
      Read_Just (Stream, Buffer);
      return Cast (Buffer);
   end I_I;

   function I_LF (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Long_Float
   is
      function Cast is new Ada.Unchecked_Conversion (S_LF, Long_Float);
      Buffer : S_LF;
   begin
      Read_Just (Stream, Buffer);
      return Cast (Buffer);
   end I_LF;

   function I_LI (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Long_Integer
   is
      function Cast is new Ada.Unchecked_Conversion (S_LI, Long_Integer);
      Buffer : S_LI;
   begin
      Read_Just (Stream, Buffer);
      return Cast (Buffer);
   end I_LI;

   function I_LLF (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Long_Long_Float
   is
      function Cast is new Ada.Unchecked_Conversion (S_LLF, Long_Long_Float);
      Buffer : S_LLF;
   begin
      Read_Just (Stream, Buffer);
      return Cast (Buffer);
   end I_LLF;

   function I_LLI (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Long_Long_Integer
   is
      function Cast is new Ada.Unchecked_Conversion (S_LLI, Long_Long_Integer);
      Buffer : S_LLI;
   begin
      Read_Just (Stream, Buffer);
      return Cast (Buffer);
   end I_LLI;

   function I_LLU (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Unsigned_Types.Long_Long_Unsigned
   is
      function Cast is
         new Ada.Unchecked_Conversion (
            S_LLU,
            Unsigned_Types.Long_Long_Unsigned);
      Buffer : S_LLU;
   begin
      Read_Just (Stream, Buffer);
      return Cast (Buffer);
   end I_LLU;

   function I_LU (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Unsigned_Types.Long_Unsigned
   is
      function Cast is
         new Ada.Unchecked_Conversion (S_LU, Unsigned_Types.Long_Unsigned);
      Buffer : S_LU;
   begin
      Read_Just (Stream, Buffer);
      return Cast (Buffer);
   end I_LU;

   function I_SF (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Short_Float
   is
      function Cast is new Ada.Unchecked_Conversion (S_SF, Short_Float);
      Buffer : S_SF;
   begin
      Read_Just (Stream, Buffer);
      return Cast (Buffer);
   end I_SF;

   function I_SI (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Short_Integer
   is
      function Cast is new Ada.Unchecked_Conversion (S_SI, Short_Integer);
      Buffer : S_SI;
   begin
      Read_Just (Stream, Buffer);
      return Cast (Buffer);
   end I_SI;

   function I_SSI (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Short_Short_Integer
   is
      function Cast is
         new Ada.Unchecked_Conversion (S_SSI, Short_Short_Integer);
      Buffer : S_SSI;
   begin
      Read_Just (Stream, Buffer);
      return Cast (Buffer);
   end I_SSI;

   function I_SSU (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Unsigned_Types.Short_Short_Unsigned
   is
      function Cast is
         new Ada.Unchecked_Conversion (
            S_SSU,
            Unsigned_Types.Short_Short_Unsigned);
      Buffer : S_SSU;
   begin
      Read_Just (Stream, Buffer);
      return Cast (Buffer);
   end I_SSU;

   function I_SU (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Unsigned_Types.Short_Unsigned
   is
      function Cast is
         new Ada.Unchecked_Conversion (S_SU, Unsigned_Types.Short_Unsigned);
      Buffer : S_SU;
   begin
      Read_Just (Stream, Buffer);
      return Cast (Buffer);
   end I_SU;

   function I_U (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Unsigned_Types.Unsigned
   is
      function Cast is
         new Ada.Unchecked_Conversion (S_U, Unsigned_Types.Unsigned);
      Buffer : S_U;
   begin
      Read_Just (Stream, Buffer);
      return Cast (Buffer);
   end I_U;

   function I_WC (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Wide_Character
   is
      function Cast is new Ada.Unchecked_Conversion (S_WC, Wide_Character);
      Buffer : S_WC;
   begin
      Read_Just (Stream, Buffer);
      return Cast (Buffer);
   end I_WC;

   function I_WWC (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Wide_Wide_Character
   is
      function Cast is
         new Ada.Unchecked_Conversion (S_WWC, Wide_Wide_Character);
      Buffer : S_WWC;
   begin
      Read_Just (Stream, Buffer);
      return Cast (Buffer);
   end I_WWC;

   procedure W_AD (
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : Fat_Pointer)
   is
      function Cast is new Ada.Unchecked_Conversion (Fat_Pointer, S_AD);
   begin
      Ada.Streams.Write (Stream.all, Cast (Item));
   end W_AD;

   procedure W_AS (
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : Thin_Pointer)
   is
      function Cast is new Ada.Unchecked_Conversion (Thin_Pointer, S_AS);
   begin
      Ada.Streams.Write (Stream.all, Cast (Item));
   end W_AS;

   procedure W_B (
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : Boolean)
   is
      function Cast is new Ada.Unchecked_Conversion (IO_Boolean, S_B);
   begin
      Ada.Streams.Write (Stream.all, Cast (IO_Boolean (Item)));
   end W_B;

   procedure W_C (
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : Character)
   is
      function Cast is new Ada.Unchecked_Conversion (Character, S_B);
   begin
      Ada.Streams.Write (Stream.all, Cast (Item));
   end W_C;

   procedure W_F (
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : Float)
   is
      function Cast is new Ada.Unchecked_Conversion (Float, S_F);
   begin
      Ada.Streams.Write (Stream.all, Cast (Item));
   end W_F;

   procedure W_I (
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : Integer)
   is
      function Cast is new Ada.Unchecked_Conversion (Integer, S_I);
   begin
      Ada.Streams.Write (Stream.all, Cast (Item));
   end W_I;

   procedure W_LF (
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : Long_Float)
   is
      function Cast is new Ada.Unchecked_Conversion (Long_Float, S_LF);
   begin
      Ada.Streams.Write (Stream.all, Cast (Item));
   end W_LF;

   procedure W_LI (
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : Long_Integer)
   is
      function Cast is new Ada.Unchecked_Conversion (Long_Integer, S_LI);
   begin
      Ada.Streams.Write (Stream.all, Cast (Item));
   end W_LI;

   procedure W_LLF (
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : Long_Long_Float)
   is
      function Cast is new Ada.Unchecked_Conversion (Long_Long_Float, S_LLF);
   begin
      Ada.Streams.Write (Stream.all, Cast (Item));
   end W_LLF;

   procedure W_LLI (
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : Long_Long_Integer)
   is
      function Cast is new Ada.Unchecked_Conversion (Long_Long_Integer, S_LLI);
   begin
      Ada.Streams.Write (Stream.all, Cast (Item));
   end W_LLI;

   procedure W_LLU (
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : Unsigned_Types.Long_Long_Unsigned)
   is
      function Cast is
         new Ada.Unchecked_Conversion (
            Unsigned_Types.Long_Long_Unsigned,
            S_LLU);
   begin
      Ada.Streams.Write (Stream.all, Cast (Item));
   end W_LLU;

   procedure W_LU (
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : Unsigned_Types.Long_Unsigned)
   is
      function Cast is
         new Ada.Unchecked_Conversion (Unsigned_Types.Long_Unsigned, S_LU);
   begin
      Ada.Streams.Write (Stream.all, Cast (Item));
   end W_LU;

   procedure W_SF (
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : Short_Float)
   is
      function Cast is new Ada.Unchecked_Conversion (Short_Float, S_SF);
   begin
      Ada.Streams.Write (Stream.all, Cast (Item));
   end W_SF;

   procedure W_SI (
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : Short_Integer)
   is
      function Cast is new Ada.Unchecked_Conversion (Short_Integer, S_SI);
   begin
      Ada.Streams.Write (Stream.all, Cast (Item));
   end W_SI;

   procedure W_SSI (
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : Short_Short_Integer)
   is
      function Cast is
         new Ada.Unchecked_Conversion (Short_Short_Integer, S_SSI);
   begin
      Ada.Streams.Write (Stream.all, Cast (Item));
   end W_SSI;

   procedure W_SSU (
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : Unsigned_Types.Short_Short_Unsigned)
   is
      function Cast is
         new Ada.Unchecked_Conversion (
            Unsigned_Types.Short_Short_Unsigned,
            S_SSU);
   begin
      Ada.Streams.Write (Stream.all, Cast (Item));
   end W_SSU;

   procedure W_SU (
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : Unsigned_Types.Short_Unsigned)
   is
      function Cast is
         new Ada.Unchecked_Conversion (Unsigned_Types.Short_Unsigned, S_SU);
   begin
      Ada.Streams.Write (Stream.all, Cast (Item));
   end W_SU;

   procedure W_U (
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : Unsigned_Types.Unsigned)
   is
      function Cast is
         new Ada.Unchecked_Conversion (Unsigned_Types.Unsigned, S_U);
   begin
      Ada.Streams.Write (Stream.all, Cast (Item));
   end W_U;

   procedure W_WC (
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : Wide_Character)
   is
      function Cast is new Ada.Unchecked_Conversion (Wide_Character, S_WC);
   begin
      Ada.Streams.Write (Stream.all, Cast (Item));
   end W_WC;

   procedure W_WWC (
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : Wide_Wide_Character)
   is
      function Cast is
         new Ada.Unchecked_Conversion (Wide_Wide_Character, S_WWC);
   begin
      Ada.Streams.Write (Stream.all, Cast (Item));
   end W_WWC;

end System.Stream_Attributes;
