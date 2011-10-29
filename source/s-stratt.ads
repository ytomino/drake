pragma License (Unrestricted);
--  implementation package required by compiler
with Ada.IO_Exceptions;
with Ada.Streams;
with System.Unsigned_Types;
package System.Stream_Attributes is
   pragma Preelaborate;

   --  required for 'Read/'Write attributes by compiler (s-stratt.ads)

   type Thin_Pointer is record
      P1 : Address;
   end record;
   pragma Suppress_Initialization (Thin_Pointer);

   type Fat_Pointer is record
      P1 : Address;
      P2 : Address;
   end record;
   pragma Suppress_Initialization (Fat_Pointer);

   function I_AD (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Fat_Pointer;
   pragma Inline (I_AD);

   function I_AS (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Thin_Pointer;
   pragma Inline (I_AS);

   function I_B (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Boolean;
   pragma Inline (I_B);

   function I_C (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Character;
   pragma Inline (I_C);

   function I_F (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Float;
   pragma Inline (I_F);

   function I_I (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Integer;
   pragma Inline (I_I);

   function I_LF (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Long_Float;
   pragma Inline (I_LF);

   function I_LI (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Long_Integer;
   pragma Inline (I_LI);

   function I_LLF (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Long_Long_Float;
   pragma Inline (I_LLF);

   function I_LLI (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Long_Long_Integer;
   pragma Inline (I_LLI);

   function I_LLU (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Unsigned_Types.Long_Long_Unsigned;
   pragma Inline (I_LLU);

   function I_LU (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Unsigned_Types.Long_Unsigned;
   pragma Inline (I_LU);

   function I_SF (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Short_Float;
   pragma Inline (I_SF);

   function I_SI (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Short_Integer;
   pragma Inline (I_SI);

   function I_SSI (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Short_Short_Integer;
   pragma Inline (I_SSI);

   function I_SSU (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
     return Unsigned_Types.Short_Short_Unsigned;
   pragma Inline (I_SSU);

   function I_SU (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Unsigned_Types.Short_Unsigned;
   pragma Inline (I_SU);

   function I_U (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Unsigned_Types.Unsigned;
   pragma Inline (I_U);

   function I_WC (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Wide_Character;
   pragma Inline (I_WC);

   function I_WWC (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Wide_Wide_Character;
   pragma Inline (I_WWC);

   procedure W_AD (
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : Fat_Pointer);
   pragma Inline (W_AD);

   procedure W_AS (
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : Thin_Pointer);
   pragma Inline (W_AS);

   procedure W_B (
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : Boolean);
   pragma Inline (W_B);

   procedure W_C (
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : Character);
   pragma Inline (W_C);

   procedure W_F (
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : Float);
   pragma Inline (W_F);

   procedure W_I (
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : Integer);
   pragma Inline (W_I);

   procedure W_LF (
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : Long_Float);
   pragma Inline (W_LF);

   procedure W_LI (
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : Long_Integer);
   pragma Inline (W_LI);

   procedure W_LLF (
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : Long_Long_Float);
   pragma Inline (W_LLF);

   procedure W_LLI (
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : Long_Long_Integer);
   pragma Inline (W_LLI);

   procedure W_LLU (
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : Unsigned_Types.Long_Long_Unsigned);
   pragma Inline (W_LLU);

   procedure W_LU (
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : Unsigned_Types.Long_Unsigned);
   pragma Inline (W_LU);

   procedure W_SF (
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : Short_Float);
   pragma Inline (W_SF);

   procedure W_SI (
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : Short_Integer);
   pragma Inline (W_SI);

   procedure W_SSI (
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : Short_Short_Integer);
   pragma Inline (W_SSI);

   procedure W_SSU (
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : Unsigned_Types.Short_Short_Unsigned);
   pragma Inline (W_SSU);

   procedure W_SU (
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : Unsigned_Types.Short_Unsigned);
   pragma Inline (W_SU);

   procedure W_U (
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : Unsigned_Types.Unsigned);
   pragma Inline (W_U);

   procedure W_WC (
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : Wide_Character);
   pragma Inline (W_WC);

   procedure W_WWC (
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : Wide_Wide_Character);
   pragma Inline (W_WWC);

   --  required for default 'Read/'Write attributes by compiler (s-stratt.ads)
   --  in original libgnat, Block_IO_OK is a function, but constant is ok (?)
   Block_IO_OK : constant Boolean := True;

   --  for shorthand
   End_Error : exception
      renames Ada.IO_Exceptions.End_Error;

end System.Stream_Attributes;
