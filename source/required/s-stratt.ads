pragma License (Unrestricted);
--  implementation unit required by compiler
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
   function I_AS (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Thin_Pointer;
   function I_B (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Boolean;
   function I_C (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Character;
   function I_F (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Float;
   function I_I (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Integer;
   function I_LF (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Long_Float;
   function I_LI (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Long_Integer;
   function I_LLF (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Long_Long_Float;
   function I_LLI (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Long_Long_Integer;
   function I_LLU (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Unsigned_Types.Long_Long_Unsigned;
   function I_LU (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Unsigned_Types.Long_Unsigned;
   function I_SF (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Short_Float;
   function I_SI (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Short_Integer;
   function I_SSI (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Short_Short_Integer;
   function I_SSU (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Unsigned_Types.Short_Short_Unsigned;
   function I_SU (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Unsigned_Types.Short_Unsigned;
   function I_U (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Unsigned_Types.Unsigned;
   function I_WC (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Wide_Character;
   function I_WWC (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Wide_Wide_Character;

   pragma Inline (I_AD);
   pragma Inline (I_AS);
   pragma Inline (I_B);
   pragma Inline (I_C);
   pragma Inline (I_F);
   pragma Inline (I_I);
   pragma Inline (I_LF);
   pragma Inline (I_LI);
   pragma Inline (I_LLF);
   pragma Inline (I_LLI);
   pragma Inline (I_LLU);
   pragma Inline (I_LU);
   pragma Inline (I_SF);
   pragma Inline (I_SI);
   pragma Inline (I_SSI);
   pragma Inline (I_SSU);
   pragma Inline (I_SU);
   pragma Inline (I_U);
   pragma Inline (I_WC);
   pragma Inline (I_WWC);

   procedure W_AD (
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : Fat_Pointer);
   procedure W_AS (
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : Thin_Pointer);
   procedure W_B (
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : Boolean);
   procedure W_C (
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : Character);
   procedure W_F (
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : Float);
   procedure W_I (
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : Integer);
   procedure W_LF (
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : Long_Float);
   procedure W_LI (
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : Long_Integer);
   procedure W_LLF (
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : Long_Long_Float);
   procedure W_LLI (
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : Long_Long_Integer);
   procedure W_LLU (
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : Unsigned_Types.Long_Long_Unsigned);
   procedure W_LU (
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : Unsigned_Types.Long_Unsigned);
   procedure W_SF (
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : Short_Float);
   procedure W_SI (
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : Short_Integer);
   procedure W_SSI (
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : Short_Short_Integer);
   procedure W_SSU (
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : Unsigned_Types.Short_Short_Unsigned);
   procedure W_SU (
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : Unsigned_Types.Short_Unsigned);
   procedure W_U (
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : Unsigned_Types.Unsigned);
   procedure W_WC (
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : Wide_Character);
   procedure W_WWC (
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item : Wide_Wide_Character);

   pragma Inline (W_AD);
   pragma Inline (W_AS);
   pragma Inline (W_B);
   pragma Inline (W_C);
   pragma Inline (W_F);
   pragma Inline (W_I);
   pragma Inline (W_LF);
   pragma Inline (W_LI);
   pragma Inline (W_LLF);
   pragma Inline (W_LLI);
   pragma Inline (W_LLU);
   pragma Inline (W_LU);
   pragma Inline (W_SF);
   pragma Inline (W_SI);
   pragma Inline (W_SSI);
   pragma Inline (W_SSU);
   pragma Inline (W_SU);
   pragma Inline (W_U);
   pragma Inline (W_WC);
   pragma Inline (W_WWC);

   --  required for default 'Read/'Write attributes by compiler (s-stratt.ads)
   --  in original libgnat, Block_IO_OK is a function, but constant is ok (?)
   Block_IO_OK : constant Boolean := True;

   --  for shorthand
   End_Error : exception
      renames Ada.IO_Exceptions.End_Error;

end System.Stream_Attributes;
