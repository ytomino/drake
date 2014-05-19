pragma License (Unrestricted);
--  extended unit
with Ada.IO_Exceptions;
package Ada.Streams.Block_Transmission is
   --  There are effective read/write/input/output operations for stream.
   pragma Pure;

   generic
      type Element_Type is (<>);
      type Index_Type is (<>);
      type Array_Type is array (Index_Type range <>) of Element_Type;
   procedure Read (
      Stream : not null access Root_Stream_Type'Class;
      Item : out Array_Type);

   generic
      type Element_Type is (<>);
      type Index_Type is (<>);
      type Array_Type is array (Index_Type range <>) of Element_Type;
   procedure Write (
      Stream : not null access Root_Stream_Type'Class;
      Item : Array_Type);

   generic
      type Element_Type is (<>);
      type Index_Type is (<>);
      type Array_Type is array (Index_Type range <>) of Element_Type;
      with procedure Read (
         Stream : not null access Root_Stream_Type'Class;
         Item : out Array_Type);
   function Input (
      Stream : not null access Root_Stream_Type'Class)
      return Array_Type;

   generic
      type Element_Type is (<>);
      type Index_Type is (<>);
      type Array_Type is array (Index_Type range <>) of Element_Type;
      with procedure Write (
         Stream : not null access Root_Stream_Type'Class;
         Item : Array_Type);
   procedure Output (
      Stream : not null access Root_Stream_Type'Class;
      Item : Array_Type);

   --  for Stream_Element_Array
   package Stream_Element_Arrays is

      procedure Read (
         Stream : not null access Root_Stream_Type'Class;
         Item : out Stream_Element_Array);

      procedure Write (
         Stream : not null access Root_Stream_Type'Class;
         Item : Stream_Element_Array);

   end Stream_Element_Arrays;

   --  for shorthand
   End_Error : exception
      renames IO_Exceptions.End_Error;
   Data_Error : exception
      renames IO_Exceptions.Data_Error;

end Ada.Streams.Block_Transmission;
